use std::io::Write;
use std::io::stderr;
use std::ops::Range;

use clap::Args;
use crossterm::style::{Attribute, Color, ContentStyle, Stylize};
use crossterm::tty::IsTty;
use derive_more::Constructor;

use zicc_intcode::{Instruction, ReadParamMode, WriteParamMode};
use zicc_limits::Value;

use crate::hooks::Hooks;
use crate::mem::Memory;

pub mod colors {
    //! Color palette

    use crossterm::style::Color;

    pub const IP_LABEL: Color = Color::Cyan;
    pub const IP_VALUE: Color = Color::Cyan;
    pub const RB_LABEL: Color = Color::Yellow;
    pub const RB_VALUE: Color = Color::Yellow;
    pub const OPCODE_RAW: Color = Color::Cyan;
    pub const OPCODE_MNEMONIC: Color = Color::Cyan;
    pub const IMMEDIATE: Color = Color::Green;
    pub const RELATIVE: Color = Color::Magenta;
    pub const ABSOLUTE: Color = Color::DarkGrey;
    pub const ELLIPSIS: Color = Color::DarkGrey;
    pub const MEMORY_VALUE: Color = Color::White;
}

#[derive(Debug, Clone, Copy, Constructor)]
pub(super) struct Tui {
    pub colored: bool,
    pub fallback_width: u16,
}

#[derive(Debug, Clone, Args)]
pub struct TuiArgs {
    /// Force color output in the TUI
    #[arg(long, conflicts_with("debug_no_color"))]
    pub debug_color: bool,

    /// Disable color output in the TUI
    #[arg(long, conflicts_with("debug_color"))]
    pub debug_no_color: bool,

    /// Terminal width fallback for the TUI display
    #[arg(long)]
    pub debug_term_size: Option<u16>,
}

impl Default for Tui {
    fn default() -> Self {
        Self {
            colored: stderr().is_tty(),
            fallback_width: 80,
        }
    }
}

impl From<TuiArgs> for Tui {
    fn from(cli: TuiArgs) -> Self {
        let colored = if cli.debug_color {
            true
        } else if cli.debug_no_color {
            false
        } else {
            stderr().is_tty()
        };

        Self {
            colored,
            fallback_width: cli.debug_term_size.unwrap_or(80),
        }
    }
}

// ── MemWindow ──────────────────────────────────────────────────────

struct MemWindow {
    start: usize,
    end: usize,
    cell_width: usize,
    has_left: bool,
    has_right: bool,
}

fn compute_window(
    center: usize,
    content_len: usize,
    prefix_len: usize,
    width: usize,
    content: &[Value],
) -> MemWindow {
    let cell_width: usize = 5;
    let cell_slop: usize = 8;

    let available = width.saturating_sub(prefix_len);
    let cells = if available <= cell_slop {
        0
    } else {
        (available - cell_slop) / cell_width
    };

    if cells == 0 {
        return MemWindow {
            start: 0,
            end: 0,
            cell_width,
            has_left: false,
            has_right: false,
        };
    }

    let half = cells / 2;
    let start = center.saturating_sub(half);
    let end = (start + cells).min(content_len.max(start));

    let has_left = start > 0
        && content[..start.min(content_len)]
            .iter()
            .any(|v| v != &Value::ZERO);
    let has_right = end < content_len
        && content[end..]
            .iter()
            .any(|v| v != &Value::ZERO);

    MemWindow {
        start,
        end,
        cell_width,
        has_left,
        has_right,
    }
}

fn prefix_len(label_val: &str) -> usize {
    4 + label_val.len() + 3 // "{label}:" + val + "   "  (label is 2 chars: IP/RB)
}

// ── Memory line rendering (shared by IP and RB) ────────────────────

struct MemLineColors {
    label: Color,
    value: Color,
    highlight: Color,
    highlight_bold: bool,
    memory: Color,
    ellipsis: Color,
}

const IP_COLORS: MemLineColors = MemLineColors {
    label: colors::IP_LABEL,
    value: colors::IP_VALUE,
    highlight: colors::OPCODE_RAW,
    highlight_bold: true,
    memory: colors::MEMORY_VALUE,
    ellipsis: colors::ELLIPSIS,
};

const RB_COLORS: MemLineColors = MemLineColors {
    label: colors::RB_LABEL,
    value: colors::RB_VALUE,
    highlight: colors::RB_VALUE,
    highlight_bold: true,
    memory: colors::MEMORY_VALUE,
    ellipsis: colors::ELLIPSIS,
};

fn render_memory_line(
    label: &str,
    label_val_str: &str,
    win: &MemWindow,
    content: &[Value],
    highlight_idx: usize,
    c: &MemLineColors,
    width: usize,
) -> (String, Vec<(Range<usize>, Color, bool)>) {
    let mut text = String::new();
    let mut styles = Vec::new();

    // prefix
    styles.push((0..4, c.label, false));
    text.push_str(label);
    text.push_str(": ");
    let ps = text.len();
    text.push_str(label_val_str);
    styles.push((ps..text.len(), c.value, false));
    text.push_str("   ");

    if win.end == win.start || win.start >= win.end {
        // no cells fit, just pad and return
        if text.len() < width {
            text.push_str(&" ".repeat(width - text.len()));
        }
        return (text, styles);
    }

    // left ellipsis
    if win.has_left {
        let es = text.len();
        text.push_str("... ");
        styles.push((es..text.len(), c.ellipsis, false));
    }

    // cells
    for i in win.start..win.end {
        let val = content.get(i).cloned().unwrap_or(Value::ZERO);
        let cell = format!("{:>4} ", val);
        let cs = text.len();
        text.push_str(&cell);
        if i == highlight_idx {
            styles.push((cs..text.len(), c.highlight, c.highlight_bold));
        } else {
            styles.push((cs..text.len(), c.memory, false));
        }
    }

    // right ellipsis
    if win.has_right {
        let es = text.len();
        text.push_str(" ...");
        styles.push((es..text.len(), c.ellipsis, false));
    }

    // pad
    if text.len() < width {
        text.push_str(&" ".repeat(width - text.len()));
    }

    (text, styles)
}

// ── Column helpers ──────────────────────────────────────────────────

/// Column where the cell at the given index starts (beginning of the 5-char cell).
fn value_cell_column(win: &MemWindow, prefix_len: usize, index: usize) -> usize {
    let cell_index = index.saturating_sub(win.start);
    let n_cells = win.end.saturating_sub(win.start);
    if cell_index >= n_cells {
        return prefix_len;
    }
    prefix_len
        + if win.has_left { 4 } else { 0 }
        + cell_index * win.cell_width
}

/// Column where the value *text* starts within its cell (after the leading space for right-alignment).
fn value_text_column(win: &MemWindow, prefix_len: usize, index: usize, val_str: &str) -> Option<usize> {
    let cell_index = index.saturating_sub(win.start);
    let n_cells = win.end.saturating_sub(win.start);
    if cell_index >= n_cells {
        return None;
    }
    let cell_col = value_cell_column(win, prefix_len, index);
    Some(cell_col + (4usize.saturating_sub(val_str.len())))
}

// ── Instruction line (line 2) ──────────────────────────────────────

fn build_instruction_line(
    instruction: &Instruction<usize>,
    content: &[Value],
    indent_col: usize,
    width: usize,
) -> (String, Vec<(Range<usize>, Color, bool)>) {
    let instr_parts = format_instr(instruction, content);
    let indent = " ".repeat(indent_col);

    let mut text = String::new();
    let mut styles = Vec::new();

    // indentation (unstyled)
    text.push_str(&indent);

    // instruction parts
    for (part_text, color, bold) in &instr_parts {
        let ps = text.len();
        text.push_str(part_text);
        styles.push((ps..text.len(), *color, *bold));
    }

    // pad
    if text.len() < width {
        text.push_str(&" ".repeat(width - text.len()));
    }

    (text, styles)
}

// ── Caret line (line 4) ────────────────────────────────────────────

fn render<W: Write>(
    out: &mut W,
    plain: &str,
    styles: &[(Range<usize>, Color, bool)],
    colored: bool,
) {
    if !colored {
        let _ = write!(out, "{}", plain);
        return;
    }

    let mut pos = 0;
    for (range, color, bold) in styles {
        if pos < range.start {
            let _ = write!(out, "{}", &plain[pos..range.start]);
        }
        let mut cs = ContentStyle::new().with(*color);
        if *bold {
            cs = cs.attribute(Attribute::Bold);
        }
        let _ = write!(out, "{}", cs.apply(&plain[range.start..range.end]));
        pos = range.end;
    }
    if pos < plain.len() {
        let _ = write!(out, "{}", &plain[pos..]);
    }
}

fn build_caret_line(
    win: &MemWindow,
    prefix_len: usize,
    highlight_idx: usize,
    highlight_str: &str,
    color: Color,
    width: usize,
) -> (String, Vec<(Range<usize>, Color, bool)>) {
    let col = match value_text_column(win, prefix_len, highlight_idx, highlight_str) {
        Some(c) => c,
        None => 0,
    };

    let caret = "^".repeat(highlight_str.len());
    let indent = " ".repeat(col);

    let mut text = String::new();
    let mut styles = Vec::new();
    text.push_str(&indent);
    let cs = text.len();
    text.push_str(&caret);
    styles.push((cs..text.len(), color, false));

    // pad
    if text.len() < width {
        text.push_str(&" ".repeat(width - text.len()));
    }

    (text, styles)
}

// ── Hooks implementation ───────────────────────────────────────────

impl Hooks for Tui {
    fn before_instruction(
        &mut self,
        instruction: &Instruction<usize>,
        _pos: Range<usize>,
        memory: &Memory,
    ) {
        let width = crossterm::terminal::size()
            .map(|(w, _)| w)
            .unwrap_or(self.fallback_width) as usize;

        let ip = memory.ip();
        let content = memory.content();
        let rb = memory.rb();

        // ── Line 1: IP memory line ──
        let ip_str = format!("{}", ip);
        let ip_prefix = prefix_len(&ip_str);
        let ip_win = compute_window(ip, content.len(), ip_prefix, width, content);
        let (line1, styles1) = render_memory_line("IP", &ip_str, &ip_win, content, ip, &IP_COLORS, width);

        // ── Line 2: instruction aligned to opcode value ──
        let opcode_val = content.get(ip).cloned().unwrap_or(Value::ZERO);
        let opcode_str = format!("{}", opcode_val);
        let instr_col = value_text_column(&ip_win, ip_prefix, ip, &opcode_str).unwrap_or(ip_prefix);
        let (line2, styles2) = build_instruction_line(instruction, content, instr_col, width);

        // ── Line 3: RB memory line centered at address rb ──
        let rb_str = format!("{}", rb);
        let rb_prefix = prefix_len(&rb_str);
        let rb_addr = usize::try_from(rb).unwrap_or(0);
        let rb_win = compute_window(rb_addr, content.len(), rb_prefix, width, content);
        let (line3, styles3) = render_memory_line("RB", &rb_str, &rb_win, content, rb_addr, &RB_COLORS, width);

        // ── Line 4: carets under the value at address rb ──
        let rb_val = content.get(rb_addr).cloned().unwrap_or(Value::ZERO);
        let rb_val_str = format!("{}", rb_val);
        let (line4, styles4) = build_caret_line(&rb_win, rb_prefix, rb_addr, &rb_val_str, colors::RB_VALUE, width);

        // ── Render ──
        let mut out = stderr();
        render(&mut out, &line1, &styles1, self.colored);
        let _ = writeln!(out);
        render(&mut out, &line2, &styles2, self.colored);
        let _ = writeln!(out);
        render(&mut out, &line3, &styles3, self.colored);
        let _ = writeln!(out);
        render(&mut out, &line4, &styles4, self.colored);
        let _ = writeln!(out);
    }
}

fn format_instr(instruction: &Instruction<usize>, content: &[Value]) -> Vec<(String, Color, bool)> {
    use Instruction::*;

    let val = |pos: usize| content.get(pos).cloned().unwrap_or(Value::ZERO);

    match instruction {
        Add((am, ap), (bm, bp), (cm, cp)) => {
            let av = val(*ap);
            let bv = val(*bp);
            let cv = val(*cp);
            vec![
                (format!("ADD"), colors::OPCODE_MNEMONIC, true),
                (format!("  "), colors::ABSOLUTE, false),
                (param_read(*am, &av), color_read(*am), false),
                (format!(" "), colors::ABSOLUTE, false),
                (param_read(*bm, &bv), color_read(*bm), false),
                (format!(" "), colors::ABSOLUTE, false),
                (param_write(*cm, &cv), color_write(*cm), false),
            ]
        }
        Mul((am, ap), (bm, bp), (cm, cp)) => {
            let av = val(*ap);
            let bv = val(*bp);
            let cv = val(*cp);
            vec![
                (format!("MUL"), colors::OPCODE_MNEMONIC, true),
                (format!("  "), colors::ABSOLUTE, false),
                (param_read(*am, &av), color_read(*am), false),
                (format!(" "), colors::ABSOLUTE, false),
                (param_read(*bm, &bv), color_read(*bm), false),
                (format!(" "), colors::ABSOLUTE, false),
                (param_write(*cm, &cv), color_write(*cm), false),
            ]
        }
        Inp((wm, wp)) => {
            let wv = val(*wp);
            vec![
                (format!("INP"), colors::OPCODE_MNEMONIC, true),
                (format!("  "), colors::ABSOLUTE, false),
                (param_write(*wm, &wv), color_write(*wm), false),
            ]
        }
        Out((rm, rp)) => {
            let rv = val(*rp);
            vec![
                (format!("OUT"), colors::OPCODE_MNEMONIC, true),
                (format!("  "), colors::ABSOLUTE, false),
                (param_read(*rm, &rv), color_read(*rm), false),
            ]
        }
        Jnz((am, ap), (bm, bp)) => {
            let av = val(*ap);
            let bv = val(*bp);
            vec![
                (format!("JNZ"), colors::OPCODE_MNEMONIC, true),
                (format!("  "), colors::ABSOLUTE, false),
                (param_read(*am, &av), color_read(*am), false),
                (format!(" "), colors::ABSOLUTE, false),
                (param_read(*bm, &bv), color_read(*bm), false),
            ]
        }
        Jez((am, ap), (bm, bp)) => {
            let av = val(*ap);
            let bv = val(*bp);
            vec![
                (format!("JEZ"), colors::OPCODE_MNEMONIC, true),
                (format!("  "), colors::ABSOLUTE, false),
                (param_read(*am, &av), color_read(*am), false),
                (format!(" "), colors::ABSOLUTE, false),
                (param_read(*bm, &bv), color_read(*bm), false),
            ]
        }
        Slt((am, ap), (bm, bp), (cm, cp)) => {
            let av = val(*ap);
            let bv = val(*bp);
            let cv = val(*cp);
            vec![
                (format!("SLT"), colors::OPCODE_MNEMONIC, true),
                (format!("  "), colors::ABSOLUTE, false),
                (param_read(*am, &av), color_read(*am), false),
                (format!(" "), colors::ABSOLUTE, false),
                (param_read(*bm, &bv), color_read(*bm), false),
                (format!(" "), colors::ABSOLUTE, false),
                (param_write(*cm, &cv), color_write(*cm), false),
            ]
        }
        Seq((am, ap), (bm, bp), (cm, cp)) => {
            let av = val(*ap);
            let bv = val(*bp);
            let cv = val(*cp);
            vec![
                (format!("SEQ"), colors::OPCODE_MNEMONIC, true),
                (format!("  "), colors::ABSOLUTE, false),
                (param_read(*am, &av), color_read(*am), false),
                (format!(" "), colors::ABSOLUTE, false),
                (param_read(*bm, &bv), color_read(*bm), false),
                (format!(" "), colors::ABSOLUTE, false),
                (param_write(*cm, &cv), color_write(*cm), false),
            ]
        }
        Inb((rm, rp)) => {
            let rv = val(*rp);
            vec![
                (format!("INB"), colors::OPCODE_MNEMONIC, true),
                (format!("  "), colors::ABSOLUTE, false),
                (param_read(*rm, &rv), color_read(*rm), false),
            ]
        }
        Hlt => vec![(format!("HLT"), colors::OPCODE_MNEMONIC, true)],
    }
}

fn param_read(mode: ReadParamMode, val: &Value) -> String {
    match mode {
        ReadParamMode::Absolute => format!("{}", val),
        ReadParamMode::Immediate => format!("#{}", val),
        ReadParamMode::Relative => format!("@{}", val),
    }
}

fn param_write(mode: WriteParamMode, val: &Value) -> String {
    match mode {
        WriteParamMode::Absolute => format!("{}", val),
        WriteParamMode::Relative => format!("@{}", val),
    }
}

fn color_read(mode: ReadParamMode) -> Color {
    match mode {
        ReadParamMode::Absolute => colors::ABSOLUTE,
        ReadParamMode::Immediate => colors::IMMEDIATE,
        ReadParamMode::Relative => colors::RELATIVE,
    }
}

fn color_write(mode: WriteParamMode) -> Color {
    match mode {
        WriteParamMode::Absolute => colors::ABSOLUTE,
        WriteParamMode::Relative => colors::RELATIVE,
    }
}
