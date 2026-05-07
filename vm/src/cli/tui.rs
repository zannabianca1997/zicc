use std::borrow::Cow;
use std::io::{self, stderr};
use std::ops::Range;
use std::usize;

use clap::Args;
use crossterm::style::{Attributes, ContentStyle};
use crossterm::tty::IsTty;
use derive_more::Constructor;
use itertools::Itertools;
use zicc_intcode::{Instruction, OpCode, ReadParamMode};
use zicc_limits::Value;

use crate::hooks::Hooks;
use crate::mem::Memory;

pub mod colors {
    //! Color palette

    use crossterm::style::Color;

    pub const IP_LABEL: Color = Color::Cyan;
    pub const IP_VALUE: Color = IP_LABEL;

    pub const RB_LABEL: Color = Color::Yellow;
    pub const RB_VALUE: Color = RB_LABEL;

    pub const OPCODE: Color = Color::Cyan;

    pub const ABSOLUTE: Color = Color::Grey;
    pub const IMMEDIATE: Color = Color::Green;
    pub const RELATIVE: Color = Color::Magenta;

    pub const ELLIPSIS: Color = Color::Grey;

    pub const MEMORY_VALUE: Color = Color::White;
}

#[derive(Debug, Clone, Copy, Constructor)]
pub(super) struct Tui {
    pub colored: bool,
    pub fallback_width: u16,
}

#[derive(Debug, Clone, Args, Default)]
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
        Self::from(TuiArgs::default())
    }
}

impl From<TuiArgs> for Tui {
    fn from(cli: TuiArgs) -> Self {
        Self {
            colored: if cli.debug_color {
                true
            } else if cli.debug_no_color {
                false
            } else {
                stderr().is_tty()
            },
            fallback_width: cli.debug_term_size.unwrap_or(80),
        }
    }
}

impl Hooks for Tui {
    fn before_instruction(
        &mut self,
        instruction: &Instruction<usize>,
        pos: Range<usize>,
        memory: &Memory,
    ) {
        let width = crossterm::terminal::size()
            .ok()
            .map_or(self.fallback_width, |(w, _)| w) as usize;

        let ip_row = TuiRow::new(
            TuiRowHeader::new_for_ip(memory),
            memory,
            pos.start,
            instruction_cells(instruction, pos.start, memory),
            width,
        );

        let rb_header = TuiRowHeader::new_for_rb(memory);
        let rb_window_width = width.saturating_sub(rb_header.width());
        let rb_window = match usize::try_from(memory.rb()) {
            Ok(pos) => TuiMemoryWindow::new(
                memory,
                pos,
                vec![TuiCell::for_rb_value(memory.get(pos))],
                rb_window_width,
            ),
            Err(_) => {
                let mut w = TuiMemoryWindow::new(memory, 0, vec![], rb_window_width);
                w.start_marker = StartMarker::NegativeOverflow;
                w
            }
        };
        let rb_row = TuiRow {
            header: rb_header,
            window: rb_window,
        };

        let mut out = stderr();

        if self.colored {
            let _ = ip_row.print::<true>(&mut out);
            let _ = rb_row.print::<true>(&mut out);
        } else {
            let _ = ip_row.print::<false>(&mut out);
            let _ = rb_row.print::<false>(&mut out);
        }
    }
}

fn instruction_cells(
    instruction: &Instruction<usize>,
    pos: usize,
    memory: &Memory,
) -> Vec<TuiCell<2>> {
    let mut cells = Vec::with_capacity(instruction.len());

    cells.push(TuiCell::for_opcode(memory.get(pos), instruction.opcode()));

    use Instruction::*;

    match instruction {
        Add(a, b, c) | Mul(a, b, c) | Slt(a, b, c) | Seq(a, b, c) => {
            cells.push(TuiCell::for_param(a.0, memory.get(a.1)));
            cells.push(TuiCell::for_param(b.0, memory.get(b.1)));
            cells.push(TuiCell::for_param(c.0.into(), memory.get(c.1)));
        }
        Jnz(a, b) | Jez(a, b) => {
            cells.push(TuiCell::for_param(a.0, memory.get(a.1)));
            cells.push(TuiCell::for_param(b.0, memory.get(b.1)));
        }
        Inp(a) => {
            cells.push(TuiCell::for_param(a.0.into(), memory.get(a.1)));
        }
        Out(a) | Inb(a) => {
            cells.push(TuiCell::for_param(a.0, memory.get(a.1)));
        }
        Hlt => {}
    }

    cells
}

#[derive(Debug, Clone)]
struct TuiCell<const HEIGHT: usize> {
    style: ContentStyle,
    lines: [Cow<'static, str>; HEIGHT],
}

const CELL_SEPARATOR: &str = " ";

impl<const H: usize> TuiCell<H> {
    fn width(&self) -> usize {
        self.lines.iter().map(|l| l.len()).max().unwrap_or(0)
    }

    fn print<const STYLE: bool>(&self, out: &mut impl io::Write, row: usize) -> io::Result<()> {
        let width = self.width();
        let content = format!("{:width$}", &self.lines[row]);

        if STYLE {
            write!(out, "{}", self.style.apply(content))
        } else {
            write!(out, "{content}")
        }
    }
}

impl TuiCell<2> {
    fn for_memory_value(value: &Value) -> Self {
        Self {
            style: ContentStyle {
                foreground_color: Some(colors::MEMORY_VALUE),
                ..Default::default()
            },
            lines: [Cow::Owned(value.to_string()), Cow::Borrowed("")],
        }
    }

    fn for_rb_value(value: &Value) -> Self {
        let value = value.to_string();
        let carets = "^".repeat(value.len());
        Self {
            style: ContentStyle {
                foreground_color: Some(colors::RB_VALUE),
                ..Default::default()
            },
            lines: [Cow::Owned(value), Cow::Owned(carets)],
        }
    }

    fn for_opcode(value: &Value, opcode: OpCode) -> Self {
        Self {
            style: ContentStyle {
                foreground_color: Some(colors::OPCODE),
                ..Default::default()
            },
            lines: [
                Cow::Owned(value.to_string()),
                Cow::Owned(opcode.to_string()),
            ],
        }
    }
    fn for_param(mode: ReadParamMode, value: &Value) -> Self {
        let value = value.to_string();
        let param = match mode {
            ReadParamMode::Absolute => value.clone(),
            ReadParamMode::Immediate => format!("#{value}"),
            ReadParamMode::Relative => format!("@{value}"),
        };
        Self {
            style: ContentStyle {
                foreground_color: Some(match mode {
                    ReadParamMode::Absolute => colors::ABSOLUTE,
                    ReadParamMode::Immediate => colors::IMMEDIATE,
                    ReadParamMode::Relative => colors::RELATIVE,
                }),
                ..Default::default()
            },
            lines: [Cow::Owned(value), Cow::Owned(param)],
        }
    }
}

#[derive(Debug, Clone)]
struct TuiRowHeader<const HEIGHT: usize> {
    labels: TuiCell<HEIGHT>,
    values: TuiCell<HEIGHT>,
}

impl<const H: usize> TuiRowHeader<H> {
    fn width(&self) -> usize {
        self.labels.width() + CELL_SEPARATOR.len() + self.values.width()
    }

    fn print<const STYLE: bool>(&self, out: &mut impl io::Write, row: usize) -> io::Result<()> {
        self.labels.print::<STYLE>(out, row)?;
        write!(out, "{CELL_SEPARATOR}")?;
        self.values.print::<STYLE>(out, row)?;
        Ok(())
    }
}

impl TuiRowHeader<2> {
    fn new_for_ip(mem: &Memory) -> Self {
        Self {
            labels: IP_HEADER_LABELS,
            values: TuiCell {
                style: ContentStyle {
                    foreground_color: Some(colors::IP_VALUE),
                    ..Default::default()
                },
                lines: [Cow::Owned(mem.ip().to_string()), Cow::Borrowed("")],
            },
        }
    }
    fn new_for_rb(mem: &Memory) -> Self {
        Self {
            labels: RB_HEADER,
            values: TuiCell {
                style: ContentStyle {
                    foreground_color: Some(colors::RB_VALUE),
                    ..Default::default()
                },
                lines: [Cow::Owned(mem.rb().to_string()), Cow::Borrowed("")],
            },
        }
    }
}

const ELLIPSE_HEADER: TuiCell<2> = TuiCell {
    style: ContentStyle {
        foreground_color: Some(colors::ELLIPSIS),
        background_color: None,
        underline_color: None,
        attributes: Attributes::none(),
    },
    lines: [Cow::Borrowed("..."), Cow::Borrowed("")],
};
const NEG_OVERFLOW_HEADER: TuiCell<2> = TuiCell {
    style: ContentStyle {
        foreground_color: Some(colors::ELLIPSIS),
        background_color: None,
        underline_color: None,
        attributes: Attributes::none(),
    },
    lines: [Cow::Borrowed("<<<"), Cow::Borrowed("")],
};
const IP_HEADER_LABELS: TuiCell<2> = TuiCell {
    style: ContentStyle {
        foreground_color: Some(colors::IP_LABEL),
        background_color: None,
        underline_color: None,
        attributes: Attributes::none(),
    },
    lines: [Cow::Borrowed("IP:"), Cow::Borrowed("")],
};
const RB_HEADER: TuiCell<2> = TuiCell {
    style: ContentStyle {
        foreground_color: Some(colors::RB_LABEL),
        background_color: None,
        underline_color: None,
        attributes: Attributes::none(),
    },
    lines: [Cow::Borrowed("RB:"), Cow::Borrowed("")],
};

#[derive(Debug, Clone, Copy)]
enum StartMarker {
    None,
    Ellipsis,
    NegativeOverflow,
}

#[derive(Debug, Clone)]
struct TuiMemoryWindow {
    cells: Vec<TuiCell<2>>,
    start_marker: StartMarker,
    ellipse_end: bool,
}

impl TuiMemoryWindow {
    fn new(memory: &Memory, pos: usize, center: Vec<TuiCell<2>>, width: usize) -> Self {
        let center_len = center.len();
        let mem_len = memory.non_zero_length();

        let mut window_start = pos;
        let mut window_end = pos + center_len;

        let mut window = Self {
            cells: center,
            start_marker: if window_start > 0 {
                StartMarker::Ellipsis
            } else {
                StartMarker::None
            },
            ellipse_end: window_end < mem_len,
        };

        loop {
            let can_add_before = window_start > 0;
            let can_add_after = window_end < mem_len;

            if !can_add_before && !can_add_after {
                break;
            }

            if can_add_before {
                let new_idx = window_start - 1;
                window
                    .cells
                    .insert(0, TuiCell::for_memory_value(memory.get(new_idx)));
                window_start = new_idx;
                window.start_marker = if new_idx > 0 {
                    StartMarker::Ellipsis
                } else {
                    StartMarker::None
                };

                if window.width() >= width {
                    window.cells.remove(0);
                    window_start = new_idx + 1;
                    window.start_marker = if window_start > 0 {
                        StartMarker::Ellipsis
                    } else {
                        StartMarker::None
                    };
                    break;
                }
            }

            if can_add_after {
                let new_idx = window_end;
                window
                    .cells
                    .push(TuiCell::for_memory_value(memory.get(new_idx)));
                window_end = new_idx + 1;
                window.ellipse_end = window_end < mem_len;

                if window.width() >= width {
                    window.cells.pop();
                    window_end = new_idx;
                    window.ellipse_end = window_end < mem_len;
                    break;
                }
            }
        }

        window
    }

    fn width(&self) -> usize {
        let start_width = match self.start_marker {
            StartMarker::None => None,
            StartMarker::Ellipsis => Some(ELLIPSE_HEADER.width()),
            StartMarker::NegativeOverflow => Some(NEG_OVERFLOW_HEADER.width()),
        };

        let cells_widths = start_width
            .into_iter()
            .chain(self.cells.iter().map(TuiCell::width))
            .chain(self.ellipse_end.then_some(ELLIPSE_HEADER.width()));

        Itertools::intersperse(cells_widths, 1).sum::<usize>()
    }

    fn print<const STYLE: bool>(&self, mut out: &mut impl io::Write, row: usize) -> io::Result<()> {
        let start_cell: Option<&TuiCell<2>> = match self.start_marker {
            StartMarker::None => None,
            StartMarker::Ellipsis => Some(&ELLIPSE_HEADER),
            StartMarker::NegativeOverflow => Some(&NEG_OVERFLOW_HEADER),
        };
        start_cell
            .into_iter()
            .chain(&self.cells)
            .chain(self.ellipse_end.then_some(&ELLIPSE_HEADER))
            .map(|cell| {
                write!(out, "{CELL_SEPARATOR}")?;
                cell.print::<STYLE>(&mut out, row)
            })
            .collect()
    }
}

#[derive(Debug, Clone)]
struct TuiRow {
    header: TuiRowHeader<2>,
    window: TuiMemoryWindow,
}
impl TuiRow {
    fn new(
        header: TuiRowHeader<2>,
        memory: &Memory,
        pos: usize,
        center: Vec<TuiCell<2>>,
        width: usize,
    ) -> Self {
        let window =
            TuiMemoryWindow::new(memory, pos, center, width.saturating_sub(header.width()));
        Self { header, window }
    }

    fn print<const STYLE: bool>(&self, out: &mut impl io::Write) -> io::Result<()> {
        self.header.print::<STYLE>(out, 0)?;
        self.window.print::<STYLE>(out, 0)?;
        writeln!(out)?;
        self.header.print::<STYLE>(out, 1)?;
        self.window.print::<STYLE>(out, 1)?;
        writeln!(out)?;
        Ok(())
    }
}
