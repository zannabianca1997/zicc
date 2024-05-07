use self::typedef::TypeDefInt;

use super::*;

peg::parser! {
  grammar parser() for [Token] {
    rule typedef() -> typedef::TypeDef
      = def: typedefdata() {typedef::TypeDef::Data(def)}

    rule typedefdata() -> typedef::TypeDefData
      = def: typedefint() {typedef::TypeDefData::Int(def)}

    rule typedefint() -> typedef::TypeDefInt
      = [Token::Keyword(Keyword::Int(int_kwd))] { TypeDefInt{ int_kwd }}

    rule static() -> ItemStatic
      = [Token::Keyword(Keyword::Static(static_kw))]
        [Token::Identifier(ident)]
        [Token::Punct(Punct::Colon(colon))]
        ty: typedefdata()
        [Token::Punct(Punct::Semi(semi))]
      {
        ItemStatic { static_kw, ident, colon, ty, init: None, semi }
      }

    rule item() -> Item
      = item: static() { Item::Static(item)}

    pub rule file() -> File
      = items:(item() *) { File { items } }
  }
}
