use std::collections::HashMap;

pub mod extend;

pub fn parse_enum<'a>(enumcode: &'a str) -> HashMap<&'a str, &'a str> {
  let (_, begin) = enumcode
    .split_once("/* ENUM begin */")
    .expect("Unable to fetch enum begin");
  let (r#enum, _) = begin
    .split_once("/* ENUM end */")
    .expect("Unable to fetch enum end");

  let mut kw_map = HashMap::new();

  let mut value = Value::None;
  for line in r#enum
    .lines()
    .filter(|x| !x.starts_with("//") && !x.is_empty())
    .map(|x| x.trim())
  {
    if line.starts_with("#[regex") {
      value = Value::Verbatim;
      continue;
    } else if line.starts_with("#[token") {
      let value_str = line
        .strip_prefix("#[token(\"")
        .expect("Could not strip prefix")
        .split_once("\"")
        .expect("Could not strip suffix")
        .0;
      value = Value::Token(value_str);
      continue;
    } else if line.ends_with(",") {
      let enum_value = line.strip_suffix(",").unwrap();

      let k = match value {
        Value::None => panic!("Parsing error for token.rs in sasm-lexer"),
        Value::Token(x) => x,
        Value::Verbatim => enum_value,
      };

      kw_map.insert(k, enum_value);

      value = Value::None;
    }
  }

  kw_map
}

pub enum Value<'a> {
  Verbatim,
  Token(&'a str),
  None,
}
