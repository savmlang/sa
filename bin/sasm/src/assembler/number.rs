use crate::assembler::{OutValue, State};

macro_rules! mixedradix {
  ($data:ident, $radix:expr) => {
    let data = { $data.get(2..).expect("Malformed integer literal") };

    let (data, width) = data.split_once("::u").unwrap();

    let width = width.parse::<u8>().unwrap();
    let intdata = u64::from_str_radix(data, $radix).unwrap();

    let mask = OutValue::mask(width);

    if intdata > mask {
      panic!("Integral overflow!");
    }

    let output = intdata & mask;

    let $data = OutValue {
      data: output,
      width,
    };
  };
}

pub fn parse_expr<'a>(
  state: &mut State<'a>,
  data: &'a str,
  macromode: bool,
  macroemit: bool,
) -> OutValue {
  // Resolve variables!
  if &data[0..=0] == "$" {
    let map = if macromode {
      &state.curr_macro.as_ref().unwrap().resolved
    } else {
      &state.resolved
    };
    let (data, width) = &data[1..].split_once("::u").unwrap();

    return map.get(data).unwrap().into_width(width.parse().unwrap());
  }

  if macromode {
    match &data[0..=0] {
      "^" => {
        let (data, width) = &data[1..].split_once("::u").unwrap();
        return state
          .resolved
          .get(data)
          .unwrap()
          .into_width(width.parse().unwrap());
      }
      "@" => {
        let argument_name = &data;

        let mc = state.curr_macro.as_mut().unwrap();

        let idx = mc.compiled.len().wrapping_sub(1);

        let pos = mc
          .args
          .iter()
          .position(|x| *x == *argument_name)
          .expect("Found unknown argument");

        if !macroemit {
          if idx == usize::MAX {
            panic!("Macro operand cannot be used in the place!");
          }

          let reloctable = mc.reloctable.entry(idx).or_default();

          reloctable.push(pos);
        }

        return OutValue {
          data: pos as _,
          width: 0,
        };
      }
      _ => {}
    }
  }

  match &data[0..=1] {
    // %u N [...]
    "%u" => {
      let bits = data.strip_prefix("%u").unwrap();

      let (width, data) = bits.split_once("[").unwrap();

      let width = width.parse::<u8>().unwrap();

      let (data, width) = data
        .strip_suffix("]")
        .unwrap()
        .split("|")
        .map(|x| x.trim())
        .map(|x| parse_expr(state, x, macromode, macroemit))
        .fold((0u64, 0u8), |(acc_data, acc_width), out| {
          // Check for overflow before shifting
          if acc_width + out.width > width {
            panic!(
              "Pack overflow: {} bits exceeds %u{}",
              acc_width + out.width,
              width
            );
          }

          // The "Reverse" logic: Shift existing data left, then OR in the new LSB
          let new_data = (acc_data << out.width) | (out.data & OutValue::mask(out.width));
          (new_data, acc_width + out.width)
        });

      let mask = OutValue::mask(width);
      let output = data & mask;

      OutValue {
        data: output,
        width,
      }
    }
    "0b" => {
      mixedradix!(data, 2);

      data
    }
    "0x" => {
      mixedradix!(data, 16);

      data
    }
    "0o" => {
      mixedradix!(data, 8);

      data
    }
    "fp" => {
      let data = data.strip_prefix("fp").unwrap();
      let (data, width) = data.split_once("::u").unwrap();

      let width = width.parse::<u8>().unwrap();

      let data = match width {
        32 => {
          let v = lexical_core::parse::<f32>(data.as_bytes()).unwrap();

          v.to_bits() as u64
        }
        64 => {
          let v = lexical_core::parse::<f64>(data.as_bytes()).unwrap();

          v.to_bits() as u64
        }
        width => panic!("Unknown Floating Point : {width}"),
      };

      OutValue { data, width }
    }
    // Fallback to basic parsing
    _ => {
      println!("{data}");
      let (data, width) = data.split_once("::u").unwrap();
      let width = width.parse::<u8>().unwrap();

      let mask = OutValue::mask(width);

      // Convert to `I64` where I stands for INTEGER
      let intdata = if data.starts_with("-") {
        let dat = data
          .parse::<i64>()
          .expect("Unable to parse as negative integer");

        // The Min Calc: -2^(width-1)
        // Using 1i64 ensures we don't overflow during the shift
        let min_allowed = if width < 64 {
          -(1i64 << (width - 1))
        } else {
          u64::MIN.cast_signed()
        };

        if width < 64 && dat < min_allowed {
          panic!(
            "Value {} is out of range for signed u{} (min: {})",
            dat, width, min_allowed
          );
        }

        dat.cast_unsigned()
      } else {
        let data = data.parse::<u64>().expect("Unable to parse as integer");

        if data > mask {
          panic!("Range overflow! {data} is >= 2^{width}");
        }

        data
      };

      let output = intdata & mask;

      OutValue {
        data: output,
        width,
      }
    }
  }
}
