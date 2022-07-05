#[allow(unused_macros)]
macro_rules! bigdec {
    ($lit:literal) => {{
        use std::str::FromStr;
        bigdecimal::BigDecimal::from_str(stringify!($lit)).expect("Invalid decimal float literal")
    }};
}
