enum Color {
    Blue,
    Cyan,
    Green,
    Magenta,
    Red,
    Yellow,
}
impl Color {
    fn to_string(&self) -> String {
        match self {
            Color::Blue => "94".into(),
            Color::Cyan => "96".into(),
            Color::Green => "92".into(),
            Color::Magenta => "95".into(),
            Color::Red => "91".into(),
            Color::Yellow => "93".into(),
        }
    }
}

pub(crate) trait ColorExt: std::fmt::Display {
    fn blue(self) -> String;
    fn cyan(self) -> String;
    fn green(self) -> String;
    fn magenta(self) -> String;
    fn red(self) -> String;
    fn yellow(self) -> String;
}
impl<T: std::fmt::Display> ColorExt for T {
    fn blue(self) -> String {
        color(self.to_string(), Color::Blue)
    }
    fn cyan(self) -> String {
        color(self.to_string(), Color::Cyan)
    }
    fn green(self) -> String {
        color(self.to_string(), Color::Green)
    }
    fn magenta(self) -> String {
        color(self.to_string(), Color::Magenta)
    }
    fn red(self) -> String {
        color(self.to_string(), Color::Red)
    }
    fn yellow(self) -> String {
        color(self.to_string(), Color::Yellow)
    }
}

/// Convert the given string in string with the given color
fn color<T: AsRef<str>>(str: T, color: Color) -> String {
    let str = str.as_ref();
    let mut out = String::new();

    // Start color escape sequence
    out.push_str("\x1B[");

    // Always set bold to keep it bright and simple
    out.push_str("1;");

    // Write out foreground color
    out.push_str(&color.to_string());

    // Close escape sequence
    out.push_str("m");

    // Write out the actual String
    out.push_str(&str);

    // Reset color
    out.push_str("\x1B[0m");

    out
}
