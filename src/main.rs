#![deny(unsafe_code)]
#![warn(clippy::nursery, clippy::pedantic)]

#[allow(unsafe_code)]
mod parser;

fn main() {
    let source_code = r"
        sprite Stage {

        }

        sprite Main {

        }
    ";
    let document = parser::parse(source_code);
    parser::print(0, document.into());
}
