use codemap::Pos;
use std::collections::HashMap;

type Result<T> = std::result::Result<T, ()>;

pub type ResolvedCalls = HashMap<Pos, usize>;

#[expect(clippy::enum_variant_names)]
pub enum Special {
    WhenFlagClicked,
    WhenKeyPressed,
    WhenCloned,
    WhenReceived,
}

impl TryFrom<&str> for Special {
    type Error = ();

    fn try_from(name: &str) -> Result<Self> {
        match name {
            "when-flag-clicked" => Ok(Self::WhenFlagClicked),
            "when-key-pressed" => Ok(Self::WhenKeyPressed),
            "when-cloned" => Ok(Self::WhenCloned),
            "when-received" => Ok(Self::WhenReceived),
            _ => Err(()),
        }
    }
}
