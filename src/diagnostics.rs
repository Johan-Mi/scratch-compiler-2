use codemap_diagnostic::{ColorConfig, Diagnostic, Emitter, Level, SpanLabel};

#[derive(Default)]
pub struct Diagnostics(Vec<Diagnostic>);

impl Diagnostics {
    pub fn show(self) {
        if !self.0.is_empty() {
            Emitter::stderr(ColorConfig::Auto, None).emit(&self.0);
        }
    }

    pub fn error(
        &mut self,
        message: impl Into<String>,
        labels: impl Into<Vec<SpanLabel>>,
    ) {
        self.0.push(Diagnostic {
            level: Level::Error,
            message: message.into(),
            code: None,
            spans: labels.into(),
        });
    }

    pub fn note(
        &mut self,
        message: impl Into<String>,
        labels: impl Into<Vec<SpanLabel>>,
    ) {
        self.0.push(Diagnostic {
            level: Level::Note,
            message: message.into(),
            code: None,
            spans: labels.into(),
        });
    }
}
