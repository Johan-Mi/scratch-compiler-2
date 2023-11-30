use codemap::Span;
use codemap_diagnostic::{
    ColorConfig, Diagnostic, Emitter, Level, SpanLabel, SpanStyle,
};

#[derive(Default)]
pub struct Diagnostics(Vec<Diagnostic>);

impl Diagnostics {
    pub fn show(self, code_map: &codemap::CodeMap) {
        if !self.0.is_empty() {
            Emitter::stderr(ColorConfig::Auto, Some(code_map)).emit(&self.0);
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

pub fn primary(span: Span, text: impl Into<String>) -> SpanLabel {
    SpanLabel {
        span,
        label: Some(text.into()),
        style: SpanStyle::Primary,
    }
}

pub fn secondary(span: Span, text: impl Into<String>) -> SpanLabel {
    SpanLabel {
        span,
        label: Some(text.into()),
        style: SpanStyle::Secondary,
    }
}
