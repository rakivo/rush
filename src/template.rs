use crate::loc::Loc;
use crate::parser::{prep, Job, Defs, Phony};

#[derive(Clone)]
#[cfg_attr(feature = "dbg", derive(Debug))]
enum TemplateChunk<'a> {
    Static(&'a str),
    JoinedStatic(&'a str),
    Placeholder(&'a str),
    JoinedPlaceholder(&'a str),
}

#[derive(Clone, Default)]
#[cfg_attr(feature = "dbg", derive(Debug))]
pub struct Template<'a> {
    loc: Loc,
    chunks: Vec::<TemplateChunk<'a>>,
}

impl Template<'_> {
    const CONSTANT_PLACEHOLDERS: &'static [&'static str] = &["in", "out"];

    pub fn new(s: &str, loc: Loc) -> Template {
        let mut start = 0;
        let mut chunks = Vec::new();
        let mut last_was_placeholder = false;

        while let Some(i) = s[start..].find('$') {
            let i = start + i;
            if i > start {
                let trimmed_static = &s[start..i];
                if !trimmed_static.is_empty() {
                    let chunk = if last_was_placeholder {
                        TemplateChunk::JoinedStatic(trimmed_static)
                    } else {
                        TemplateChunk::Static(trimmed_static)
                    };
                    chunks.push(chunk)
                }
            }

            let placeholder_start = i + 1;
            let placeholder_end = s[placeholder_start..]
                .find(|c: char| !c.is_alphanumeric() && c != '_')
                .map(|end| placeholder_start + end)
                .unwrap_or_else(|| s.len());

            if placeholder_start < placeholder_end {
                let placeholder = &s[placeholder_start..placeholder_end];
                let joined = placeholder_end < s.len() && s.chars().nth(placeholder_end) == Some('.');
                if joined {
                    chunks.push(TemplateChunk::JoinedPlaceholder(placeholder));
                } else {
                    chunks.push(TemplateChunk::Placeholder(placeholder));
                }
                last_was_placeholder = true
            } else {
                report!(loc, "empty placeholder")
            }

            start = placeholder_end
        }

        if start < s.len() {
            let trimmed_static = s[start..].trim();
            if !trimmed_static.is_empty() {
                let chunk = if last_was_placeholder {
                    TemplateChunk::JoinedStatic(trimmed_static)
                } else {
                    TemplateChunk::Static(trimmed_static)
                };
                chunks.push(chunk)
            }
        }

        Template { loc, chunks }
    }

    #[inline]
    pub fn check(&self, defs: &Defs) -> Result::<(), String> {
        for placeholder in self.chunks.iter().filter_map(|c| {
            match c {
                TemplateChunk::Placeholder(p) |
                TemplateChunk::JoinedPlaceholder(p) if !Self::CONSTANT_PLACEHOLDERS.contains(p) => Some(p),
                _ => None
            }
        }) {
            if !defs.contains_key(placeholder) {
                return Err(report_fmt!(self.loc, "undefined variable: {placeholder}"))
            }
        } Ok(())
    }

    #[inline]
    pub fn compile_(&self, job: &Job, defs: &Defs) -> Result::<String, String> {
        self.chunks.iter().map(|c| {
            match c {
                TemplateChunk::Static(s) | TemplateChunk::JoinedStatic(s) => Ok(*s),
                TemplateChunk::Placeholder(placeholder) | TemplateChunk::JoinedPlaceholder(placeholder) => match *placeholder {
                    "in" => Ok(match job.phony {
                        Phony::Phony { .. } => report!(job.loc, "$in is not supported yet in phony jobs"),
                        Phony::NotPhony { inputs_str, .. } => { inputs_str },
                    }),
                    "out" => Ok(job.target),
                    _ => job.shadows
                        .as_ref()
                        .and_then(|shadows| shadows.get(placeholder).map(|shadow| shadow.value))
                        .or_else(|| defs.get(placeholder).map(|def| def.value))
                        .ok_or(report_fmt!(self.loc, "undefined variable: {placeholder}"))
                },
            }
        }).collect()
    }

    #[inline]
    pub fn compile(&self, job: &prep::Job, defs: &Defs) -> Result::<String, String> {
        self.chunks.iter().map(|c| {
            match c {
                TemplateChunk::Static(s) | TemplateChunk::JoinedStatic(s) => Ok(*s),
                TemplateChunk::Placeholder(placeholder) | TemplateChunk::JoinedPlaceholder(placeholder) => match *placeholder {
                    "in" => Ok(match job.phony {
                        prep::Phony::Phony { .. } => report!(job.loc, "$in is not supported yet in phony jobs"),
                        prep::Phony::NotPhony { inputs_str, .. } => { inputs_str },
                    }),
                    "out" => Ok(job.target),
                    undefined => job.shadows
                        .as_ref()
                        .and_then(|shadows| shadows.get(placeholder).map(|shadow| shadow.value))
                        .or_else(|| defs.get(placeholder).map(|def| def.value))
                        .ok_or(report_fmt!(self.loc, "undefined variable: {undefined}"))
                }
            }
        }).collect()
    }
}
