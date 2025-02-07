use crate::loc::Loc;
use crate::parser::{prep, Shadows};
use crate::parser::comp::{Job, Defs};

#[cfg_attr(feature = "dbg", derive(Debug))]
pub enum TemplateChunk<'a> {
    Static(&'a str),
    JoinedStatic(&'a str),
    Placeholder(&'a str),
    JoinedPlaceholder(&'a str),
}

#[cfg_attr(feature = "dbg", derive(Debug))]
pub struct Template<'a> {
    loc: Loc,
    in_used: bool,
    pub chunks: Vec::<TemplateChunk<'a>>,
}

impl Template<'_> {
    const CONSTANT_PLACEHOLDERS: &'static [&'static str] = &["in", "out"];

    #[cfg_attr(feature = "dbg", track_caller)]
    pub fn new(s: &str, loc: Loc) -> Template {
        let mut start = 0;
        let mut in_used = false;
        let mut chunks = Vec::new();
        let mut last_was_placeholder_or_joined = false;

        while let Some(i) = s[start..].find('$') {
            let i = start + i;
            if i > start {
                let trimmed_static = &s[start..i];
                if !trimmed_static.is_empty() {
                    let chunk = if last_was_placeholder_or_joined && trimmed_static.chars().all(|c| !c.is_whitespace()) {
                        TemplateChunk::JoinedStatic(trimmed_static)
                    } else {
                        TemplateChunk::Static(trimmed_static)
                    };
                    chunks.push(chunk);
                    last_was_placeholder_or_joined = false
                }
            }

            let placeholder_start = i + 1;
            let curly_syntax = s.as_bytes().get(placeholder_start) == Some(&b'{');
            let placeholder_end = if curly_syntax {
                s[placeholder_start + 1..]
                    .find('}')
                    .map(|end| placeholder_start + 1 + end)
                    .unwrap_or_else(|| report!(loc, "missing closing brace for placeholder"))
            } else {
                s[placeholder_start..]
                    .find(|c: char| !c.is_alphanumeric() && c != '_')
                    .map(|end| placeholder_start + end)
                    .unwrap_or_else(|| s.len())
            };

            if placeholder_start < placeholder_end {
                let placeholder = if curly_syntax {
                    &s[placeholder_start + 1..placeholder_end]
                } else {
                    &s[placeholder_start..placeholder_end]
                };

                in_used = placeholder == "in";

                let chunk = if last_was_placeholder_or_joined {
                    TemplateChunk::JoinedPlaceholder(placeholder)
                } else {
                    TemplateChunk::Placeholder(placeholder)
                };

                chunks.push(chunk);
                last_was_placeholder_or_joined = true
            } else {
                report!(loc, "empty placeholder")
            }

            start = placeholder_end;
            if curly_syntax { start += 1 }
        }

        if start < s.len() {
            let trimmed_static = s[start..].trim();
            if !trimmed_static.is_empty() {
                let chunk = if last_was_placeholder_or_joined && s[start..].chars().all(|c| !c.is_whitespace()) {
                    TemplateChunk::JoinedStatic(trimmed_static)
                } else {
                    TemplateChunk::Static(trimmed_static)
                };
                chunks.push(chunk)
            }
        }

        Template { loc, in_used, chunks }
    }

    #[inline]
    pub fn check(&self, shadows: &Shadows, defs: &Defs) -> Result::<(), String> {
        for placeholder in self.chunks.iter().filter_map(|c| {
            match c {
                TemplateChunk::Placeholder(p) |
                TemplateChunk::JoinedPlaceholder(p) if !Self::CONSTANT_PLACEHOLDERS.contains(p) => Some(p),
                _ => None
            }
        }) {
            if !shadows.as_ref().map_or(false, |s| s.contains_key(placeholder)) && !defs.contains_key(placeholder) {
                return Err(report_fmt!(self.loc, "undefined variable: {placeholder}"))
            }
        } Ok(())
    }

    #[inline]
    fn _compile(&self, output_str: &str, input_str: &str, shadows: &Shadows, defs: &Defs) -> Result::<String, String> {
        self.chunks.iter().map(|c| {
            match c {
                TemplateChunk::Static(s) | TemplateChunk::JoinedStatic(s) => Ok(*s),
                TemplateChunk::Placeholder(placeholder) | TemplateChunk::JoinedPlaceholder(placeholder) => match *placeholder {
                    "in" => Ok(input_str),
                    "out" => Ok(output_str),
                    _ => shadows
                        .as_ref()
                        .and_then(|shadows| shadows.get(placeholder).map(|shadow| *shadow))
                        .or_else(|| defs.get(placeholder).map(|def| def.value.as_str()))
                        .ok_or(report_fmt!(self.loc, "undefined variable: {placeholder}"))
                },
            }
        }).collect()
    }

    #[inline(always)]
    pub fn compile(&self, job: &Job, defs: &Defs) -> Result::<String, String> {
        self._compile(job.target, job.inputs_str(self.in_used), &job.shadows, defs)
    }

    #[inline(always)]
    pub fn compile_prep(&self, job: &prep::Job, defs: &Defs) -> Result::<String, String> {
        self._compile(job.target, job.inputs_str(self.in_used), &job.shadows, defs)
    }

    #[inline]
    #[cfg_attr(feature = "dbg", track_caller)]
    pub fn compile_def(&self, defs: &Defs) -> String {
        self.chunks.iter().map(|c| {
            match c {
                TemplateChunk::Static(s) | TemplateChunk::JoinedStatic(s) => *s,
                TemplateChunk::Placeholder(placeholder) | TemplateChunk::JoinedPlaceholder(placeholder) => match *placeholder {
                    "in" => report!(self.loc, "$in is not allowed in variables definitions\n"),
                    "out" => report!(self.loc, "$out is not allowed in variables definitions\n"),
                    _ => defs.get(placeholder).map(|def| def.value.as_str()).unwrap_or_else(|| {
                        report!(self.loc, "undefined variable: {placeholder}\n")
                    })
                },
            }
        }).collect()
    }
}
