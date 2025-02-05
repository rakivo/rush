use crate::loc::Loc;
use crate::parser::{prep, Job, Defs, Shadows};

#[cfg_attr(feature = "dbg", derive(Debug))]
enum TemplateChunk<'a> {
    Static(&'a str),
    JoinedStatic(&'a str),
    Placeholder(&'a str),
    JoinedPlaceholder(&'a str),
}

#[cfg_attr(feature = "dbg", derive(Debug))]
pub struct Template<'a> {
    loc: Loc,
    in_used: bool,
    chunks: Vec::<TemplateChunk<'a>>,
}

impl Template<'_> {
    const CONSTANT_PLACEHOLDERS: &'static [&'static str] = &["in", "out"];

    pub fn new(s: &str, loc: Loc) -> Template {
        let mut start = 0;
        let mut chunks = Vec::new();

        let mut in_used = false;
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
                let ref placeholder = s[placeholder_start..placeholder_end];
                in_used = placeholder == "in";
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

        Template { loc, in_used, chunks }
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
    fn _compile<'a>(&self, output_str: &str, input_str: &str, shadows: &Shadows, defs: &Defs) -> Result::<String, String> {
        self.chunks.iter().map(|c| {
            match c {
                TemplateChunk::Static(s) | TemplateChunk::JoinedStatic(s) => Ok(*s),
                TemplateChunk::Placeholder(placeholder) | TemplateChunk::JoinedPlaceholder(placeholder) => match *placeholder {
                    "in" => Ok(input_str),
                    "out" => Ok(output_str),
                    _ => shadows
                        .as_ref()
                        .and_then(|shadows| shadows.get(placeholder).map(|shadow| shadow.value))
                        .or_else(|| defs.get(placeholder).map(|def| def.value))
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
    pub fn compile_def<'a>(&self, defs: &Defs) -> String {
        self.chunks.iter().map(|c| {
            match c {
                TemplateChunk::Static(s) | TemplateChunk::JoinedStatic(s) => *s,
                TemplateChunk::Placeholder(placeholder) | TemplateChunk::JoinedPlaceholder(placeholder) => match *placeholder {
                    "in" => report!(self.loc, "$in is not allowed in variables definitions"),
                    "out" => report!(self.loc, "$out is not allowed in variables definitions"),
                    _ => defs.get(placeholder).map(|def| def.value).unwrap_or_else(|| {
                        report!(self.loc, "undefined variable: {placeholder}")
                    })
                },
            }
        }).collect()
    }
}
