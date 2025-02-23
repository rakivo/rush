use crate::loc::Loc;
use crate::types::StrHashSet;
use crate::dbg_unwrap::DbgUnwrap;
use crate::parser::comp::{Edge, Defs};
use crate::parser::{prep, comp, Shadows};

#[cfg_attr(feature = "dbg", derive(Debug))]
pub enum TemplateChunk<'a> {
    Static(&'a str),
    JoinedStatic(&'a str),
    Placeholder(&'a str),
    JoinedPlaceholder(&'a str),
}

#[cfg_attr(feature = "dbg", derive(Debug))]
pub struct Template<'a> {
    in_used: bool,
    statics_len: usize,

    pub loc: Loc<'a>,
    pub chunks: Vec::<TemplateChunk<'a>>,
}

impl<'a> Template<'a> {
    const AVERAGE_COMPILED_CHUNK_SIZE: usize = 24;
    const CONSTANT_PLACEHOLDERS: &'static [&'static str] = &["in", "out"];

    #[cfg_attr(feature = "dbg", track_caller)]
    pub fn new(s: &'a str, loc: Loc<'a>) -> Template<'a> {
        let mut start = 0;
        let mut in_used = false;
        let mut statics_len = 0;
        let mut chunks = Vec::with_capacity(s.len() / 2);

        #[inline(always)]
        fn is_joined_static(s: &str) -> bool {
            s.as_bytes().first().map_or(false, |b| !b.is_ascii_whitespace())
        }

        while let Some(i) = s[start..].find('$') {
            let i = start + i;
            if i > start {
                let ref not_trimmed = s[start..i];
                let trimmed = not_trimmed.trim();
                if !trimmed.is_empty() {
                    let chunk = if is_joined_static(not_trimmed) {
                        TemplateChunk::JoinedStatic(trimmed)
                    } else {
                        TemplateChunk::Static(trimmed)
                    };
                    chunks.push(chunk);
                    statics_len += trimmed.len();
                }
            }

            let placeholder_start = i + 1;
            let curly_syntax = s.as_bytes().get(placeholder_start) == Some(&b'{');
            let placeholder_end = if curly_syntax {
                s[placeholder_start + 1..]
                    .find('}')
                    .map(|end| placeholder_start + 1 + end)
                    .unwrap_or_else(|| report_panic!(loc, "missing closing brace for placeholder"))
            } else {
                s[placeholder_start..]
                    .find(|c: char| !c.is_alphanumeric() && c != '_')
                    .map(|end| placeholder_start + end)
                    .unwrap_or_else(|| s.len())
            };

            let is_joined_placeholder = || -> bool {
                if i == 0 { return false }
                !s[i-1..placeholder_end].as_bytes()
                    .iter()
                    .any(|b| b.is_ascii_whitespace())
            };

            if placeholder_start < placeholder_end {
                let placeholder = if curly_syntax {
                    &s[placeholder_start + 1..placeholder_end]
                } else {
                    &s[placeholder_start..placeholder_end]
                };

                in_used = placeholder == "in";

                let chunk = if is_joined_placeholder() {
                    TemplateChunk::JoinedPlaceholder(placeholder)
                } else {
                    TemplateChunk::Placeholder(placeholder)
                };

                chunks.push(chunk);
            } else {
                report_panic!(loc, "empty placeholder")
            }

            start = placeholder_end;
            if curly_syntax { start += 1 }
        }

        if start < s.len() {
            let ref not_trimmed = s[start..];
            let trimmed = not_trimmed.trim();
            if !trimmed.is_empty() {
                let chunk = if is_joined_static(not_trimmed) {
                    TemplateChunk::JoinedStatic(trimmed)
                } else {
                    TemplateChunk::Static(trimmed)
                };
                chunks.push(chunk);
                statics_len += trimmed.len();
            }
        }

        Template { loc, in_used, statics_len, chunks }
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

    #[inline(always)]
    fn allocate_result(&self) -> String {
        String::with_capacity(self.statics_len + self.chunks.len() * Self::AVERAGE_COMPILED_CHUNK_SIZE)
    }

    fn _compile(&self, target_str: &str, input_str: &str, shadows: &Shadows, defs: &Defs) -> Result::<String, String> {
        let mut ret = self.allocate_result();
        for chunk in self.chunks.iter() {
            match chunk {
                TemplateChunk::Static(s) => {
                    if !ret.is_empty() && !s.is_empty() { ret.push(' ') }
                    ret.push_str(s)
                }
                TemplateChunk::JoinedStatic(s) => ret.push_str(s),
                TemplateChunk::Placeholder(placeholder) => {
                    let compiled = match *placeholder {
                        "in" => Ok(input_str),
                        "out" => Ok(target_str),
                        _ => shadows
                            .as_ref()
                            .and_then(|shadows| shadows.get(placeholder).map(|shadow| *shadow))
                            .or_else(|| defs.get(placeholder).map(|def| def.0.as_str()))
                            .ok_or(report_fmt!(self.loc, "undefined variable: {placeholder}")),
                    }?;

                    if !ret.is_empty() && !compiled.is_empty() { ret.push(' ') }
                    ret.push_str(compiled)
                }
                TemplateChunk::JoinedPlaceholder(placeholder) => {
                    ret.push_str(match *placeholder {
                        "in" => Ok(input_str),
                        "out" => Ok(target_str),
                        _ => shadows
                            .as_ref()
                            .and_then(|shadows| shadows.get(placeholder).map(|shadow| *shadow))
                            .or_else(|| defs.get(placeholder).map(|def| def.0.as_str()))
                            .ok_or(report_fmt!(self.loc, "undefined variable: {placeholder}"))
                    }?);
                }
            }
        } Ok(ret)
    }

    #[inline(always)]
    pub fn compile(&self, edge: &Edge, defs: &Defs) -> Result::<String, String> {
        self._compile(edge.target, edge.inputs_str(self.in_used), &edge.shadows, defs)
    }

    #[inline(always)]
    pub fn compile_prep(&self, edge: &prep::Edge, defs: &Defs) -> Result::<String, String> {
        self._compile(edge.target, edge.inputs_str(self.in_used), &edge.shadows, defs)
    }

    #[inline]
    #[cfg_attr(feature = "dbg", track_caller)]
    pub fn compile_def(&self, defs: &Defs) -> String {
        let mut ret = self.allocate_result();
        for chunk in self.chunks.iter() {
            match chunk {
                TemplateChunk::Static(s) => {
                    if !ret.is_empty() && !s.is_empty() { ret.push(' ') }
                    ret.push_str(s)
                },
                TemplateChunk::JoinedStatic(s) => ret.push_str(s),
                TemplateChunk::Placeholder(placeholder) => {
                    let compiled = match *placeholder {
                        "in" => report_panic!(self.loc, "$in is not allowed in variables definitions"),
                        "out" => report_panic!(self.loc, "$out is not allowed in variables definitions"),
                        _ => defs.get(placeholder).map(|def| def.0.as_str()).unwrap_or_else(|| {
                            report_panic!(self.loc, "undefined variable: {placeholder}")
                        })
                    };

                    if !ret.is_empty() && !placeholder.is_empty() { ret.push(' ') }
                    ret.push_str(compiled)
                },
                TemplateChunk::JoinedPlaceholder(placeholder) => {
                    ret.push_str(match *placeholder {
                        "in" => report_panic!(self.loc, "$in is not allowed in variables definitions"),
                        "out" => report_panic!(self.loc, "$out is not allowed in variables definitions"),
                        _ => defs.get(placeholder).map(|def| def.0.as_str()).unwrap_or_else(|| {
                            report_panic!(self.loc, "undefined variable: {placeholder}")
                        })
                    })
                }
            }
        } ret
    }

    pub fn compile_def_recursive<'b>(
        name: &'b str,
        def: &prep::Def<'b>,
        defs: &prep::Defs<'b>,
        compiling: &mut StrHashSet<'b>,
        compiled_defs: &mut comp::Defs<'b>,
    ) {
        if compiling.contains(name) {
            report_panic!{
                def.0.loc,
                "circular reference detected involving {name}"
            }
        }

        if compiled_defs.contains_key(name) { return }

        compiling.insert(name);

        let mut ret = String::with_capacity(32);
        for chunk in def.0.chunks.iter() {
            match chunk {
                TemplateChunk::Static(s) => {
                    if !ret.is_empty() && !s.is_empty() { ret.push(' ') }
                    ret.push_str(s)
                }
                TemplateChunk::JoinedStatic(s) => ret.push_str(s),
                TemplateChunk::Placeholder(placeholder) => {
                    if !ret.is_empty() && !placeholder.is_empty() { ret.push(' ') }

                    if compiling.contains(placeholder) {
                        report_panic!{
                            def.0.loc,
                            "circular reference detected involving {placeholder}"
                        }
                    }

                    let compiled = match defs.0.get(placeholder) {
                        Some(def) => {
                            if !compiled_defs.contains_key(placeholder) {
                                Template::compile_def_recursive(placeholder, def, defs, compiling, compiled_defs);
                            }
                            compiled_defs.get(placeholder).unwrap_dbg().0.as_str()
                        }
                        None => report_panic!{
                            def.0.loc,
                            "undefined variable: {placeholder}"
                        }
                    };

                    ret.push_str(compiled)
                }
                TemplateChunk::JoinedPlaceholder(placeholder) => {
                    if compiling.contains(placeholder) {
                        report_panic!{
                            def.0.loc,
                            "circular reference detected involving {placeholder}"
                        }
                    }

                    let compiled = match defs.0.get(placeholder) {
                        Some(def) => {
                            if !compiled_defs.contains_key(placeholder) {
                                Template::compile_def_recursive(placeholder, def, defs, compiling, compiled_defs);
                            }
                            compiled_defs.get(placeholder).unwrap_dbg().0.as_str()
                        }
                        None => report_panic!{
                            def.0.loc,
                            "undefined variable: {placeholder}"
                        }
                    };

                    ret.push_str(compiled)
                }
            }
        }

        compiling.remove(name);
        compiled_defs.insert(name, comp::Def(ret));
    }
}
