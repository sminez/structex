//! Compiling of [Ast] nodes into a complete [Prog];
use crate::{
    Error,
    ast::{self, Ast, Parser, Sequence},
    se::{Action, Extract, Guard, Narrow},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Inst {
    /// Run all branches in parallel over the current dot
    Parallel(Vec<Inst>),

    /// For each match of the regex in dot, set dot and run `on_extract`
    /// Between each match of the regex in dot, set dot and run `on_filter`
    Extract(Extract),

    /// If re matches, run if_matching for the current dot otherwise run if_not_matching
    Guard(Guard),

    /// Narrow the current match if possible, otherwise end the branch
    Narrow(Narrow),

    /// An action to emit alongside the corresponding match
    Action(usize),

    /// Default action: emit only the match itself
    EmitMatch,
}

#[derive(Debug, Default)]
pub(crate) struct Compiler {
    pub(crate) re: Vec<String>,
    pub(crate) tags: String,
    pub(crate) actions: Vec<Action>,
    pub(crate) allowed_argless_tags: Option<String>,
    pub(crate) allowed_single_arg_tags: Option<String>,
}

impl Compiler {
    pub fn compile(&mut self, s: &str) -> Result<Inst, Error> {
        let ast = Parser::new(s)
            .with_allowed_argless_tags(self.allowed_argless_tags.as_deref())
            .with_allowed_single_arg_tags(self.allowed_single_arg_tags.as_deref())
            .parse()?;

        Ok(self.instructions_for(ast))
    }

    fn instructions_for(&mut self, node: Ast) -> Inst {
        match node {
            Ast::Narrow(n) => self.add_for_narrow(n),

            Ast::Extract(ext) => self.add_for_extract(ext, false),
            Ast::ExtractBetween(ext) => self.add_for_extract(ext, true),

            Ast::Parallel(Sequence { nodes, .. }) => self.add_for_parallel(nodes),

            Ast::Guard(g) => self.add_for_guard(g, false),
            Ast::InvGuard(g) => self.add_for_guard(g, true),

            Ast::Action(a) => self.add_for_action(a),

            Ast::Comment(_) => panic!("Parser returned comment"),
        }
    }

    fn push_re(&mut self, re: String) -> usize {
        match self.re.iter().position(|s| s == &re) {
            Some(idx) => idx,
            None => {
                self.re.push(re);
                self.re.len() - 1
            }
        }
    }

    fn push_tag(&mut self, tag: char) {
        if !self.tags.contains(tag) {
            self.tags.push(tag)
        }
    }

    fn push_action(&mut self, action: Action) -> usize {
        match self.actions.iter().position(|a| a == &action) {
            Some(idx) => idx,
            None => {
                self.actions.push(action);
                self.actions.len() - 1
            }
        }
    }

    fn add_for_narrow(&mut self, n: ast::ReNode) -> Inst {
        let re = self.push_re(n.re);
        let node = Box::new(self.instructions_for(*n.node));

        Inst::Narrow(Narrow { re, node })
    }

    fn add_for_guard(&mut self, g: ast::ReNode, inverted: bool) -> Inst {
        let re = self.push_re(g.re);
        let branch = Box::new(self.instructions_for(*g.node));

        let (if_matching, if_not_matching) = if inverted {
            (None, Some(branch))
        } else {
            (Some(branch), None)
        };

        Inst::Guard(Guard {
            re,
            if_matching,
            if_not_matching,
        })
    }

    fn add_for_extract(&mut self, ext: ast::ReNode, between: bool) -> Inst {
        let re = self.push_re(ext.re);
        let branch = Box::new(self.instructions_for(*ext.node));

        let (on_extract, on_filter) = if between {
            (None, Some(branch))
        } else {
            (Some(branch), None)
        };

        Inst::Extract(Extract {
            re,
            on_extract,
            on_filter,
        })
    }

    fn add_for_parallel(&mut self, nodes: Vec<Ast>) -> Inst {
        let mut branches = Vec::new();

        for node in nodes {
            let branch = self.instructions_for(node);

            match branch {
                Inst::Guard(g) => {
                    if let Some(g) = g.try_merge(&mut branches) {
                        branches.push(Inst::Guard(g));
                    }
                }

                Inst::Extract(e) => {
                    if let Some(e) = e.try_merge(&mut branches) {
                        branches.push(Inst::Extract(e));
                    }
                }

                branch => branches.push(branch),
            }
        }

        if branches.len() == 1 {
            branches.remove(0)
        } else {
            Inst::Parallel(branches)
        }
    }

    fn add_for_action(&mut self, action: ast::Action) -> Inst {
        match action.tag {
            Some(tag) => {
                self.push_tag(tag);
                let action = self.push_action(Action { tag, arg: action.s });

                Inst::Action(action)
            }

            None => Inst::EmitMatch,
        }
    }
}
