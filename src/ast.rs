use crate::types::BuiltinType;
use derive_more::{Constructor, IsVariant, Unwrap};
use hashbrown::{HashMap, HashSet};
use num_bigint::BigInt;
use std::collections::VecDeque;
use std::fmt;

pub type NodeId = usize;

#[derive(Debug, Clone, IsVariant, Unwrap)]
pub enum Node {
    Ident(String),
    Number(BigInt),
    Binary(Binary),
    Unary(Unary),
    Call(CallExpr),
    FnProto(FnProto),
    FnLiteral(FnLiteral),
    Block(Block),
    BuiltinType(BuiltinType),
    Return(Option<NodeId>),
    ImplicitReturn(NodeId),
    VarStmt(VarStmt),
}

#[derive(Debug, Copy, Clone, Constructor)]
pub struct Binary {
    pub op: Oper,
    pub lhs: NodeId,
    pub rhs: NodeId,
}

#[derive(Debug, Copy, Clone, IsVariant)]
pub enum Oper {
    Add,
    Sub,
    Mul,
    Div,
    Rem,

    Eql,
    Neq,
    Gtr,
    Lss,
    Geq,
    Leq,

    And,
    Or,
    Xor,
    Shl,
    Shr,

    LogicAnd,
    LogicOr,
}
#[derive(Debug, Copy, Clone, Constructor)]
pub struct Unary {
    pub op: UnaryOp,
    pub inner: NodeId,
}

#[derive(Debug, Copy, Clone, IsVariant)]
pub enum UnaryOp {
    BitNot,
    Negation,
    AddressOf,
    LogicNot,
}

#[derive(Debug, Clone, Constructor)]
pub struct CallExpr {
    proc: NodeId,
    args: Vec<NodeId>,
}

#[derive(Debug, Clone, Constructor)]
pub struct FnProto {
    /// `NodeId` is a type definition node representing the type of the parameter.
    pub params: HashMap<String, NodeId>,
    pub result: Option<NodeId>,
}

/// Represents the syntax tree for the node `(x: int) -> int { x * 2 }`.
#[derive(Debug, Copy, Clone, Constructor)]
pub struct FnLiteral {
    pub type_defn: NodeId,
    pub block: NodeId,
}

/// A block is a list of statements with an optional implicit return,
/// as well as a scope containing all declarations defined in the block.
#[derive(Debug, Default, Clone, Constructor)]
pub struct Block {
    pub parent: Option<Box<Block>>,
    pub stmts: Vec<NodeId>,
    pub members: HashMap<String, usize>,
}

#[derive(Debug, Clone, Constructor)]
pub struct VarStmt {
    pub name: String,
    pub type_defn: Option<NodeId>,
    pub init_expr: NodeId,
}

#[derive(Debug, Clone)]
pub struct Decl {
    pub name: String,

    /// If the Decl has an explicit type annotation
    pub type_defn: Option<NodeId>,

    pub root_expr: NodeId,
    pub nodes: Vec<NodeId>,
}

impl Decl {
    pub fn new(name: String, type_defn: Option<NodeId>, root_expr: NodeId) -> Self {
        Self {
            name,
            type_defn,
            root_expr,
            nodes: Vec::new(),
        }
    }

    #[inline]
    pub fn with_expr(name: String, root_expr: NodeId) -> Self {
        Self::new(name, None, root_expr)
    }

    /// Iterates all identifiers in the Decl's syntax tree
    /// and finds the corresponding entry, returning a list of
    /// Decl ids or an error if an identifier is not defined.
    #[allow(dead_code)]
    fn resolve_idents(&self, ast: &Ast) -> Result<Vec<usize>, NodeId> {
        let mut items = Vec::new();
        for &index in &self.nodes {
            if let Some(node) = ast.nodes.get(index) {
                match node {
                    Node::Ident(name) => {
                        if let Some(decl_id) = ast.members.get(name) {
                            items.push(*decl_id);
                        } else {
                            return Err(index);
                        }
                    }
                    _ => {}
                }
            }
        }
        Ok(items)
    }

    /// Returns a set containing the indices of all
    /// declarations whose type must be inferred before
    /// this declaration.
    pub fn get_type_deps(&self, ast: &Ast) -> HashSet<usize> {
        let mut deps = HashSet::new();
        let mut stack = VecDeque::new();

        if let Some(type_defn) = self.type_defn {
            stack.push_back(type_defn);
        }
        stack.push_back(self.root_expr);

        while let Some(index) = stack.pop_front() {
            match &ast.nodes[index] {
                Node::Ident(name) => {
                    if let Some(&id) = ast.members.get(name) {
                        deps.insert(id);
                    }
                }
                Node::Number(_) | Node::Return(None) => {}
                Node::Binary(bin) => {
                    stack.push_back(bin.lhs);
                    stack.push_back(bin.rhs);
                }
                Node::Unary(unary) => {
                    stack.push_back(unary.inner);
                }
                Node::Call(CallExpr { proc, args }) => {
                    stack.push_back(*proc);
                    stack.extend(args);
                }
                Node::FnProto(FnProto { params, result }) => {
                    for (_, &type_defn) in params {
                        stack.push_back(type_defn);
                    }
                    if let Some(result) = result {
                        stack.push_back(*result);
                    }
                }
                Node::FnLiteral(FnLiteral { type_defn, .. }) => {
                    stack.push_back(*type_defn);
                }
                Node::Block(Block { stmts, .. }) => {
                    stack.extend(stmts);
                }
                Node::BuiltinType(_) => {}
                Node::Return(Some(x)) => {
                    stack.push_back(*x);
                }
                Node::ImplicitReturn(x) => {
                    stack.push_back(*x);
                }
                Node::VarStmt(VarStmt {
                    name: _,
                    type_defn,
                    init_expr,
                }) => {
                    if let Some(type_defn) = type_defn {
                        stack.push_back(*type_defn);
                    }
                    stack.push_back(*init_expr);
                }
            }
        }

        deps
    }

    /// Returns a set containing the id of all
    /// declarations that need to have bytecode generated
    /// before this declaration.
    pub fn get_build_deps(&self, ast: &Ast) -> HashSet<usize> {
        let mut deps = HashSet::new();
        ast.visit(self.root_expr, |index| match &ast.nodes[index] {
            Node::Ident(name) => {
                if let Some(&id) = ast.members.get(name) {
                    deps.insert(id);
                }
            }
            _ => {}
        });
        deps
    }
}

pub struct Ast {
    pub nodes: Vec<Node>,
    pub decls: Vec<Decl>,
    pub members: HashMap<String, usize>,
}

impl Ast {
    pub fn new() -> Self {
        Self {
            nodes: Vec::new(),
            decls: Vec::new(),
            members: HashMap::new(),
        }
    }

    /// Add a new Node to the tree and return its id.
    pub fn append(&mut self, node: Node) -> NodeId {
        let id = self.nodes.len();
        self.nodes.push(node);
        id
    }

    /// Add a new Decl to the tree and return its id.
    pub fn declare(&mut self, mut decl: Decl) -> usize {
        let id = self.decls.len();
        let name = decl.name.clone();
        decl.nodes.clear();
        self.visit(decl.root_expr, |node_id| decl.nodes.push(node_id));
        self.decls.push(decl);
        self.members.insert(name, id);
        id
    }

    /// Calls the function `f` with each child node of a tree.
    pub fn visit<F>(&self, start: NodeId, mut visitor: F)
    where
        F: FnMut(NodeId),
    {
        let mut stack = vec![start];
        let mut visited = HashSet::new();

        while let Some(index) = stack.last().cloned() {
            if visited.contains(&index) {
                stack.pop();
                visitor(index);
                continue;
            }

            visited.insert(index);

            if let Some(node) = self.nodes.get(index) {
                match node {
                    Node::Ident(_)
                    | Node::Number(_)
                    | Node::BuiltinType(_)
                    | Node::Return(None) => {
                        // Leaf nodes, visit immediately
                        stack.pop();
                        visitor(index);
                    }
                    Node::Binary(Binary { lhs, rhs, .. }) => {
                        stack.push(*lhs);
                        stack.push(*rhs);
                    }
                    Node::Unary(Unary { inner, .. }) => {
                        stack.push(*inner);
                    }
                    Node::Call(CallExpr { proc, args }) => {
                        stack.push(*proc);
                        stack.extend(args);
                    }
                    Node::FnProto(FnProto { params, result }) => {
                        for (_, &type_defn) in params {
                            stack.push(type_defn);
                        }
                        if let Some(result) = result {
                            stack.push(*result);
                        }
                    }
                    Node::FnLiteral(FnLiteral { type_defn, block }) => {
                        stack.push(*type_defn);
                        stack.push(*block);
                    }
                    Node::Block(Block { stmts, .. }) => {
                        stack.extend(stmts);
                    }
                    Node::Return(Some(x)) => {
                        stack.push(*x);
                    }
                    Node::ImplicitReturn(x) => {
                        stack.push(*x);
                    }
                    Node::VarStmt(VarStmt {
                        name: _,
                        type_defn,
                        init_expr,
                    }) => {
                        if let Some(type_defn) = type_defn {
                            stack.push(*type_defn);
                        }
                        stack.push(*init_expr);
                    }
                }
            }
        }
    }

    pub fn display(&self, index: NodeId) -> String {
        if let Some(node) = self.nodes.get(index) {
            match node {
                Node::Ident(name) => name.clone(),
                Node::Number(value) => value.to_string(),
                Node::Binary(bin) => {
                    let lhs = self.display(bin.lhs);
                    let rhs = self.display(bin.rhs);
                    format!("{} {} {}", lhs, bin.op, rhs)
                }
                Node::Unary(unary) => {
                    let inner = self.display(unary.inner);
                    format!("{} {}", unary.op, inner)
                }
                Node::Call(CallExpr { proc, args }) => {
                    let proc = self.display(*proc);
                    let args = args
                        .iter()
                        .map(|&arg| self.display(arg))
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("{}({})", proc, args)
                }
                Node::FnProto(FnProto { params, result }) => {
                    let params = params
                        .iter()
                        .map(|(name, &type_defn)| format!("{}: {}", name, self.display(type_defn)))
                        .collect::<Vec<_>>()
                        .join(", ");
                    let result = if let Some(result) = result {
                        self.display(*result)
                    } else {
                        "void".into()
                    };
                    format!("({}) -> {}", params, result)
                }
                Node::FnLiteral(FnLiteral { type_defn, block }) => {
                    let type_defn = self.display(*type_defn);
                    let block = self.display(*block);
                    format!("{} {}", type_defn, block)
                }
                Node::Block(Block { stmts, .. }) => {
                    let stmts = stmts
                        .iter()
                        .map(|&x| self.display(x))
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("{{ {} }}", stmts)
                }
                Node::BuiltinType(builtin) => format!("{}", builtin),
                Node::Return(Some(result)) => format!("return {}", self.display(*result)),
                Node::ImplicitReturn(result) => format!("return {}", self.display(*result)),
                Node::Return(None) => "return".into(),
                Node::VarStmt(VarStmt {
                    name,
                    type_defn,
                    init_expr,
                }) => {
                    let init_expr = self.display(*init_expr);
                    if let Some(type_defn) = type_defn {
                        let type_defn = self.display(*type_defn);
                        format!("{name}: {type_defn} = {init_expr}")
                    } else {
                        format!("{name} := {init_expr}")
                    }
                }
            }
        } else {
            format!("(Invalid index: {})", index)
        }
    }
}

/// Performs topological sort
pub fn topological_sort<F>(decls: &[Decl], extractor: F) -> Vec<usize>
where
    F: Fn(&Decl) -> HashSet<usize>,
{
    let num_decls = decls.len();
    let mut graph = HashMap::new();
    let mut in_degree = vec![0; num_decls];

    // Build the graph structure
    for (index, decl) in decls.iter().enumerate() {
        for &dep_index in &extractor(decl) {
            graph.entry(dep_index).or_insert_with(Vec::new).push(index);
            in_degree[index] += 1;
        }
    }

    // Find all items with no dependencies to start with
    let mut queue = VecDeque::new();
    for (index, &degree) in in_degree.iter().enumerate() {
        if degree == 0 {
            queue.push_back(index);
        }
    }

    // Perform topological sort
    let mut sorted_indices = Vec::new();
    while let Some(index) = queue.pop_front() {
        if let Some(deps) = graph.get(&index) {
            for &dep_index in deps {
                in_degree[dep_index] -= 1;
                if in_degree[dep_index] == 0 {
                    queue.push_back(dep_index);
                }
            }
        }
        sorted_indices.push(index);
    }

    sorted_indices
}

impl fmt::Display for Oper {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Oper::Add => write!(f, "+"),
            Oper::Sub => write!(f, "-"),
            Oper::Mul => write!(f, "*"),
            Oper::Div => write!(f, "/"),
            Oper::Rem => write!(f, "%"),

            Oper::Eql => write!(f, "=="),
            Oper::Neq => write!(f, "!="),
            Oper::Gtr => write!(f, ">"),
            Oper::Lss => write!(f, "<"),
            Oper::Geq => write!(f, ">="),
            Oper::Leq => write!(f, "<="),

            Oper::And => write!(f, "&"),
            Oper::Or => write!(f, "|"),
            Oper::Xor => write!(f, "^"),
            Oper::Shl => write!(f, "<<"),
            Oper::Shr => write!(f, ">>"),

            Oper::LogicAnd => write!(f, "and"),
            Oper::LogicOr => write!(f, "or"),
        }
    }
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnaryOp::BitNot => write!(f, "~"),
            UnaryOp::Negation => write!(f, "-"),
            UnaryOp::AddressOf => write!(f, "*"),
            UnaryOp::LogicNot => write!(f, "!"),
        }
    }
}
