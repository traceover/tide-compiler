use std::collections::{HashMap, HashSet, VecDeque};

enum Node {
    Ident(String),
    Number(i64),
    Binary(Binary),
    FnProto(Vec<Param>, usize),
    FnLiteral(usize, usize),
    Block(Vec<usize>),
}

struct Binary {
    op: char,
    lhs: usize,
    rhs: usize,
}

impl Binary {
    fn new(op: char, lhs: usize, rhs: usize) -> Self {
        Self { op, lhs, rhs }
    }
}

struct Param {
    name: String,
    type_defn: usize,
}

impl Param {
    fn new(name: String, type_defn: usize) -> Self {
        Self { name, type_defn }
    }
}

struct Decl {
    name: String,
    type_defn: Option<usize>,
    root_expr: usize,
    nodes: Vec<usize>,
}

impl Decl {
    fn new(name: String, type_defn: Option<usize>, root_expr: usize) -> Self {
        Self {
            name,
            type_defn,
            root_expr,
            nodes: Vec::new(),
        }
    }

    #[inline]
    fn with_expr(name: String, root_expr: usize) -> Self {
        Self::new(name, None, root_expr)
    }

    /// Iterates all identifiers in the Decl's syntax tree
    /// and finds the corresponding entry, returning a list of
    /// Decl ids or an error if an identifier is not defined.
    fn resolve_idents(&self, ast: &Ast) -> Result<Vec<usize>, usize> {
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
    fn get_type_deps(&self, ast: &Ast) -> HashSet<usize> {
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
                Node::Number(_) => {}
                Node::Binary(bin) => {
                    stack.push_back(bin.lhs);
                    stack.push_back(bin.rhs);
                }
                Node::FnProto(params, result) => {
                    for param in params {
                        stack.push_back(param.type_defn);
                    }
                    stack.push_back(*result);
                }
                Node::FnLiteral(type_defn, _block) => {
                    stack.push_back(*type_defn);
                }
                Node::Block(stmts) => {
                    stack.extend(stmts);
                }
            }
        }

        deps
    }

    /// Returns a set containing the id of all
    /// declarations that need to have bytecode generated
    /// before this declaration.
    fn get_build_deps(&self, ast: &Ast) -> HashSet<usize> {
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

struct Ast {
    nodes: Vec<Node>,
    decls: Vec<Decl>,
    members: HashMap<String, usize>,
}

impl Ast {
    fn new() -> Self {
        Self {
            nodes: Vec::new(),
            decls: Vec::new(),
            members: HashMap::new(),
        }
    }

    /// Add a new Node to the tree and return its id.
    fn append(&mut self, node: Node) -> usize {
        let id = self.nodes.len();
        self.nodes.push(node);
        id
    }

    /// Add a new Decl to the tree and return its id.
    fn declare(&mut self, mut decl: Decl) -> usize {
        let id = self.decls.len();
        let name = decl.name.clone();
        decl.nodes.clear();
        self.visit(decl.root_expr, |node_id| decl.nodes.push(node_id));
        self.decls.push(decl);
        self.members.insert(name, id);
        id
    }

    /// Calls the function `f` with each child node of a tree.
    fn visit<F>(&self, start: usize, mut visitor: F)
    where
        F: FnMut(usize),
    {
        let mut stack = vec![start];
        while let Some(index) = stack.pop() {
            if let Some(node) = self.nodes.get(index) {
                match node {
                    Node::Ident(_) | Node::Number(_) => {}
                    Node::Binary(Binary { lhs, rhs, .. }) => {
                        stack.push(*lhs);
                        stack.push(*rhs);
                    }
                    Node::FnProto(params, result) => {
                        for param in params {
                            stack.push(param.type_defn);
                        }
                        stack.push(*result);
                    }
                    Node::FnLiteral(type_defn, block) => {
                        stack.push(*type_defn);
                        stack.push(*block);
                    }
                    Node::Block(stmts) => {
                        stack.extend(stmts);
                    }
                }
                visitor(index);
            }
        }
    }

    fn display(&self, index: usize) -> String {
        if let Some(node) = self.nodes.get(index) {
            match node {
                Node::Ident(name) => name.clone(),
                Node::Number(value) => value.to_string(),
                Node::Binary(bin) => {
                    let lhs = self.display(bin.lhs);
                    let rhs = self.display(bin.rhs);
                    format!("{} {} {}", lhs, bin.op, rhs)
                }
                Node::FnProto(params, result) => {
                    let params = params
                        .iter()
                        .map(|par| format!("{}: {}", par.name, self.display(par.type_defn)))
                        .collect::<Vec<_>>()
                        .join(", ");
                    let result = self.display(*result);
                    format!("({}) -> {}", params, result)
                }
                Node::FnLiteral(type_defn, block) => {
                    let type_defn = self.display(*type_defn);
                    let block = self.display(*block);
                    format!("{} {}", type_defn, block)
                }
                Node::Block(stmts) => {
                    let stmts = stmts
                        .iter()
                        .map(|&x| self.display(x))
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("{{ {} }}", stmts)
                }
            }
        } else {
            format!("(Invalid index: {})", index)
        }
    }
}

/// Performs topological sort
fn topological_sort<F>(decls: &[Decl], extractor: F) -> Vec<usize>
where
    F: Fn(&Decl) -> Vec<usize>,
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

fn main() {
    let mut ast = Ast::new();

    let defn = ast.append(Node::Number(69));
    let _ = ast.declare(Decl::with_expr("int".into(), defn));

    let num = ast.append(Node::Number(711));
    let _ = ast.declare(Decl::with_expr("ITEM".into(), num));

    let lhs = ast.append(Node::Ident("ITEM".into()));
    let rhs = ast.append(Node::Number(2));
    let bin = ast.append(Node::Binary(Binary::new('*', lhs, rhs)));

    let int_name = ast.append(Node::Ident("int".into()));
    let proto = ast.append(Node::FnProto(Vec::new(), int_name));
    let block = ast.append(Node::Block(vec![bin]));
    let fun = ast.append(Node::FnLiteral(proto, block));
    let _ = ast.declare(Decl::with_expr("fizz".into(), fun));

    let for_typing = topological_sort(&ast.decls, |decl| {
        decl.get_type_deps(&ast).into_iter().collect()
    });
    for decl_index in for_typing {
        println!("{}", ast.decls[decl_index].name);
    }

    /*
    for decl in &ast.decls {
        let deps = decl.get_type_deps(&ast);
        println!("{} :: {}", decl.name, ast.display(decl.root_expr));
        for dep in deps {
            println!("    {}", ast.decls[dep].name);
        }
    }
    */
}
