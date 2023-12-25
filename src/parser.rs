use crate::ast::*;
use crate::lexer::{Lexer, Loc, Token};
use crate::types::BuiltinType;
use hashbrown::HashMap;
use std::result;

pub struct Parser {
    index: usize,
    tokens: Vec<Token>,
    locs: Vec<Loc>,
    block: Block,
    ast: Ast,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let (tokens, locs) = lexer.unzip();
        Self {
            index: 0,
            tokens,
            locs,
            block: Block::new(None, Vec::new(), HashMap::new()),
            ast: Ast::new(),
        }
    }

    /// Constructs an instance of `Parser` and parses a list of top-level statements,
    /// returning the generated abstract syntax tree.
    pub fn parse(lexer: Lexer) -> Result<Ast> {
        let mut p = Parser::new(lexer);
        while p.index < p.tokens.len() {
            p.parse_stmt_or_decl()?;
        }
        Ok(p.ast)
    }

    pub fn parse_expr(&mut self) -> Result<NodeId> {
        self.parse_binary_op(0)
    }

    /// Parses a binary operation from tokens, taking into account the operator precedence.
    /// Continues parsing as long as the precedence of the operator is equal to or higher than `min_prec`.
    pub fn parse_binary_op(&mut self, min_prec: i32) -> Result<NodeId> {
        // Initial parsing of the left-hand side of the expression.
        let mut node = self.parse_prefix_expr()?;

        while let Some((prec, oper)) = operator_precedence(&self.peek_token()) {
            if prec < min_prec {
                break;
            }

            // Move to the next token and parse the right-hand side of the expression.
            self.index += 1;
            let rhs = self.parse_binary_op(prec + 1)?;

            node = self.ast.append(Node::Binary(Binary::new(oper, node, rhs)));
        }

        Ok(node)
    }

    pub fn parse_prefix_expr(&mut self) -> Result<NodeId> {
        let unary_op = match self.peek_token() {
            // Token::Tidle => UnaryOp::BitNot,
            Token::Minus => UnaryOp::Negation,
            Token::Asterisk => UnaryOp::AddressOf,
            Token::Bang => UnaryOp::LogicNot,
            _ => return self.parse_suffix_expr(),
        };
        self.index += 1;
        let inner = self.parse_prefix_expr()?;
        Ok(self.ast.append(Node::Unary(Unary::new(unary_op, inner))))
    }

    /// Parses a primary expression followed by '.', '(', '[', or '{' tokens.
    pub fn parse_suffix_expr(&mut self) -> Result<NodeId> {
        let mut node = self.parse_primary_expr()?;

        loop {
            match self.peek_token() {
                Token::Period => todo!(),
                Token::OpenParen => node = self.parse_call_expr(node)?,
                Token::OpenBracket => node = self.parse_array_index_expr(node)?,
                // Token::OpenBrace => node = self.parse_struct_literal_expr(node)?,
                _ => break,
            }
        }

        Ok(node)
    }

    /// A call expression is an expression followed by a list of arguments
    /// separated by commas and surrounded by parenthesis.
    pub fn parse_call_expr(&mut self, lhs: NodeId) -> Result<NodeId> {
        let start = self.index;
        self.index += 1;
        debug_assert!(self.tokens[start].is_open_paren());

        // Parse the arguments to the procedure call.
        let mut args = Vec::new();
        loop {
            if self.consume_token(Token::CloseParen) {
                break;
            }

            args.push(self.parse_expr()?);

            match self.peek_token() {
                // If there is a comma, continue parsing arguments.
                Token::Comma => self.index += 1,
                Token::CloseParen => {
                    self.index += 1;
                    break;
                }
                // Otherwise we must have a closing paren.
                _ => {
                    return Err(ParseError::UnclosedCall {
                        call_start: self.locs[start].clone(),
                        call_end: self.locs[self.index].clone(),
                    })
                }
            }
        }

        Ok(self.ast.append(Node::Call(CallExpr::new(lhs, args))))
    }

    /// An array index expression is an expression followed by another
    /// expression wrapped in square brackets. Slice operations `a[b:c]`
    /// are also handled by the parser here, because `b:c` is not a valid
    /// expression on its own.
    pub fn parse_array_index_expr(&mut self, _lhs: NodeId) -> Result<NodeId> {
        let start = self.index;
        self.index += 1;
        debug_assert!(self.tokens[start].is_open_bracket());

        todo!()
    }

    pub fn parse_struct_literal_expr(&mut self, _lhs: NodeId) -> Result<NodeId> {
        let start = self.index;
        self.index += 1;
        debug_assert!(self.tokens[start].is_open_brace());

        todo!()
    }

    pub fn parse_primary_expr(&mut self) -> Result<NodeId> {
        match self.peek_token() {
            Token::Ident(name) => {
                self.index += 1;
                let node = name
                    .parse::<BuiltinType>()
                    .map(Node::BuiltinType)
                    .unwrap_or_else(|_| Node::Ident(name.clone()));
                Ok(self.ast.append(node))
            }
            Token::IntLit(x) => {
                self.index += 1;
                Ok(self.ast.append(Node::Number(x.clone())))
            }
            Token::StrLit(_) => todo!(),
            Token::FloatLit(_) => todo!(),
            Token::OpenBrace => self.parse_block(),
            Token::OpenParen => match (self.peek_by(1), self.peek_by(2)) {
                // An empty paren group or a parameter declaration means we
                // are the start of a procedure type or literal.
                (Token::CloseParen, _) | (Token::Ident(_), Token::Colon) => {
                    self.parse_fn_type_or_lit()
                }
                _ => {
                    let group_start = self.index;
                    self.index += 1;
                    let inner = self.parse_expr()?;
                    if !self.consume_token(Token::CloseParen) {
                        return Err(ParseError::UnclosedGroupExpr {
                            group_start: self.locs[group_start].clone(),
                            group_end: self.locs[self.index].clone(),
                        });
                    }
                    Ok(inner)
                }
            },
            Token::EndOfFile => Err(ParseError::ExprButEof {
                loc: self.locs.last().cloned(),
            }),
            tok => todo!("{:?}", tok),
        }
    }

    /// Parses a function type signature and then an optional body if the
    /// next token is a `{` token, returns either `Token::FnProto` or `Token::FnLiteral`.
    pub fn parse_fn_type_or_lit(&mut self) -> Result<NodeId> {
        let start = self.index;
        self.index += 1;
        debug_assert_eq!(self.tokens[start], Token::OpenParen);

        let mut params = HashMap::new();
        loop {
            if self.consume_token(Token::CloseParen) {
                break;
            }

            // Parse the next parameter and check for duplicates.
            let (param_name, type_defn) = self.parse_param_decl()?;

            if !insert_checked(&mut params, param_name.clone(), type_defn) {
                return Err(ParseError::ReusedParamName { param_name });
            }

            // If we see a comma or close paren, continue parsing, otherwise,
            // we expect the end of the parameter list.
            match self.peek_token() {
                Token::Comma => self.index += 1,
                Token::CloseParen => {
                    self.index += 1;
                    break;
                }
                _ => {
                    return Err(ParseError::UnclosedFnType {
                        fn_start: self.locs[start].clone(),
                        fn_end: self.locs[self.index].clone(),
                    })
                }
            }
        }

        // If we see an arrow token (->) then we parse the optional return type
        // of the procedure type definition.
        let result = if self.consume_token(Token::Arrow) {
            Some(self.parse_expr()?)
        } else {
            None
        };

        let fn_type = self.ast.append(Node::FnProto(FnProto::new(params, result)));

        // Check if we are a definition or just a type.
        if self.peek_token().is_open_brace() {
            let block = self.parse_block()?;
            Ok(self
                .ast
                .append(Node::FnLiteral(FnLiteral::new(fn_type, block))))
        } else {
            Ok(fn_type)
        }
    }

    /// A statement is usually a single line or block of a procedure.
    /// Expressions can be used as statements to perform side effects.
    pub fn parse_stmt(&mut self) -> Result<NodeId> {
        match self.peek_token() {
            Token::If => todo!(),
            Token::Then | Token::Else => {
                Err(ParseError::UnexpectedKeywordStmt {
                    loc: self.locs[self.index].clone(),
                    token: self.tokens[self.index].clone(),
                    // last_stmt: self.cur_block.stmts.last(),
                    last_stmt: None,
                })
            }
            Token::For => todo!(),
            Token::While => todo!(),
            Token::Return => {
                self.index += 1;
                let result = match self.peek_token() {
                    Token::Semicolon => None,
                    _ => Some(self.parse_expr()?),
                };
                Ok(self.ast.append(Node::Return(result)))
            }
            _ => self.parse_expr(),
        }
    }

    /// A wrapper function to handle the fact that constant declarations are
    /// not actually syntax tree nodes, and get stored in a separate array in the
    /// AST, to avoid the `parse_stmt()` function having to return Option<NodeId>
    /// for when we parse a declaration and don't have anything to append to the
    /// block's list of statements.
    pub fn parse_stmt_or_decl(&mut self) -> Result<()> {
        match self.peek_token() {
            Token::Ident(_) if self.peek_by(1) == Token::Colon => {
                // Constant declarations don't get added to the ast,
                // but variables do as they have bytecode generated for them.
                if let Some(var) = self.parse_decl()? {
                    // Add the variable statement to the block.
                    self.block.stmts.push(var);
                }
            }
            _ => {
                // Parse the next statement and add it to the block.
                let stmt = self.parse_stmt()?;
                self.block.stmts.push(stmt);
            }
        }
        Ok(())
    }

    pub fn parse_block(&mut self) -> Result<NodeId> {
        let start = self.index;
        self.index += 1;
        debug_assert_eq!(self.tokens[start], Token::OpenBrace);

        // Save the current block as the parent and create a new block.
        let parent_block = std::mem::replace(&mut self.block, Block::default());
        self.block.parent = Some(Box::new(parent_block));

        loop {
            if self.consume_token(Token::CloseBrace) {
                break;
            }

            // Parse the next statement in the block.
            let stmt_start = self.locs[self.index].clone();
            self.parse_stmt_or_decl()?;

            match self.peek_token() {
                Token::Semicolon => self.index += 1,
                // Reached the end of the input without closing the block.
                Token::EndOfFile => {
                    return Err(ParseError::UnclosedBlock {
                        block_start: self.locs[start].clone(),
                        block_end: self.locs.last().cloned().unwrap(),
                    })
                }
                _ => {
                    let needs_semi = !self.tokens[self.index - 1].is_close_brace();
                    if needs_semi {
                        // Implicit return statement
                        if self.consume_token(Token::CloseBrace) {
                            let last = self.block.stmts.last_mut().unwrap();
                            *last = self.ast.append(Node::ImplicitReturn(*last));
                            break;
                        }
                        return Err(ParseError::SemiAfterStmt {
                            stmt_start,
                            stmt_end: self.locs[self.index].clone(),
                        });
                    }
                }
            }
        }

        // Extract the parent from the current block and restore it as self.block.
        let parent_block = *self.block.parent.take().unwrap();
        let current_block = std::mem::replace(&mut self.block, parent_block);

        Ok(self.ast.append(Node::Block(current_block)))
    }

    pub fn parse_decl(&mut self) -> Result<Option<NodeId>> {
        let ident = self.index;
        self.index += 1;
        let name = self.tokens[ident].clone().unwrap_ident().clone();

        let colon = self.index;
        self.index += 1;
        debug_assert_eq!(self.tokens[colon], Token::Colon);

        let type_defn = match &self.tokens[self.index] {
            Token::Colon | Token::Equals => None,
            _ => Some(self.parse_expr()?),
        };

        match self.peek_token() {
            Token::Colon => {
                self.index += 1;
                let root_expr = self.parse_expr()?;
                self.ast.declare(Decl::new(name, type_defn, root_expr));
                Ok(None)
            }
            Token::Equals => {
                self.index += 1;
                let init_expr = self.parse_expr()?;
                let var = self
                    .ast
                    .append(Node::VarStmt(VarStmt::new(name, type_defn, init_expr)));
                Ok(Some(var))
            }
            _ => Err(ParseError::InvalidDeclStart {
                decl_start: self.locs[ident].clone(),
                decl_end: self.locs[self.index].clone(),
                token: self.tokens[self.index].clone(),
            }),
        }
    }

    /// Parses a parameter declaration in the type signature of a procedure
    /// literal like `x: int` or `items: [..]$T`.
    pub fn parse_param_decl(&mut self) -> Result<(String, NodeId)> {
        let ident = self.eat_ident().ok_or(ParseError::MissingParamName {
            token: self.tokens[self.index].clone(),
            loc: self.locs[self.index].clone(),
        })?;
        if !self.consume_token(Token::Colon) {
            return Err(ParseError::ColonAfterParamName {
                token: self.tokens[self.index].clone(),
                loc: self.locs[self.index].clone(),
            });
        }
        let type_defn = self.parse_expr()?;
        Ok((ident, type_defn))
    }

    /// Consumes the next token if it equals the provided token.
    pub fn consume_token(&mut self, expect: Token) -> bool {
        if self.index < self.tokens.len() && self.tokens[self.index] == expect {
            self.index += 1;
            true
        } else {
            false
        }
    }

    /// Consumes the next token if it is an identifier.
    pub fn eat_ident(&mut self) -> Option<String> {
        match self.tokens.get(self.index) {
            Some(Token::Ident(name)) => Some(name.clone()),
            _ => None,
        }
    }

    /// Peeks the next token, returning `Token::EndOfFile` on overflow.
    pub fn peek_token(&self) -> Token {
        self.tokens
            .get(self.index)
            .cloned()
            .unwrap_or(Token::EndOfFile)
    }

    /// Peeks ahead by `offset` number of tokens, following the behavior
    /// of `peek_token` on overflow.
    pub fn peek_by(&mut self, offset: usize) -> Token {
        self.tokens
            .get(self.index + offset)
            .cloned()
            .unwrap_or(Token::EndOfFile)
    }
}

/// Returns the precedence of the token if it is an operator, or else returns `None`.
pub fn operator_precedence(token: &Token) -> Option<(i32, Oper)> {
    use Token::*;
    match token {
        PipePipe => Some((10, Oper::LogicOr)),
        AmpersandAmpersand => Some((20, Oper::LogicAnd)),
        EqualsEquals => Some((30, Oper::Eql)),
        BangEquals => Some((30, Oper::Neq)),
        Ampersand => Some((40, Oper::And)),
        Caret => Some((40, Oper::Xor)),
        Pipe => Some((40, Oper::Or)),
        // TODO: bit shift operators
        Plus => Some((60, Oper::Add)),
        Minus => Some((60, Oper::Sub)),
        Asterisk => Some((70, Oper::Mul)),
        Slash => Some((70, Oper::Div)),
        Percent => Some((70, Oper::Rem)),
        _ => None,
    }
}

pub type Result<T> = result::Result<T, ParseError>;

/// An error encountered while parsing a list of tokens into
/// an abstract syntax tree with debug and location information
/// to generate a proper compiler error message.
#[derive(Debug)]
pub enum ParseError {
    /// We parsed to the end of the file without encountering a
    /// closing brace, most likely a nested block was not closed.
    UnclosedBlock {
        block_start: Loc,
        block_end: Loc,
    },
    /// No closing parenthesis was found after the last expression
    /// in the procedure call argument list, possibly missing a
    /// comma or closing paren?
    UnclosedCall {
        call_start: Loc,
        call_end: Loc,
    },
    /// No closing parenthesis was found after the end of a parenthesized
    /// expression, either the expression is incorrect or a closing parenthesis
    /// missing afterwards.
    UnclosedGroupExpr {
        group_start: Loc,
        group_end: Loc, // Position of erroneous token
    },
    ExprButEof {
        // Optional because file might be empty
        loc: Option<Loc>,
    },
    SemiAfterStmt {
        stmt_start: Loc,
        stmt_end: Loc,
    },
    /// An unexpected keyword was encountered where we expected
    /// a statement or declaration, maybe the previous statement
    /// was unfinished?
    UnexpectedKeywordStmt {
        loc: Loc,
        token: Token,
        last_stmt: Option<NodeId>,
    },
    InvalidDeclStart {
        decl_start: Loc,
        decl_end: Loc, // Position of erroneous token
        token: Token,
    },
    /// We expected an identifier to begin a parameter declaration
    /// for a procedure literal type but we got something else instead.
    MissingParamName {
        token: Token,
        loc: Loc,
    },
    /// We parsed an identifier starting a parameter declaration, so
    /// we expected to see a `:` token immediately following it.
    ColonAfterParamName {
        token: Token,
        loc: Loc,
    },
    /// A parameter name was used twice, for example: `calc :: (x: int, x: float) -> ...`.
    ReusedParamName {
        param_name: String,
    },
    /// Here we expected a `,` to continue parsing parameters or else
    /// a closing parenthesis to mark the end of the list.
    UnclosedFnType {
        fn_start: Loc,
        fn_end: Loc,
    },
}

/// Inserts an element into a HashMap if it does not exist, returning whether
/// or not the item was successfully (uniquely) inserted.
///
/// For example:
/// ```
/// let mut map = HashMap::new();
/// assert_eq!(insert_checked(&mut map, "John", 34.35), true);
/// assert_eq!(insert_checked(&mut map, "John", 69.11), false); // Key "John" already exists
/// ```
fn insert_checked<K, V>(map: &mut HashMap<K, V>, key: K, value: V) -> bool
where
    K: Eq + std::hash::Hash,
    V: Eq,
{
    match map.entry(key) {
        hashbrown::hash_map::Entry::Occupied(_) => false, // Key exists, so don't insert
        hashbrown::hash_map::Entry::Vacant(e) => {
            e.insert(value); // Key does not exist, perform the insert
            true
        }
    }
}
