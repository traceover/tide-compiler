use crate::lexer::{Token, Loc};
use crate::ast::*;
use crate::types::BuiltinType;
use std::result;

pub struct Parser {
    index: usize,
    tokens: Vec<Token>,
    locs: Vec<Loc>,
    ast: Ast,
}

impl Parser {
    pub fn parse_expr(&mut self) -> Result<NodeId> {
        self.parse_binary_op(0)
    }

    /// Parses a binary operation from tokens, taking into account the operator precedence.
    /// Continues parsing as long as the precedence of the operator is equal to or higher than `min_prec`.
    pub fn parse_binary_op(&mut self, min_prec: i32) -> Result<NodeId> {
        // Initial parsing of the left-hand side of the expression.
        let mut node = self.parse_prefix_expr()?;

        while let Some((prec, oper)) = operator_precedence(&self.tokens[self.index]) {
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
        let unary_op = match &self.tokens[self.index] {
            // Token::Tidle => UnaryOp::BitNot,
            Token::Minus => UnaryOp::Negation,
            Token::Asterisk => UnaryOp::AddressOf,
            Token::Bang => UnaryOp::LogicNot,
            _ => return self.parse_primary_expr(),
        };
        self.index += 1;
        let inner = self.parse_prefix_expr()?;
        Ok(self.ast.append(Node::Unary(Unary::new(unary_op, inner))))
    }

    /// Parses a primary expression followed by '.', '(', '[', or '{' tokens.
    pub fn parse_suffix_expr(&mut self) -> Result<NodeId> {
        let mut node = self.parse_primary_expr()?;

        loop {
            match &self.tokens[self.index] {
                Token::Period => todo!(),
                Token::OpenParen => node = self.parse_call_expr(node)?,
                Token::OpenBracket => node = self.parse_array_index_expr(node)?,
                Token::OpenBrace => node = self.parse_struct_literal_expr(node)?,
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
        while self.index < self.tokens.len() {
            args.push(self.parse_expr()?);

            // If there is a comma, continue parsing arguments.
            if self.eat_token(Token::Comma).is_some() {
                continue;
            }

            // Otherwise we must have a closing paren.
            if self.eat_token(Token::CloseParen).is_none() {
                return Err(ParseError::UnclosedCall {
                    call_start: self.locs[start].clone(),
                    call_end: self.locs[self.index].clone(),
                });
            }
            break;
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
        let token = &self.tokens[self.index];
        match token {
            Token::Ident(name) => {
                let node = name.parse::<BuiltinType>().map(Node::BuiltinType).unwrap_or_else(|_| Node::Ident(name.clone()));
                Ok(self.ast.append(node))
            },
            Token::IntLit(x) => Ok(self.ast.append(Node::Number(x.clone()))),
            Token::StrLit(_) => todo!(),
            Token::FloatLit(_) => todo!(),
            Token::OpenBrace => self.parse_block(),
            _ => todo!(),
        }
    }

    /// A statement is usually a single line or block of a procedure.
    /// Expressions can be used as statements to perform side effects.
    /// Declarations are also parsed by parse_stmt but they do not contribute
    /// to the ast or appear in a block's list of statements.
    pub fn parse_stmt(&mut self) -> Result<NodeId> {
        let token = &self.tokens[self.index];
        match token {
            Token::If => todo!(),
            Token::Then | Token::Else => {
                Err(ParseError::UnexpectedKeywordStmt {
                    loc: self.locs[self.index].clone(),
                    token: token.clone(),
                    // last_stmt: self.cur_block.stmts.last(),
                    last_stmt: None,
                })
            },
            Token::For => todo!(),
            Token::While => todo!(),
            Token::Return => {
                self.index += 1;
                let result = match self.tokens[self.index] {
                    Token::Semicolon => None,
                    _ => Some(self.parse_expr()?),
                };
                Ok(self.ast.append(Node::Return(result)))
            },
            _ => self.parse_expr(),
        }
    }

    pub fn parse_block(&mut self) -> Result<NodeId> {
        let start = self.index;
        let mut stmts = Vec::new();

        while self.index < self.tokens.len() && !self.tokens[self.index].is_close_brace() {
            // Parse the next statement in the block.
            let stmt_start = self.locs[self.index].clone();
            stmts.push(self.parse_stmt()?);

            let needs_semi = !self.tokens[self.index - 1].is_close_brace();
            let has_semi = self.eat_token(Token::Semicolon).is_some();

            // A semicolon is not required after a block, but we consume it anyways.
            if needs_semi && !has_semi {
                if self.tokens[self.index].is_close_brace() {
                    let last = stmts.last_mut().unwrap();
                    *last = self.ast.append(Node::ImplicitReturn(*last));
                    break;
                }
                return Err(ParseError::SemiAfterStmt {
                    stmt_start,
                    stmt_end: self.locs[self.index].clone(),
                });
            }
        }

        // Reached the end of file without seeing '}' token.
        if self.eat_token(Token::CloseBrace).is_none() {
            return Err(ParseError::UnclosedBlock {
                block_start: self.locs[start].clone(),
                block_end: self.locs[self.index].clone(),
            });
        }

        Ok(self.ast.append(Node::Block(Block::new(stmts))))
    }

    /// Consumes the next token if it equals the provided token.
    pub fn eat_token(&mut self, expect: Token) -> Option<&Token> {
        let actual = &self.tokens[self.index];
        if *actual == expect {
            self.index += 1;
            Some(actual)
        } else {
            None
        }
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
}
