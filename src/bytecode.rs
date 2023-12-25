use crate::ast::*;
use crate::constant::Const;
use crate::infer::Infer;
use derive_more::Constructor;
use num_traits::ToPrimitive;
use std::collections::HashMap;
use std::fmt;

pub type Word = u64;

#[derive(Copy, Clone)]
#[repr(u8)]
pub enum Opcode {
    /// Copy a value between registers
    Move,
    /// Fill a register with zeroes
    Zero,
    /// Load a constant into a register
    Constant,
    /// Addition operator
    Add,
    /// Subtraction operator
    Sub,
    /// Multiplication operator
    Mul,
    /// Remainder (modulus) operator
    Rem,
    /// Exponentation operator
    Pow,
    /// Vivision operator
    Div,
    /// Floating-point division operator
    FDiv,
    /// Bit-wise AND operator
    BitAnd,
    /// Bit-wise OR operator
    BitOr,
    /// Bit-wise Exclusive OR operator
    BitXor,
    /// Shift bits left
    Shl,
    /// Shift bits right
    Shr,
    /// Unary minus (negation)
    Neg,
    /// Bit-wise NOT operator
    BitNot,
    /// Logical NOT operator
    Not,
    /// Unconditional jump
    Jmp,
    /// Equality test, with conditional jump
    Eql,
    /// Less than test, with conditional jump
    Lss,
    /// Less than or equal to test, with conditional jump
    Leq,
    /// Boolean test, with conditional jump
    Test,
    /// Call an address
    Call,
    /// Return from procedure call
    Return,
    /// Prints the value of a register
    Debug,
    /// Terminates the program
    Exit,
}

#[derive(Copy, Clone, Constructor)]
#[repr(packed)]
pub struct Inst {
    code: Opcode,
    a: u8,
    b: u8,
    c: u8,
}

pub struct Interp {
    pub program: Vec<Inst>,
    pub constants: Vec<Word>,
    pub registers: HashMap<u8, Word>,
    pub labels: HashMap<String, Label>,
}

#[derive(Debug, Copy, Clone)]
pub enum Label {
    Constant(usize),
    Address(u16),
}

impl Interp {
    pub fn new(program: Vec<Inst>) -> Self {
        Self {
            program,
            constants: Vec::new(),
            registers: HashMap::new(),
            labels: HashMap::new(),
        }
    }

    pub fn interp(program: Vec<Inst>) {
        let mut interp = Self::new(program);
        interp.run();
    }

    #[inline]
    pub fn register(&self, index: u8) -> Word {
        self.registers.get(&index).cloned().unwrap_or_default()
    }

    #[inline]
    pub fn constant(&self, index: u8) -> Word {
        self.constants
            .get(index as usize)
            .cloned()
            .unwrap_or_default()
    }

    pub fn run(&mut self) {
        let mut pc = 0;
        let mut return_stack = Vec::new();
        while let Some(inst) = self.program.get(pc) {
            use Opcode::*;
            match inst.code {
                Move => {
                    // R(A) := R(B)
                    let b = self.register(inst.b);
                    self.registers.insert(inst.a, b);
                    pc += 1;
                }
                Zero => {
                    // R(A), R(A+1), ..., R(A+B) := nil
                    let start = inst.a;
                    let count = inst.b;
                    for index in start..start + count {
                        self.registers.insert(index, 0);
                    }
                    pc += 1;
                }
                Constant => {
                    // R(A) := Kst(B)
                    let constant = self.constant(inst.b);
                    self.registers.insert(inst.a, constant);
                    pc += 1;
                }
                Add => {
                    // R(C) := R(A) + R(B)
                    let a = self.register(inst.a);
                    let b = self.register(inst.b);
                    self.registers.insert(inst.c, a + b);
                    pc += 1;
                }
                Sub => {
                    // R(C) := R(A) - R(B)
                    let a = self.register(inst.a);
                    let b = self.register(inst.b);
                    self.registers.insert(inst.c, a - b);
                    pc += 1;
                }
                Mul => {
                    // R(C) := R(A) * R(B)
                    let a = self.register(inst.a);
                    let b = self.register(inst.b);
                    self.registers.insert(inst.c, a * b);
                    pc += 1;
                }
                Rem => {
                    // R(C) := R(A) % R(B)
                    let a = self.register(inst.a);
                    let b = self.register(inst.b);
                    self.registers.insert(inst.c, a % b);
                    pc += 1;
                }
                Pow => {
                    todo!()
                }
                Div => {
                    // R(C) := R(A) / R(B)
                    let a = self.register(inst.a);
                    let b = self.register(inst.b);
                    self.registers.insert(inst.c, a / b);
                    pc += 1;
                }
                FDiv => {
                    // R(C) := R(A) / R(B)
                    let a: f64 = unsafe { std::mem::transmute(self.register(inst.a)) };
                    let b: f64 = unsafe { std::mem::transmute(self.register(inst.b)) };
                    let c: Word = unsafe { std::mem::transmute(a / b) };
                    self.registers.insert(inst.c, c);
                    pc += 1;
                }
                BitAnd => {
                    // R(C) := R(A) & R(B)
                    let a = self.register(inst.a);
                    let b = self.register(inst.b);
                    self.registers.insert(inst.c, a & b);
                    pc += 1;
                }
                BitOr => {
                    // R(C) := R(A) & R(B)
                    let a = self.register(inst.a);
                    let b = self.register(inst.b);
                    self.registers.insert(inst.c, a | b);
                    pc += 1;
                }
                BitXor => {
                    // R(C) := R(A) & R(B)
                    let a = self.register(inst.a);
                    let b = self.register(inst.b);
                    self.registers.insert(inst.c, a ^ b);
                    pc += 1;
                }
                Shl => {
                    // R(C) := R(A) & R(B)
                    let a = self.register(inst.a);
                    let b = self.register(inst.b);
                    self.registers.insert(inst.c, a << b);
                    pc += 1;
                }
                Shr => {
                    // R(C) := R(A) & R(B)
                    let a = self.register(inst.a);
                    let b = self.register(inst.b);
                    self.registers.insert(inst.c, a >> b);
                    pc += 1;
                }
                Neg => {
                    // R(A) := -R(B)
                    let b = self.register(inst.b);
                    self.registers.insert(inst.a, (!b) + 1);
                    pc += 1;
                }
                BitNot => {
                    // R(A) := ~R(B)
                    let b = self.register(inst.b);
                    self.registers.insert(inst.a, !b);
                    pc += 1;
                }
                Not => {
                    // R(A) := not R(B)
                    let b = self.register(inst.b);
                    self.registers.insert(inst.a, (b == 0) as Word);
                    pc += 1;
                }
                Jmp => {
                    // if (A) pc += Bx; else pc -= Bx
                    let bx = ((inst.b as u16) << 8) | inst.c as u16;
                    if inst.a != 0 {
                        pc += bx as usize;
                    } else {
                        pc -= bx as usize;
                    }
                }
                Eql => {
                    // if (((R(B) == R(C)) != A) pc++
                    let a = self.register(inst.a);
                    let b = self.register(inst.b);
                    let c = self.register(inst.c);
                    if (b == c) != (a != 0) {
                        pc += 1;
                    }
                    pc += 1;
                }
                Lss => {
                    // if (((R(B) < R(C)) != A) pc++
                    let a = self.register(inst.a);
                    let b = self.register(inst.b);
                    let c = self.register(inst.c);
                    if (b < c) != (a != 0) {
                        pc += 1;
                    }
                    pc += 1;
                }
                Leq => {
                    // if (((R(B) < R(C)) != A) pc++
                    let a = self.register(inst.a);
                    let b = self.register(inst.b);
                    let c = self.register(inst.c);
                    if (b <= c) != (a != 0) {
                        pc += 1;
                    }
                    pc += 1;
                }
                Test => {
                    // if (R(A) != (
                    todo!()
                }
                Call => {
                    return_stack.push(pc + 1);

                    let bx = ((inst.b as u16) << 8) | inst.c as u16;
                    pc = bx as usize;
                }
                Return => {
                    pc = return_stack.pop().unwrap();
                }
                Debug => {
                    println!("{}", self.register(inst.a));
                    pc += 1;
                }
                Exit => {
                    break;
                }
            }
        }
    }

    pub fn add_decls(&mut self, infer: &Infer) {
        for decl in &infer.ast.decls {
            let node = infer.ast.nodes[decl.root_expr].clone();
            match node {
                Node::FnLiteral(fn_lit) => {
                    let addr = self.program.len() as u16;
                    self.add_instructions(infer.ast, fn_lit.block, 0);
                    self.labels.insert(decl.name.clone(), Label::Address(addr));
                }
                _ => {
                    if let Some(constant) = infer.constants.get(&decl.root_expr) {
                        let id = self.add_constant(constant);
                        self.labels.insert(decl.name.clone(), Label::Constant(id));
                    } else {
                        todo!()
                    }
                }
            }
        }
    }

    pub fn add_constant(&mut self, constant: &Const) -> usize {
        let id = self.constants.len();
        let word = match constant {
            Const::Int(x) => x.to_u64().unwrap(),
            Const::Float(x) => unsafe { std::mem::transmute(x) },
            Const::Bool(x) => *x as u64,
            Const::Struct(_) => todo!(),
            Const::Type(x) => *x as u64,
        };
        self.constants.push(word);
        id
    }

    pub fn add_instructions(&mut self, ast: &Ast, node_index: NodeId, reg: u8) {
        match &ast.nodes[node_index] {
            Node::Ident(name) => {
                let label = self.labels.get(name).expect("Failed to get label");
                match label {
                    Label::Constant(id) => {
                        self.program
                            .push(Inst::new(Opcode::Constant, reg, *id as u8, 0))
                    }
                    Label::Address(_) => todo!(),
                }
            }
            Node::Number(x) => {
                let word = x.to_u64().unwrap();
                let id = self.constants.len();
                self.constants.push(word);
                self.program
                    .push(Inst::new(Opcode::Constant, reg, id as u8, 0));
            }
            Node::Binary(bin) => match bin.op {
                Oper::Add => {
                    self.add_instructions(ast, bin.lhs, reg);
                    self.add_instructions(ast, bin.rhs, reg + 1);
                    self.program.push(Inst::new(Opcode::Add, reg, reg + 1, reg));
                }
                Oper::Mul => {
                    self.add_instructions(ast, bin.lhs, reg);
                    self.add_instructions(ast, bin.rhs, reg + 1);
                    self.program.push(Inst::new(Opcode::Mul, reg, reg + 1, reg));
                }
                _ => todo!(),
            },
            Node::Unary(_) => todo!(),
            Node::Call(_) => todo!(),
            Node::FnProto(_) => todo!(),
            Node::FnLiteral(_) => todo!(),
            Node::Block(block) => {
                for &stmt in &block.stmts {
                    self.add_instructions(ast, stmt, 0);
                }
            }
            Node::BuiltinType(_) => todo!(),
            Node::Return(result) => {
                if let Some(result) = result {
                    self.add_instructions(ast, *result, reg);
                    self.program.push(Inst::new(Opcode::Return, reg, 0, 0));
                } else {
                    self.program.push(Inst::new(Opcode::Return, 0, 0, 0));
                }
            }
            Node::ImplicitReturn(result) => {
                self.add_instructions(ast, *result, reg);
                self.program.push(Inst::new(Opcode::Return, reg, 0, 0));
            }
            Node::VarStmt(_) => todo!(),
        }
    }
}

impl fmt::Display for Inst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Opcode::*;
        match self.code {
            Move => {
                write!(f, "MOVE {} {}", self.a, self.b)
            }
            Zero => {
                write!(f, "ZERO {} {}", self.a, self.b)
            }
            Constant => {
                write!(f, "CONSTANT {} {}", self.a, self.b)
            }
            Add => {
                write!(f, "ADD {} {} {}", self.a, self.b, self.c)
            }
            Sub => {
                write!(f, "SUB {} {} {}", self.a, self.b, self.c)
            }
            Mul => {
                write!(f, "MUL {} {} {}", self.a, self.b, self.c)
            }
            Rem => {
                write!(f, "REM {} {} {}", self.a, self.b, self.c)
            }
            Pow => {
                write!(f, "POW {} {} {}", self.a, self.b, self.c)
            }
            Div => {
                write!(f, "DIV {} {} {}", self.a, self.b, self.c)
            }
            FDiv => {
                write!(f, "FDIV {} {} {}", self.a, self.b, self.c)
            }
            BitAnd => {
                write!(f, "BITAND {} {} {}", self.a, self.b, self.c)
            }
            BitOr => {
                write!(f, "BITOR {} {} {}", self.a, self.b, self.c)
            }
            BitXor => {
                write!(f, "BITXOR {} {} {}", self.a, self.b, self.c)
            }
            Shl => {
                write!(f, "SHL {} {} {}", self.a, self.b, self.c)
            }
            Shr => {
                write!(f, "SHR {} {} {}", self.a, self.b, self.c)
            }
            Neg => {
                write!(f, "NEG {} {}", self.a, self.b)
            }
            BitNot => {
                write!(f, "BITNOT {} {}", self.a, self.b)
            }
            Not => {
                write!(f, "NOT {} {}", self.a, self.b)
            }
            Jmp => {
                write!(f, "JMP {} {}", self.a, self.b)
            }
            Eql => {
                write!(f, "EQL {} {} {}", self.a, self.b, self.c)
            }
            Lss => {
                write!(f, "LSS {} {} {}", self.a, self.b, self.c)
            }
            Leq => {
                write!(f, "LEQ {} {} {}", self.a, self.b, self.c)
            }
            Test => {
                write!(f, "TEST {} {} {}", self.a, self.b, self.c)
            }
            Call => {
                let bx = ((self.b as u16) << 8) | self.c as u16;
                write!(f, "CALL {}", bx)
            }
            Return => {
                write!(f, "RETURN")
            }
            Debug => {
                write!(f, "DEBUG {}", self.a)
            }
            Exit => {
                write!(f, "EXIT")
            }
        }
    }
}
