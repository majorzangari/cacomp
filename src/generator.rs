#![allow(dead_code)]
#![allow(unused_variables)]

use crate::ast::{BinaryOperator, Expression, Function, Program, Statement, UnaryOperator};
use crate::label_generator::LabelGenerator;
use std::{fmt, io};
use std::fs::File;
use std::io::Write;
// TODO: figure out non-void non-returning shit because C spec is dumb


pub struct Assembly {
    instr: Vec<String>,
}

impl Assembly {
    pub fn new(program: Program) -> Assembly {
        let gen = Generator::new(program);
        Assembly { instr: gen.instr }
    }

    pub fn write_to_file(&self, filename: &str) -> io::Result<()> {
        let mut file = File::create(filename)?;
        write!(file, "{}", self)?;
        Ok(())
    }
}

impl fmt::Display for Assembly {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for instr in &self.instr {
            // TODO: a little hacky
            if !instr.ends_with(":") {
                write!(f, "\t")?;
            }
            writeln!(f, "{}", instr)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub(crate) struct Generator {
    instr: Vec<String>,
    lg: LabelGenerator,
}

impl Generator {
    pub(crate) fn new(program: Program) -> Generator {
        let mut out = Generator { instr: Vec::new(), lg: LabelGenerator::new() };
        out.generate_instructions(program);
        out
    }

    fn generate_instructions(&mut self, program: Program) {
        // TODO: set up some docker shit for testing
        self.instr.push("section .text".to_string());
        self.instr.push("global _start".to_string());
        self.instr.push("_start:".to_string());
        self.instr.push("call main".to_string());
        self.instr.push("mov rdi, rax".to_string());
        self.instr.push("mov rax, 60".to_string());
        self.instr.push("syscall".to_string());
        for function in program.functions {
            self.lg.reset_vars();
            self.generate_function_instructions(function);
        }
    }

    fn generate_function_instructions(&mut self, function: Function) {
        self.instr.push(format!("{}:", function.id));
        self.instr.push("push rbp".to_string());
        self.instr.push("mov rbp, rsp".to_string());

        for statement in function.body {
            self.generate_statement_instructions(statement);
        }

        // TODO: figure out function epilogue for branching programs
        // for now im gonna say its the return statement's job
    }


    fn generate_statement_instructions(&mut self, statement: Statement) {
        match statement {
            Statement::Return(expr) => {
                self.generate_expression_instructions(expr);
                self.instr.push("mov rbp, rsp".to_string());
                self.instr.push("pop rbp".to_string());
                self.instr.push("ret".to_string());
            }
            Statement::Expression(expr) => {
                self.generate_expression_instructions(expr);
            }
            Statement::Declaration(id, expr) => {
                let offset = self.lg.generate_or_get_offset(id);
                match expr {
                    Some(expr) => {
                        self.generate_expression_instructions(expr);
                        self.instr.push(format!("mov dword [rbp-{}], eax", offset));
                    },
                    None => {
                        self.instr.push(format!("mov dword [rbp-{}], 0", offset));
                    }
                }
            }
        }
    }

    fn generate_expression_instructions(&mut self, expression: Expression) {
        match expression {
            Expression::UnOp(op, expr) => self.generate_unary_operator_instructions(op, *expr),
            Expression::BinOp(op, left, right) => self.generate_binary_operator_instructions(op, *left, *right),
            Expression::Constant(v) => self.instr.push(format!("mov rax, {v}")),
            Expression::Assignment(id, expr) => {
                // TODO: fix messy borrowing
                if !self.lg.is_declared(id.clone()) {
                    // TODO: better error handling
                    panic!("You didn't declare this");
                }
                self.generate_expression_instructions(*expr);
                let offset = self.lg.generate_or_get_offset(id);
                self.instr.push(format!("mov dword [rbp-{}], eax", offset));
            },
            Expression::CompoundAssignment(id, op, expr) => {
                todo!();
            }
            Expression::Variable(id) => {
                // TODO: fix messy borrowing
                if !self.lg.is_declared(id.clone()) {
                    // TODO: better error handling
                    panic!("You didn't declare this");
                }
                let offset = self.lg.generate_or_get_offset(id);
                self.instr.push(format!("mov eax, dword [rbp-{}]", offset));
            },
        };
    }

    fn generate_unary_operator_instructions(&mut self, op: UnaryOperator, expression: Expression) {
        self.generate_expression_instructions(expression);
        match op {
            UnaryOperator::Minus => self.instr.push("neg rax".to_string()),
            UnaryOperator::Complement => self.instr.push("not rax".to_string()),
            UnaryOperator::Negate => {
                self.instr.push("cmp rax, 0".to_string());
                self.instr.push("sete al".to_string());
                self.instr.push("movzx rax, al".to_string());
            }
        }
    }

    fn generate_binary_operator_instructions(&mut self, op: BinaryOperator, left: Expression, right: Expression) {
        self.generate_expression_instructions(left);
        self.instr.push("push rax".to_string());
        // first is pop
        // second is rax
        self.generate_expression_instructions(right);
        match op {
            BinaryOperator::Addition => {
                self.instr.push("pop rcx".to_string());
                self.instr.push("add rax, rcx".to_string())
            },
            BinaryOperator::Subtraction => {
                self.instr.push("pop rcx".to_string());
                self.instr.push("sub rcx, rax".to_string());
                self.instr.push("mov rax, rcx".to_string());
            },
            BinaryOperator::Multiplication => {
                self.instr.push("pop rcx".to_string());
                self.instr.push("imul rax, rcx".to_string())
            },
            BinaryOperator::Division => {
                self.instr.push("mov rax, rcx".to_string());
                self.instr.push("pop rax".to_string());
                self.instr.push("cqo".to_string());
                self.instr.push("idiv rcx".to_string());
            }
            BinaryOperator::LogicalAnd => {
                let eval_second = self.lg.generate_label();
                let done = self.lg.generate_label();
                self.instr.push("pop rcx".to_string());
                self.instr.push("cmp rcx, 0".to_string());
                self.instr.push(format!("jne {}", eval_second));
                self.instr.push("mov rax, 0".to_string());
                self.instr.push(format!("jmp {}", done));

                self.instr.push(format!("{}:", eval_second));
                self.instr.push("cmp rax, 0".to_string());
                self.instr.push("setne al".to_string());

                self.instr.push(format!("{}:", done));
            },
            BinaryOperator::LogicalOr => {
                let eval_second = self.lg.generate_label();
                let done = self.lg.generate_label();
                self.instr.push("pop rcx".to_string());
                self.instr.push("cmp rcx, 0".to_string());
                self.instr.push(format!("je {}", eval_second));
                self.instr.push("mov rax, 1".to_string());
                self.instr.push(format!("jmp {}", done));

                self.instr.push(format!("{}:", eval_second));
                self.instr.push("cmp rax, 0".to_string());
                self.instr.push("setne al".to_string());
                self.instr.push("movzx rax, al".to_string());

                self.instr.push(format!("{}:", done));
            },
            // TODO: figure out yucky syntax for these
            BinaryOperator::Equal | BinaryOperator::NotEqual | BinaryOperator::LessThan | BinaryOperator::GreaterThan | BinaryOperator::LessThanEq | BinaryOperator::GreaterThanEq => {
                self.instr.push("pop rcx".to_string());
                self.instr.push("cmp rcx, rax".to_string());
                match op {
                    BinaryOperator::Equal => self.instr.push("sete al".to_string()),
                    BinaryOperator::NotEqual => self.instr.push("setne al".to_string()),
                    BinaryOperator::LessThan => self.instr.push("setl al".to_string()),
                    BinaryOperator::GreaterThan => self.instr.push("setg al".to_string()),
                    BinaryOperator::LessThanEq => self.instr.push("setle al".to_string()),
                    BinaryOperator::GreaterThanEq => self.instr.push("setge al".to_string()),
                    _ => unreachable!(),
                }
                self.instr.push("movzx rax, al".to_string());
            }
            BinaryOperator::ShiftLeft | BinaryOperator::ShiftRight => {
                self.instr.push("mov rax, rcx".to_string());
                self.instr.push("pop rax".to_string());
                let instruction = match op {
                    BinaryOperator::ShiftLeft => "shl",
                    BinaryOperator::ShiftRight => "shr",
                    _ => unreachable!(),
                };
                self.instr.push(format!("{} rax, rcx", instruction));
            },
            BinaryOperator::BitwiseAnd => {
                self.instr.push("pop rcx".to_string());
                self.instr.push("and rax, rcx".to_string());
            },
            BinaryOperator::BitwiseOr => {
                self.instr.push("pop rcx".to_string());
                self.instr.push("or rax, rcx".to_string());
            },
            BinaryOperator::BitwiseXor => {
                self.instr.push("pop rcx".to_string());
                self.instr.push("xor rax, rcx".to_string());
            },
        }
    }
}