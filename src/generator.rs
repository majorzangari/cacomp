#![allow(dead_code)]
#![allow(unused_variables)]

use std::fmt;
use crate::ast::{BinaryOperator, Expression, Function, Program, Statement, UnaryOperator};

// TODO: get better name for this
#[derive(Debug)]
pub struct Generator {
    instr: Vec<String>,
}
impl Generator {
    pub fn new(program: Program) -> Generator {
        let mut out = Generator { instr: Vec::new() };
        out.generate_instructions(program);
        out
    }

    fn generate_instructions(&mut self, program: Program) {
        self.instr.push("global _start".to_string());
        self.instr.push("_start:".to_string());
        self.generate_function_instructions(program.0);
    }

    fn generate_function_instructions(&mut self, function: Function) {
        assert_eq!("main", function.id); // TODO: obviously this is stupid
        self.generate_statement_instructions(function.body);
    }

    // Always return for now
    fn generate_statement_instructions(&mut self, statement: Statement) {
        self.generate_expression_instructions(statement.0);
        self.instr.push("mov rdi, rax".to_string());
        self.instr.push("mov rax, 60".to_string());
        self.instr.push("syscall".to_string());
    }

    fn generate_expression_instructions(&mut self, expression: Expression) {
        match expression {
            Expression::UnOp(op, expr) => self.generate_unary_operator_instructions(op, *expr),
            Expression::BinOp(_, _, _) => todo!(),
            Expression::Constant(v) => {
                self.instr.push(format!("mov rax, {v}"));
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
}

impl fmt::Display for Generator {
    // TODO: add some indentation
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for instr in &self.instr {
            writeln!(f, "{}", instr)?;
        }
        Ok(())
    }
}