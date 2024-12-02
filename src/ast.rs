#![allow(dead_code)]
#![allow(unused_variables)]

use std::fmt;
use fmt::Display;
use std::fmt::Formatter;

#[derive(Debug)]
pub struct Program{
    pub functions: Vec<Function>,
}

#[derive(Debug)]
pub struct Function {
    pub id: String,
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    Return(Expression),
    Expression(Expression),
    Declaration(String, Option<Expression>),
}

#[derive(Debug)]
pub enum Expression {
    Assignment(String, Box<Expression>),
    CompoundAssignment(String, CompoundAssignment, Box<Expression>),
    Variable(String),
    UnOp(UnaryOperator, Box<Expression>),
    BinOp(BinaryOperator, Box<Expression>, Box<Expression>),
    Constant(i32),
}

#[derive(Debug)]
pub enum UnaryOperator {
    Minus,
    Complement,
    Negate,
}

impl UnaryOperator {
    fn str_rep(&self) -> &str {
        match self {
            UnaryOperator::Minus => "Minus",
            UnaryOperator::Complement => "Complement",
            UnaryOperator::Negate => "Negate",
        }
    }
}

#[derive(Debug)]
pub enum BinaryOperator {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    LogicalAnd,
    LogicalOr,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanEq,
    GreaterThanEq,
    ShiftLeft,
    ShiftRight,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
}

impl BinaryOperator {
    pub fn is_comparison_operator(&self) -> bool {
        matches!(self, BinaryOperator::Equal | BinaryOperator::NotEqual | BinaryOperator::LessThan | BinaryOperator::LessThanEq | BinaryOperator::GreaterThan | BinaryOperator::GreaterThanEq)
    }

    pub fn str_rep(&self) -> &str {
        match self {
            BinaryOperator::Addition => "Addition",
            BinaryOperator::Subtraction => "Subtraction",
            BinaryOperator::Multiplication => "Multiplication",
            BinaryOperator::Division => "Division",
            BinaryOperator::LogicalAnd => "LogicalAnd",
            BinaryOperator::LogicalOr => "LogicalOr",
            BinaryOperator::Equal => "Equals",
            BinaryOperator::NotEqual => "NotEqual",
            BinaryOperator::LessThan => "LessThan",
            BinaryOperator::GreaterThan => "GreaterThan",
            BinaryOperator::LessThanEq => "LessThanOrEqual",
            BinaryOperator::GreaterThanEq => "GreaterThanOrEqual",
            BinaryOperator::ShiftLeft => "BitwiseShiftLeft",
            BinaryOperator::ShiftRight => "BitwiseShiftRight",
            BinaryOperator::BitwiseAnd => "BitwiseAnd",
            BinaryOperator::BitwiseOr => "BitwiseOr",
            BinaryOperator::BitwiseXor => "BitwiseXor",
        }
    }
}

#[derive(Debug)]
pub enum CompoundAssignment {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Modulus,
    BitwiseXor,
    BitwiseOr,
    BitwiseAnd,
    ShiftLeft,
    ShiftRight,
}

impl CompoundAssignment {
    pub fn str_rep(&self) -> &str {
        match self {
            CompoundAssignment::Addition => "Addition",
            CompoundAssignment::Subtraction => "Subtraction",
            CompoundAssignment::Multiplication => "Multiplication",
            CompoundAssignment::Division => "Division",
            CompoundAssignment::Modulus => "Modulus",
            CompoundAssignment::BitwiseXor => "Xor",
            CompoundAssignment::BitwiseOr => "Or",
            CompoundAssignment::BitwiseAnd => "And",
            CompoundAssignment::ShiftLeft => "ShiftLeft",
            CompoundAssignment::ShiftRight => "ShiftRight",
        }
    }
}

/// Trait for types that can be displayed with indentation
trait IndentDisplay {
    fn indent_display(&self, tabs: usize, f: &mut Formatter<'_>) -> fmt::Result;
}

impl IndentDisplay for Program {
    fn indent_display(&self, tabs: usize, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}Program:", " ".repeat(tabs))?;
        for function in &self.functions {
            write!(f, "\n")?;
            function.indent_display(tabs + 1, f)?;
        }
        Ok(())
    }
}
impl IndentDisplay for Function {
    fn indent_display(&self, tabs: usize, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}Function \"{}\":", " ".repeat(tabs), self.id)?;
        for statement in &self.body {
            write!(f, "\n")?;
            statement.indent_display(tabs + 1, f)?;
        }
        Ok(())
    }
}
impl IndentDisplay for Statement {
    fn indent_display(&self, tabs: usize, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Return(expr) => {
                write!(f, "{}Return:", " ".repeat(tabs))?;
                expr.indent_display(tabs + 1, f)?;
            }
            Statement::Expression(expr) => {
                write!(f, "{}Expression:", " ".repeat(tabs))?;
                expr.indent_display(tabs + 1, f)?;
            }
            Statement::Declaration(id, expr) => {
                write!(f, "{}Declaration \"{}\":", " ".repeat(tabs), id)?;
                if let Some(expression) = expr {
                    write!(f, "\n")?;
                    expression.indent_display(tabs + 1, f)?;
                }
            }
        }
        Ok(())
    }
}
impl IndentDisplay for Expression {
    fn indent_display(&self, tabs: usize, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Assignment(id, expr) => {
                write!(f, "{}Assignment \"{}\"\n", " ".repeat(tabs), id)?;
                expr.indent_display(tabs + 1, f)?;
            }
            Expression::CompoundAssignment(id, op, expr) => {
                write!(f, "{}{} CompoundAssignment \"{}\"", " ".repeat(tabs), op.str_rep(), id)?;
                expr.indent_display(tabs + 1, f)?;
            }
            Expression::Variable(id) => {
                write!(f, "{}Variable \"{}\"", " ".repeat(tabs), id)?;
            }
            Expression::UnOp(op, expr) => {
                write!(f, "{}{}\n", " ".repeat(tabs), op.str_rep())?;
                expr.indent_display(tabs + 1, f)?;
            }
            Expression::BinOp(op, left, right) => {
                write!(f, "{}{}\n", " ".repeat(tabs), op.str_rep())?;
                left.indent_display(tabs + 1, f)?;
                write!(f, "\n")?;
                right.indent_display(tabs + 1, f)?;
            }
            Expression::Constant(i) => {
                write!(f, "{}Constant {}", " ".repeat(tabs), i)?;
            }
        }
        Ok(())
    }
}

/// Implement Display for a type by delegating to IndentDisplay
macro_rules! impl_display_from_indent_display {
    ($type:ty) => {
        impl Display for $type {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                self.indent_display(0, f)
            }
        }
    }
}

impl_display_from_indent_display!(Program);
impl_display_from_indent_display!(Function);
impl_display_from_indent_display!(Statement);