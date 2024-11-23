#![allow(dead_code)]
#![allow(unused_variables)]

#[derive(Debug)]
pub struct Program(pub Function);

#[derive(Debug)]
pub struct Function {
    pub id: String,
    pub body: Statement,
}

#[derive(Debug)]
pub struct Statement(pub Expression); // Always return for now

#[derive(Debug)]
pub enum Expression {
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

#[derive(Debug)]
pub enum BinaryOperator {
    Addition,
    Subtraction,
    Multiplication,
    Division,
}

