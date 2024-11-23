#[derive(Debug)]
pub struct Program(pub Function);

#[derive(Debug)]
pub struct Function {
    pub id: String,
    pub body: Statement,
}

#[derive(Debug)]
pub struct Statement(pub Expression);

#[derive(Debug)]
pub enum Expression {
    UnOp(UnaryOperator, Box<Expression>),
    BinOp(BinaryOperator, Box<Expression>, Box<Expression>),
    Constant(i32),
}

#[derive(Debug)]
pub enum UnaryOperator {
    MINUS,
    COMPLEMENT,
    NEGATIVE,
}

#[derive(Debug)]
pub enum BinaryOperator {
    ADDITION,
    SUBTRACTION,
    MULTIPLICATION,
    DIVISION,
}

