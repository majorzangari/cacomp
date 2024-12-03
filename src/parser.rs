#![allow(dead_code)]
#![allow(unused_variables)]

use crate::ast::{BinaryOperator, CompoundAssignment, Expression, Function, Program, Statement, UnaryOperator};
use crate::parser::ParseError::SyntaxError;
use crate::tokenizer::{Keyword, Token, TokenType, Tokenizer, TokenizerError};
use std::io::Read;

// Current Grammar:
// <program> ::= { <function> }
// <function> ::= "int" <id> "(" ")" "{" { <statement> } "}"
// <statement> ::= "return" <exp> ";"
// | <exp> ";"
// | "int" <id> [ = <exp>] ";"
// <exp> ::= <id> "=" <exp> | <logical-or-exp>
// <logical-or-exp> ::= <logical-and-exp> { "||" <logical-and-exp> }
// <logical-and-exp> ::= <bitwise-xor-exp> { "&&" <bitwise-xor-exp> }
// <bitwise-xor-exp> ::= <bitwise-or-exp> { "^" <bitwise-or-exp> }
// <bitwise-or-exp> ::= <bitwise-and-exp> { "|" <bitwise-and-exp> }
// <bitwise-and-exp> ::= <equality-exp> { "&" <equality-exp> }
// <equality-exp> ::= <relational-exp> { ("!=" | "==") <relational-exp> }
// <relational-exp> ::= <shift-exp> { ("<" | ">" | "<=" | ">=") <shift-exp> }
// <shift-exp> ::= <additive-exp> { ("<<" | ">>") <additive-exp> }
// <additive-exp> ::= <term> { ("+" | "-") <term> }
// <term> ::= <factor> { ("*" | "/") <factor> }
// <factor> ::= "(" <exp> ")" | <unary_op> <factor> | <int> | <id>
// <unary_op> ::= "!" | "~" | "-"

pub fn parse<R: Read> (reader: R) -> Result<Program, ParseError> {
    let mut tk = Tokenizer::new(reader);
    parse_program(&mut tk)
}

// <program> ::= <function>
fn parse_program<R: Read> (tk: &mut Tokenizer<R>) -> Result<Program, ParseError> {
    let mut functions = Vec::new();
    while matches!(tk.peek().map_err(ParseError::TokenizerError)?.token_type, TokenType::Keyword(Keyword::Int)) {
        let function = parse_function(tk)?;
        functions.push(function);
    }
    Ok(Program{functions})
}

// <function> ::= "int" <id> "(" ")" "{" { <statement> } "}"
fn parse_function<R: Read> (tk: &mut Tokenizer<R>) -> Result<Function, ParseError> {
    consume(tk, TokenType::Keyword(Keyword::Int))?;
    let id = consume_identifier(tk)?;
    consume(tk, TokenType::OpenParen)?;
    consume(tk, TokenType::CloseParen)?;
    consume(tk, TokenType::OpenBrace)?;

    let mut body = Vec::new();
    while tk.peek().map_err(ParseError::TokenizerError)?.token_type != TokenType::CloseBrace {
        let statement = parse_statement(tk)?;
        body.push(statement);
    }

    consume(tk, TokenType::CloseBrace)?;

    Ok(Function{id, body})
}

// <statement> ::= "return" <exp> ";"
// | <exp> ";"
// | "int" <id> [ = <exp>] ";"
fn parse_statement<R: Read> (tk: &mut Tokenizer<R>) -> Result<Statement, ParseError> {
    let next_type = tk.peek().map_err(ParseError::TokenizerError)?.token_type;

    match next_type {
        TokenType::Keyword(Keyword::Return) => parse_statement_return(tk),
        TokenType::Keyword(Keyword::Int) => parse_statement_declaration(tk),
        _ => parse_statement_pure_expression(tk),
    }
}

// "return" <exp> ";"
fn parse_statement_return<R: Read> (tk: &mut Tokenizer<R>) -> Result<Statement, ParseError> {
    consume(tk, TokenType::Keyword(Keyword::Return))?;
    let expr = parse_expression(tk)?;
    consume(tk, TokenType::Semicolon)?;
    let out = Statement::Return(expr);
    Ok(out)
}

// "int" <id> [ = <exp>] ";"
fn parse_statement_declaration<R: Read> (tk: &mut Tokenizer<R>) -> Result<Statement, ParseError> {
    consume(tk, TokenType::Keyword(Keyword::Int))?;
    let id = consume_identifier(tk)?;

    let next_type = tk.peek().map_err(ParseError::TokenizerError)?.token_type;
    let expr = match next_type {
        TokenType::Semicolon => None,
        _ => {
            consume(tk, TokenType::Assignment)?;
            let expr = parse_expression(tk)?;
            Some(expr)
        }
    };

    consume(tk, TokenType::Semicolon)?;
    let out = Statement::Declaration(id, expr);
    Ok(out)
}

// <exp> ";"
fn parse_statement_pure_expression<R: Read> (tk: &mut Tokenizer<R>) -> Result<Statement, ParseError> {
    let expr = parse_expression(tk)?;
    consume(tk, TokenType::Semicolon)?;
    let out = Statement::Expression(expr);
    Ok(out)
}

// <exp> ::= <id> "=" <exp> | <logical-or-exp>
fn parse_expression<R: Read> (tk: &mut Tokenizer<R>) -> Result<Expression, ParseError> {
    let is_assignment = tk.peek_ahead(1).map_err(ParseError::TokenizerError)?.is_assignment_operator();
    match is_assignment {
        true => parse_expression_assignment(tk),
        false => parse_logical_or_expression(tk),
    }
}

// <id> "=" <exp>
fn parse_expression_assignment<R: Read> (tk: &mut Tokenizer<R>) -> Result<Expression, ParseError> {
    let id = consume_identifier(tk)?;
    let next = tk.next().map_err(ParseError::TokenizerError)?;
    let expr = parse_expression(tk)?;
    let out = match next.token_type {
        TokenType::Assignment => Ok(Expression::Assignment(id, Box::new(expr))),
        TokenType::PlusEquals => Ok(Expression::CompoundAssignment(id, CompoundAssignment::Addition, Box::new(expr))),
        TokenType::MinusEquals => Ok(Expression::CompoundAssignment(id, CompoundAssignment::Subtraction, Box::new(expr))),
        TokenType::StarEquals => Ok(Expression::CompoundAssignment(id, CompoundAssignment::Multiplication, Box::new(expr))),
        TokenType::SlashEquals => Ok(Expression::CompoundAssignment(id, CompoundAssignment::Division, Box::new(expr))),
        TokenType::XorEquals => Ok(Expression::CompoundAssignment(id, CompoundAssignment::BitwiseXor, Box::new(expr))),
        TokenType::OrEquals => Ok(Expression::CompoundAssignment(id, CompoundAssignment::BitwiseOr, Box::new(expr))),
        TokenType::AndEquals => Ok(Expression::CompoundAssignment(id, CompoundAssignment::BitwiseAnd, Box::new(expr))),
        TokenType::LeftShiftEquals => Ok(Expression::CompoundAssignment(id, CompoundAssignment::ShiftLeft, Box::new(expr))),
        TokenType::RightShiftEquals => Ok(Expression::CompoundAssignment(id, CompoundAssignment::ShiftRight, Box::new(expr))),
        _ => Err(SyntaxError(next.line_number, format!("Unexpected assignment operator \"{:?}\"", next.token_type)))
    };
    out
}

// <logical-or-exp> ::= <logical-and-exp> { "||" <logical-and-exp> }
fn parse_logical_or_expression<R: Read> (tk: &mut Tokenizer<R>) -> Result<Expression, ParseError> {
    let mut out = parse_logical_and_expr(tk)?;
    let mut next = tk.peek().map_err(ParseError::TokenizerError)?;
    while next.token_type == TokenType::LogicalOr {
        let op = BinaryOperator::LogicalOr;
        tk.advance().map_err(ParseError::TokenizerError)?;
        let next_term = parse_logical_and_expr(tk)?;
        out = Expression::BinOp(op, Box::new(out), Box::new(next_term));
        next = tk.peek().map_err(ParseError::TokenizerError)?;
    }
    Ok(out)
}

// <logical-and-exp> ::= <bitwise-xor-exp> { "&&" <bitwise-xor-exp> }
fn parse_logical_and_expr<R: Read> (tk: &mut Tokenizer<R>) -> Result<Expression, ParseError> {
    let mut out = parse_bitwise_xor_expr(tk)?;
    let mut next = tk.peek().map_err(ParseError::TokenizerError)?;
    while next.token_type == TokenType::LogicalAnd {
        let op = BinaryOperator::LogicalAnd;
        tk.advance().map_err(ParseError::TokenizerError)?;
        let next_term = parse_bitwise_xor_expr(tk)?;
        out = Expression::BinOp(op, Box::new(out), Box::new(next_term));
        next = tk.peek().map_err(ParseError::TokenizerError)?;
    }
    Ok(out)
}

// <bitwise-xor-exp> ::= <bitwise-or-exp> { "^" <bitwise-or-exp> }
fn parse_bitwise_xor_expr<R: Read> (tk: &mut Tokenizer<R>) -> Result<Expression, ParseError> {
    let mut out = parse_bitwise_or_expr(tk)?;
    let mut next = tk.peek().map_err(ParseError::TokenizerError)?;
    while next.token_type == TokenType::BitwiseXor {
        let op = BinaryOperator::BitwiseXor;
        tk.advance().map_err(ParseError::TokenizerError)?;
        let next_term = parse_bitwise_or_expr(tk)?;
        out = Expression::BinOp(op, Box::new(out), Box::new(next_term));
        next = tk.peek().map_err(ParseError::TokenizerError)?;
    }
    Ok(out)
}

// <bitwise-or-exp> ::= <bitwise-and-exp> { "|" <bitwise-and-exp> }
fn parse_bitwise_or_expr<R: Read> (tk: &mut Tokenizer<R>) -> Result<Expression, ParseError> {
    let mut out = parse_bitwise_and_expr(tk)?;
    let mut next = tk.peek().map_err(ParseError::TokenizerError)?;
    while next.token_type == TokenType::BitwiseOr {
        let op = BinaryOperator::BitwiseOr;
        tk.advance().map_err(ParseError::TokenizerError)?;
        let next_term = parse_bitwise_and_expr(tk)?;
        out = Expression::BinOp(op, Box::new(out), Box::new(next_term));
        next = tk.peek().map_err(ParseError::TokenizerError)?;
    }
    Ok(out)
}

// <bitwise-and-exp> ::= <equality-exp> { "&" <equality-exp> }
fn parse_bitwise_and_expr<R: Read> (tk: &mut Tokenizer<R>) -> Result<Expression, ParseError> {
    let mut out = parse_equality_expr(tk)?;
    let mut next = tk.peek().map_err(ParseError::TokenizerError)?;
    while next.token_type == TokenType::BitwiseAnd {
        let op = BinaryOperator::BitwiseAnd;
        tk.advance().map_err(ParseError::TokenizerError)?;
        let next_term = parse_equality_expr(tk)?;
        out = Expression::BinOp(op, Box::new(out), Box::new(next_term));
        next = tk.peek().map_err(ParseError::TokenizerError)?;
    }
    Ok(out)
}

// <equality-exp> ::= <relational-exp> { ("!=" | "==") <relational-exp> }
fn parse_equality_expr<R: Read> (tk: &mut Tokenizer<R>) -> Result<Expression, ParseError> {
    let mut out = parse_relational_expr(tk)?;
    let mut next = tk.peek().map_err(ParseError::TokenizerError)?;
    while matches!(next.token_type, TokenType::Equal | TokenType::NotEqual) {
        let op = match next.token_type {
            TokenType::Equal => BinaryOperator::Equal,
            TokenType::NotEqual => BinaryOperator::NotEqual,
            _ => return Err(SyntaxError(next.line_number, format!("Unexpected equality operator: {:?}", next))),
        };
        tk.advance().map_err(ParseError::TokenizerError)?;
        let next_term = parse_relational_expr(tk)?;
        out = Expression::BinOp(op, Box::new(out), Box::new(next_term));
        next = tk.peek().map_err(ParseError::TokenizerError)?;
    }
    Ok(out)
}

// <relational-exp> ::= <additive-exp> { ("<" | ">" | "<=" | ">=") <additive-exp> }
fn parse_relational_expr<R: Read> (tk: &mut Tokenizer<R>) -> Result<Expression, ParseError> {
    let mut out = parse_shift_expr(tk)?;
    let mut next = tk.peek().map_err(ParseError::TokenizerError)?;
    while matches!(next.token_type, TokenType::LessThan | TokenType::GreaterThan | TokenType::LessThanEq | TokenType::GreaterThanEq) {
        let op = match next.token_type {
            TokenType::LessThan => BinaryOperator::LessThan,
            TokenType::GreaterThan => BinaryOperator::GreaterThan,
            TokenType::LessThanEq => BinaryOperator::LessThanEq,
            TokenType::GreaterThanEq => BinaryOperator::GreaterThanEq,
            _ => return Err(SyntaxError(next.line_number, format!("Unexpected relational operator: {:?}", next))),
        };
        tk.advance().map_err(ParseError::TokenizerError)?;
        let next_term = parse_shift_expr(tk)?;
        out = Expression::BinOp(op, Box::new(out), Box::new(next_term));
        next = tk.peek().map_err(ParseError::TokenizerError)?;
    }
    Ok(out)
}

fn parse_shift_expr<R: Read> (tk: &mut Tokenizer<R>) -> Result<Expression, ParseError> {
    let mut out = parse_additive_expression(tk)?;
    let mut next = tk.peek().map_err(ParseError::TokenizerError)?;
    while matches!(next.token_type, TokenType::BitwiseShiftRight | TokenType::BitwiseShiftLeft) {
        let op = match next.token_type {
            TokenType::BitwiseShiftRight => BinaryOperator::ShiftRight,
            TokenType::BitwiseShiftLeft => BinaryOperator::ShiftLeft,
            _ => return Err(SyntaxError(next.line_number, format!("Unexpected relational operator: {:?}", next))),
        };
        tk.advance().map_err(ParseError::TokenizerError)?;
        let next_term = parse_additive_expression(tk)?;
        out = Expression::BinOp(op, Box::new(out), Box::new(next_term));
        next = tk.peek().map_err(ParseError::TokenizerError)?;
    }
    Ok(out)
}

// <additive-exp> ::= <term> { ("+" | "-") <term> }
fn parse_additive_expression<R: Read> (tk: &mut Tokenizer<R>) -> Result<Expression, ParseError> {
    let mut out = parse_term(tk)?;
    let mut next = tk.peek().map_err(ParseError::TokenizerError)?;
    while next.token_type == TokenType::Plus || next.token_type == TokenType::Minus {
        let op = match next.token_type {
            TokenType::Plus => BinaryOperator::Addition,
            TokenType::Minus => BinaryOperator::Subtraction,
            _ => return Err(SyntaxError(next.line_number, format!("Unexpected additive operator, found {:?}", next))),
        };
        tk.advance().map_err(ParseError::TokenizerError)?;
        let next_term = parse_term(tk)?;
        out = Expression::BinOp(op, Box::new(out), Box::new(next_term));
        next = tk.peek().map_err(ParseError::TokenizerError)?;
    }
    Ok(out)
}

// <term> ::= <factor> { ("*" | "/") <factor> }
fn parse_term<R: Read> (tk: &mut Tokenizer<R>) -> Result<Expression, ParseError> {
    let mut out = parse_factor(tk)?;
    let mut next = tk.peek().map_err(ParseError::TokenizerError)?;
    while next.token_type == TokenType::Star || next.token_type == TokenType::Divide {
        let op = match next.token_type {
            TokenType::Star => BinaryOperator::Multiplication,
            TokenType::Divide => BinaryOperator::Division,
            _ => return Err(SyntaxError(next.line_number, format!("Unexpected multiplicative operator, found {:?}", next))),
        };
        tk.advance().map_err(ParseError::TokenizerError)?;
        let next_factor = parse_factor(tk)?;
        out = Expression::BinOp(op, Box::new(out), Box::new(next_factor));
        next = tk.peek().map_err(ParseError::TokenizerError)?;
    }
    Ok(out)
}

// <factor> ::= "(" <exp> ")" | <unary_op> <factor> | <int> | <id>
fn parse_factor<R: Read> (tk: &mut Tokenizer<R>) -> Result<Expression, ParseError> {
    let next = tk.next().map_err(ParseError::TokenizerError)?;
    match next.token_type {
        TokenType::OpenParen => {
            let out = parse_expression(tk)?;
            println!("found: {:?}", out);
            consume(tk, TokenType::CloseParen)?;
            Ok(out)
        },
        _ if next.is_unary_operator() => {
            let op = match next.token_type {
                TokenType::Minus => UnaryOperator::Minus,
                TokenType::BitwiseComplement => UnaryOperator::Complement,
                TokenType::Negate => UnaryOperator::Negate,
                _ => return Err(SyntaxError(next.line_number, format!("Unexpected unary operator, found {:?}", next))),
            };
            let factor = parse_factor(tk)?;
            let out = Expression::UnOp(op, Box::new(factor));
            Ok(out)
        },
        TokenType::IntegerLiteral(v) => {
            let out = Expression::Constant(v);
            Ok(out)
        },
        TokenType::Identifier(s) => {
            let out = Expression::Variable(s);
            Ok(out)
        },
        _ => Err(SyntaxError(next.line_number, format!("Unexpected factor, found {:?}", next))),
    }
}

/// Consume a token of the given type from the tokenizer.
fn consume<R: Read> (tk: &mut Tokenizer<R>, token_type: TokenType) -> Result<Token, ParseError> {
    let token = tk.next().map_err(ParseError::TokenizerError)?;
    if token.token_type != token_type {
        return Err(SyntaxError(token.line_number, format!("Expected {:?}, found {:?}", token_type, token)));
    }
    Ok(token)
}

fn consume_identifier<R: Read> (tk: &mut Tokenizer<R>) -> Result<String, ParseError> {
    let token = tk.next().map_err(ParseError::TokenizerError)?;
    if let TokenType::Identifier(s) = token.token_type {
        return Ok(s);
    }
    Err(SyntaxError(token.line_number, format!("Expected identifier, found {:?}", token)))
}

#[derive(Debug)]
pub enum ParseError {
    TokenizerError(TokenizerError),
    SyntaxError(u32, String),
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::generator::Assembly;
    #[test]
    fn print() {
        println!("{}", parse(std::io::Cursor::new("\
        int main() {
            int x = 3;
            x += 4;
            return 1 + 3;
        }")).unwrap());
    }

    #[test]
    fn file() {
        let parsed = parse(std::io::Cursor::new("\
            int main() {
                int x = 3;
                int y = x;
                return x;
            }")).unwrap();
        let asm = Assembly::new(parsed);
        asm.write_to_file("main.asm").unwrap();
    }

    #[test]
    fn test() {
        let parsed = parse(std::io::Cursor::new("\
        int main() { 
            int x = 3;
            x = x + 4;
            int y = x + 5;
            y = x + y;
            return y;
        }")).unwrap();
        println!("{}", Assembly::new(parsed));
    }
}