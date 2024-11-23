#![allow(dead_code)]
#![allow(unused_variables)]

use std::io::Read;
use crate::ast::{BinaryOperator, Expression, Function, Program, Statement, UnaryOperator};
use crate::parser::ParseError::SyntaxError;
use crate::tokenizer::{Keyword, Token, TokenType, Tokenizer, TokenizerError};

// Current Grammar:
// <Program> ::= <Function>
// <Function> ::= "int" <id> "(" ")" "{" <Statement> "}"
// <Statement> ::= "return" <Expr> ";"
// <Expr> ::= <Term> ( ("+" | "-") <Term> )*
// <Term> ::= <Factor> ( ("*" | "/") <Factor>)
// <Factor> ::= "(" <Expr> ")" | <UnaryOp> <Factor> | <int>

pub fn parse<R: Read> (reader: R) -> Result<Program, ParseError> {
    let mut tk = Tokenizer::new(reader);
    parse_program(&mut tk)
}

fn parse_program<R: Read> (tk: &mut Tokenizer<R>) -> Result<Program, ParseError> {
    let function = parse_function(tk)?;
    Ok(Program(function))
}

fn parse_function<R: Read> (tk: &mut Tokenizer<R>) -> Result<Function, ParseError> {
    consume(tk, TokenType::Keyword(Keyword::Int))?;
    let id = consume_identifier(tk)?;
    consume(tk, TokenType::OpenParen)?;
    consume(tk, TokenType::CloseParen)?;
    consume(tk, TokenType::OpenBrace)?;

    let body = parse_statement(tk)?;
    consume(tk, TokenType::CloseBrace)?;

    let out = Function {
        id,
        body,
    };
    Ok(out)
}

// <Statement> ::= "return" <Expr> ";"
fn parse_statement<R: Read> (tk: &mut Tokenizer<R>) -> Result<Statement, ParseError> {
    consume(tk, TokenType::Keyword(Keyword::Return))?;
    let expr = parse_expression(tk)?;
    consume(tk, TokenType::Semicolon)?;
    Ok(Statement(expr))
}


// <Expr> ::= <Term> ( ("+" | "-") <Term> )*
fn parse_expression<R: Read> (tk: &mut Tokenizer<R>) -> Result<Expression, ParseError> {
    let mut out = parse_term(tk)?;
    let mut next = tk.peek().map_err(ParseError::TokenizerError)?;
    while next.token_type == TokenType::Plus || next.token_type == TokenType::Minus {
        let op = match next.token_type {
            TokenType::Plus => BinaryOperator::Addition,
            TokenType::Minus => BinaryOperator::Subtraction,
            _ => return Err(SyntaxError(next.line_number)),
        };
        tk.advance().map_err(ParseError::TokenizerError)?;
        let next_term = parse_term(tk)?;
        out = Expression::BinOp(op, Box::new(out), Box::new(next_term));
        next = tk.peek().map_err(ParseError::TokenizerError)?;
    }
    Ok(out)
}

// <Term> ::= <Factor> ( ("*" | "/") <Factor>)
fn parse_term<R: Read> (tk: &mut Tokenizer<R>) -> Result<Expression, ParseError> {
    let mut out = parse_factor(tk)?;
    let mut next = tk.peek().map_err(ParseError::TokenizerError)?;
    while next.token_type == TokenType::Star || next.token_type == TokenType::Divide {
        let op = match next.token_type {
            TokenType::Star => BinaryOperator::Multiplication,
            TokenType::Divide => BinaryOperator::Division,
            _ => return Err(SyntaxError(next.line_number)),
        };
        tk.advance().map_err(ParseError::TokenizerError)?;
        let next_factor = parse_term(tk)?;
        out = Expression::BinOp(op, Box::new(out), Box::new(next_factor));
        next = tk.peek().map_err(ParseError::TokenizerError)?;
    }
    Ok(out)
}

// <Factor> ::= "(" <Expr> ")" | <UnaryOp> <Factor> | <int>
fn parse_factor<R: Read> (tk: &mut Tokenizer<R>) -> Result<Expression, ParseError> {
    let next = tk.next().map_err(ParseError::TokenizerError)?;
    match next.token_type {
        TokenType::OpenParen => {
            let out = parse_expression(tk)?;
            consume(tk, TokenType::CloseParen)?;
            Ok(out)
        },
        _ if next.is_unary_operator() => {
            let op = match next.token_type {
                TokenType::Minus => UnaryOperator::Minus,
                TokenType::BitwiseComplement => UnaryOperator::Complement,
                TokenType::Negate => UnaryOperator::Negate,
                _ => return Err(SyntaxError(next.line_number)),
            };
            let factor = parse_factor(tk)?;
            let out = Expression::UnOp(op, Box::new(factor));
            Ok(out)
        },
        TokenType::IntegerLiteral(v) => {
            let out = Expression::Constant(v);
            Ok(out)
        },
        _ => Err(SyntaxError(next.line_number))
    }
}

/// Consume a token of the given type from the tokenizer.
fn consume<R: Read> (tk: &mut Tokenizer<R>, token_type: TokenType) -> Result<Token, ParseError> {
    let token = tk.next().map_err(ParseError::TokenizerError)?;
    if token.token_type != token_type {
        return Err(SyntaxError(token.line_number));
    }
    Ok(token)
}

fn consume_identifier<R: Read> (tk: &mut Tokenizer<R>) -> Result<String, ParseError> {
    let token = tk.next().map_err(ParseError::TokenizerError)?;
    if let TokenType::Identifier(s) = token.token_type {
        return Ok(s);
    }
    Err(SyntaxError(token.line_number))
}

#[derive(Debug)]
pub enum ParseError {
    TokenizerError(TokenizerError),
    SyntaxError(u32),
}

#[cfg(test)]
mod tests {
    use crate::generator::Generator;
    use super::*;
    #[test]
    fn print() {
        println!("{:?}", parse(std::io::Cursor::new("int main() { return 1 + 2 * 3; }")));
    }

    #[test]
    fn test() {
        let parsed = parse(std::io::Cursor::new("int main() { return -~--3; }")).unwrap();
        println!("{}", Generator::new(parsed));
    }
}