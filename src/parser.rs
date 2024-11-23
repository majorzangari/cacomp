use std::io::Read;
use crate::ast::{Expression, Function, Program, Statement, UnaryOperator};
use crate::parser::ParseError::SyntaxError;
use crate::tokenizer::{Keyword, Token, TokenType, Tokenizer, TokenizerError};

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
    let next = tk.peek().map_err(ParseError::TokenizerError)?;
    if next.is_unary_operator() {
        let op = match next.token_type {
            TokenType::Minus => UnaryOperator::MINUS,
            TokenType::Negate => UnaryOperator::NEGATIVE,
            TokenType::BitwiseComplement => UnaryOperator::COMPLEMENT,
            _ => return Err(SyntaxError(next.line_number)),
        };
        tk.advance().map_err(ParseError::TokenizerError)?;
        let child_expr = parse_expression(tk)?;
        let out = Expression::UnOp(op, Box::new(child_expr));
        Ok(out)
    } else {
        let out = Expression::Constant(consume_integer_literal(tk)?);
        Ok(out)
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

fn consume_integer_literal<R: Read> (tk: &mut Tokenizer<R>) -> Result<i32, ParseError> {
    let token = tk.next().map_err(ParseError::TokenizerError)?;
    if let TokenType::IntegerLiteral(v) = token.token_type {
        return Ok(v);
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
    use super::*;
    #[test]
    fn print() {
        println!("{:?}", parse(std::io::Cursor::new("int main() { return -!~2; }")));
    }
}