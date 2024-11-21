use std::collections::VecDeque;
use std::fs::File;
use std::io;
use std::io::{BufReader, Read};

use crate::scanner::Scanner;
// TODO: change up Vec, maybe slow for fifo

pub struct Tokenizer<R: Read> {
    sc: Scanner<R>,
    line_number: i32,
    tokens: VecDeque<Token>,
}

impl<R: Read> Tokenizer<R> {
    /// Create a new Tokenizer from a file
    pub fn new(reader: R) -> Tokenizer<R> {
        Tokenizer {
            sc: Scanner::new(reader),
            line_number: 0,
            tokens: VecDeque::new(),
        }
    }

    /// Peek at the next token without consuming it
    pub fn peek(&mut self) -> Result<Token, TokenizerError> {
        if self.tokens.is_empty() {
            return self.lex_one_token().map_err(TokenizerError::IOError);
        }
        Ok(self.tokens.pop_front().unwrap())
    }

    /// Get the next token, consuming it
    pub fn next(&mut self) -> Result<Token, TokenizerError> {
        if self.tokens.is_empty() {
            self.lex_one_token().map_err(TokenizerError::IOError)?;
            return Ok(self.tokens.pop_front().unwrap());
        }
        Ok(self.tokens.pop_front().unwrap())
    }


    /// Constructs one token from the input file and pushes it onto the
    /// token queue. Pushes the next token it can create, or an EOF token
    /// if the end of file was reached. If another error occurs while tokenizing,
    /// does not push a token.
    fn lex_one_token(&mut self) -> io::Result<Token> {
        let next = self.sc.next()?;
        if let Some(c) = next {
            match c {
                _ => {}
            }
            todo!();
        } else {
            Ok(Token::EOF)
        }

    }
}

#[derive(Debug, Clone)]
pub enum Token {
    OpenBrace,
    CloseBrace,
    OpenParen,
    CloseParen,
    Semicolon,
    Keyword(Keyword),
    Identifier(String),
    IntegerLiteral(i32),
    EOF,
}

#[derive(Debug)]
pub enum TokenizerError {
    IOError(io::Error),
    InvalidToken(String),
}

#[derive(Debug, Clone)]
pub enum Keyword {
    Int,
    Return,
}

impl Keyword {
    fn as_str(&self) -> &str {
        match self {
            Keyword::Int => "int",
            Keyword::Return => "return",
        }
    }

    fn from_str(input: &str) -> Option<Keyword> {
        match input {
            "int" => Some(Keyword::Int),
            "return" => Some(Keyword::Return),
            _ => None,
        }
    }
}