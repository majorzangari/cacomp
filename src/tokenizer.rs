use std::collections::VecDeque;
use std::io;
use std::io::Read;

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
            let token = match c {
                '{' => Token::OpenBrace,
                '}' => Token::CloseBrace,
                '(' => Token::OpenParen,
                ')' => Token::CloseParen,
                ';' => Token::Semicolon,
                'a'..='z' | 'A'..='Z' | '_' => {
                    let str = self.get_identifier_string(c)?;
                    let keyword = Keyword::from_str(&str);
                    match keyword {
                        Some(keyword) => Token::Keyword(keyword),
                        None => Token::Identifier(str),
                    }
                }
                '0'..='9' => { // TODO: support other integer types
                    let num = self.lex_number(c)?;
                    Token::IntegerLiteral(num)
                }
                _ => todo!(),
            };
            Ok(token)
        } else {
            Ok(Token::EOF)
        }
    }

    /// Extracts an identifier string starting with the given character.
    ///
    /// This function takes an initial character and continues to read characters
    /// from the scanner until it encounters a non-alphanumeric or
    /// underscore character. It returns the collected string as an identifier.
    fn get_identifier_string(&mut self, c: char) -> io::Result<String> {
        let mut out = String::new();
        out.push(c);

        loop {
            let next = self.sc.next()?;
            if let Some(c) = next {
                if c.is_ascii_alphanumeric() || c == '_' {
                    out.push(c);
                }
            } else {
                return Ok(out);
            }
        }
    }

    /// Extracts an integer literal starting with the given character.
    fn lex_number(&mut self, c: char) -> io::Result<i32> {
        let mut int_str = String::new();
        int_str.push(c);

        loop {
            let next = self.sc.next()?;
            if let Some(c) = next {
                if c.is_ascii_digit() {
                    int_str.push(c);
                }
            } else {
                return Ok(int_str.parse().unwrap());
            }
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