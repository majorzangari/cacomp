use std::collections::VecDeque;
use std::fs::File;
use std::io::{BufReader, Read};

pub struct Tokenizer {
    br: BufReader<File>,
    line_number: i32,
    tokens: VecDeque<Token>
}

impl Tokenizer {
    /// Create a new Tokenizer from a file
    pub fn new(file: File) -> Tokenizer {
        Tokenizer {
            br: BufReader::new(file),
            line_number: 0,
            tokens: VecDeque::new()
        }
    }

    /// Peek at the next token without consuming it
    pub fn peek(&mut self) -> Option<&Token> {
        if self.tokens.is_empty() {
            self.lex_one_token();
        }
        self.tokens.get(0)
    }

    /// Get the next token, consuming it
    pub fn next(&mut self) -> Option<Token> {
        self.peek();
        self.tokens.pop_front()
    }


    pub fn lex_one_token(&mut self) {
        let mut buffer = [0; 1];
        if let Ok(_) = self.br.read_exact(&mut buffer) {
            let c = buffer[0] as char;
        } else {
            self.tokens.push_back(Token::EOF);
        }
    }
}

fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens= Vec::new();

    for c in input.chars() {
        while c.is_whitespace() {
            continue;
        }

        let token = match c {
            '{' => Token::OpenBrace,
            '}' => Token::CloseBrace,
            '(' => Token::OpenParen,
            ')' => Token::CloseParen,
            ';' => Token::Semicolon,
            _ => todo!(),
        };
    }

    tokens
}

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