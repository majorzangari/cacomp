use std::collections::VecDeque;
use std::io;
use std::io::Read;

use crate::scanner::Scanner;

pub struct Tokenizer<R: Read> {
    sc: Scanner<R>,
    line_number: u32,
    tokens: VecDeque<Token>,
}

impl<R: Read> Tokenizer<R> {
    /// Create a new Tokenizer from a file
    pub fn new(reader: R) -> Tokenizer<R> {
        Tokenizer {
            sc: Scanner::new(reader),
            line_number: 1,
            tokens: VecDeque::new(),
        }
    }

    pub fn has_next(&mut self) -> Result<bool, TokenizerError> {
        let next_type = self.peek()?.token_type;
        Ok(next_type != TokenType::EOF)
    }

    pub fn advance(&mut self) -> Result<(), TokenizerError> {
        self.next()?;
        Ok(())
    }

    /// Peek at the next token without consuming it
    pub fn peek(&mut self) -> Result<Token, TokenizerError> {
        if self.tokens.is_empty() {
            self.lex_one_token().map_err(TokenizerError::IOError)?;
        }
        Ok(self.tokens.front().unwrap().clone())
    }

    /// Get the next token, consuming it
    pub fn next(&mut self) -> Result<Token, TokenizerError> {
        if self.tokens.is_empty() {
            self.lex_one_token().map_err(TokenizerError::IOError)?;
        }
        Ok(self.tokens.pop_front().unwrap())
    }


    /// Constructs one token from the input file, pushes it onto the
    /// token queue and returns it. Pushes the next token it can create, or an EOF token
    /// if the end of file was reached. If another error occurs while tokenizing,
    /// does not push a token.
    fn lex_one_token(&mut self) -> io::Result<()> {
        loop {
            let next = self.sc.next()?;
            return if let Some(c) = next {
                if c.is_whitespace() {
                    if c == '\n' {
                        self.line_number += 1;
                    }
                    continue;
                }
                let token_type = match c {
                    '{' => TokenType::OpenBrace,
                    '}' => TokenType::CloseBrace,
                    '(' => TokenType::OpenParen,
                    ')' => TokenType::CloseParen,
                    ';' => TokenType::Semicolon,
                    '-' => TokenType::Minus,
                    '~' => TokenType::BitwiseComplement,
                    '!' => TokenType::Negate,
                    '+' => TokenType::Plus,
                    '*' => TokenType::Star,
                    '/' => TokenType::Divide,
                    'a'..='z' | 'A'..='Z' | '_' => {
                        let str = self.get_identifier_string(c)?;
                        let keyword = Keyword::from_str(&str);
                        match keyword {
                            Some(keyword) => TokenType::Keyword(keyword),
                            None => TokenType::Identifier(str),
                        }
                    }
                    '0'..='9' => { // TODO: support other integer types
                        let num = self.lex_number(c)?;
                        TokenType::IntegerLiteral(num)
                    }
                    _ => panic!{"{}", c},
                };
                self.tokens.push_back(Token::new(token_type, self.line_number));
                Ok(())
            } else {
                self.tokens.push_back(Token::new(TokenType::EOF, self.line_number));
                Ok(())
            }
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
            let next = self.sc.peek()?;
            if let Some(c) = next{
                if c.is_ascii_alphanumeric() || c == '_' {
                    self.sc.advance()?;
                    out.push(c);
                    continue;
                }
            }
            return Ok(out);
        }
    }

    /// Extracts an integer literal starting with the given character.
    fn lex_number(&mut self, c: char) -> io::Result<i32> {
        let mut int_str = String::new();
        int_str.push(c);

        loop {
            let next = self.sc.peek()?;
            if let Some(c) = next {
                if c.is_ascii_digit() {
                    self.sc.advance()?;
                    int_str.push(c);
                    continue;
                }
            }
            return int_str.parse().map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "Failed to parse integer"));
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token{
    pub token_type: TokenType,
    pub line_number: u32,
}

impl Token {
    pub fn new(token_type: TokenType, line_number: u32) -> Token {
        Token {
            token_type,
            line_number,
        }
    }

    pub fn is_unary_operator(&self) -> bool {
        match self.token_type {
            TokenType::Minus | TokenType::BitwiseComplement | TokenType::Negate => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    OpenBrace,          // {
    CloseBrace,         // }
    OpenParen,          // (
    CloseParen,         // )
    Semicolon,          // ;
    Minus,              // -
    BitwiseComplement,  // ~
    Negate,             // !
    Plus,               // +
    Star,               // *
    Divide,             // /
    Keyword(Keyword),
    Identifier(String),
    IntegerLiteral(i32),
    EOF,
}

#[derive(Debug)]
pub enum TokenizerError {
    IOError(io::Error),
    InvalidToken(String, u32),
}

#[derive(Debug, Clone, PartialEq)]
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

#[cfg(test)]
mod tests {
    use std::io::Cursor;
    use super::*;

    fn create_tokenizer(input: &str) -> Tokenizer<Cursor<&[u8]>> {
        Tokenizer::new(Cursor::new(input.as_bytes()))
    }

    #[test]
    fn peek() {
        let mut tokenizer = create_tokenizer("int main() { return 0; }");
        assert_eq!(tokenizer.peek().unwrap(), Token::new(TokenType::Keyword(Keyword::Int), 1));
    }

    #[test]
    fn next() {
        let mut tokenizer = create_tokenizer("int main() { return 0; }");
        assert_eq!(tokenizer.next().unwrap(), Token::new(TokenType::Keyword(Keyword::Int), 1));
        assert_eq!(tokenizer.next().unwrap(), Token::new(TokenType::Identifier("main".to_string()), 1));
        assert_eq!(tokenizer.next().unwrap(), Token::new(TokenType::OpenParen, 1));
        assert_eq!(tokenizer.next().unwrap(), Token::new(TokenType::CloseParen, 1));
        assert_eq!(tokenizer.next().unwrap(), Token::new(TokenType::OpenBrace, 1));
        assert_eq!(tokenizer.next().unwrap(), Token::new(TokenType::Keyword(Keyword::Return), 1));
        assert_eq!(tokenizer.next().unwrap(), Token::new(TokenType::IntegerLiteral(0), 1));
        assert_eq!(tokenizer.next().unwrap(), Token::new(TokenType::Semicolon, 1));
        assert_eq!(tokenizer.next().unwrap(), Token::new(TokenType::CloseBrace, 1));
    }

    #[test]
    fn has_next() {
        let mut tokenizer = create_tokenizer("int main() { return 0; }");
        for _ in 0..9 {
            assert!(tokenizer.has_next().unwrap());
            tokenizer.next().unwrap();
        }
        assert!(!tokenizer.has_next().unwrap());
    }

    #[test]
    fn all_tokens() {
        let mut tokenizer = create_tokenizer("{}();-~!+*/");
        assert_eq!(tokenizer.next().unwrap(), Token::new(TokenType::OpenBrace, 1));
        assert_eq!(tokenizer.next().unwrap(), Token::new(TokenType::CloseBrace, 1));
        assert_eq!(tokenizer.next().unwrap(), Token::new(TokenType::OpenParen, 1));
        assert_eq!(tokenizer.next().unwrap(), Token::new(TokenType::CloseParen, 1));
        assert_eq!(tokenizer.next().unwrap(), Token::new(TokenType::Semicolon, 1));
        assert_eq!(tokenizer.next().unwrap(), Token::new(TokenType::Minus, 1));
        assert_eq!(tokenizer.next().unwrap(), Token::new(TokenType::BitwiseComplement, 1));
        assert_eq!(tokenizer.next().unwrap(), Token::new(TokenType::Negate, 1));
        assert_eq!(tokenizer.next().unwrap(), Token::new(TokenType::Plus, 1));
        assert_eq!(tokenizer.next().unwrap(), Token::new(TokenType::Star, 1));
        assert_eq!(tokenizer.next().unwrap(), Token::new(TokenType::Divide, 1));
    }

    #[test]
    fn print() {
        let mut tk = create_tokenizer(r#"
        int main() {
            return 0;
        }"#);
        while tk.has_next().unwrap() {
            println!("{:?}", tk.next().unwrap());
        }
    }
}