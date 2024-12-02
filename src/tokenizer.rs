#![allow(dead_code)]
#![allow(unused_variables)]

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

    /// Check if there are more tokens to read
    pub fn has_next(&mut self) -> Result<bool, TokenizerError> {
        let next_type = self.peek()?.token_type;
        Ok(next_type != TokenType::EOF)
    }

    /// Advance to the next token
    pub fn advance(&mut self) -> Result<(), TokenizerError> {
        self.next()?;
        Ok(())
    }

    /// Peek at the next token without consuming it
    pub fn peek(&mut self) -> Result<Token, TokenizerError> {
        if self.tokens.is_empty() {
            self.lex_one_token()?;
        }
        Ok(self.tokens.front().unwrap().clone())
    }

    /// Peek at a token a certain distance ahead without consuming it
    /// tk.peek_ahead(0) is equivalent to tk.peek()
    pub fn peek_ahead(&mut self, distance: usize) -> Result<Token, TokenizerError> {
        while self.tokens.len() < distance + 1 {
            if self.tokens.back().map_or(false, |v| v.token_type == TokenType::EOF) {
                return Ok(self.tokens.back().unwrap().clone());
            }
            self.lex_one_token()?;
        }
        Ok(self.tokens.get(distance).unwrap().clone())
    }

    /// Get the next token, consuming it
    pub fn next(&mut self) -> Result<Token, TokenizerError> {
        if self.tokens.is_empty() {
            self.lex_one_token()?;
        }
        Ok(self.tokens.pop_front().unwrap())
    }


    /// Constructs one token from the input file, pushes it onto the
    /// token queue and returns it. Pushes the next token it can create, or an EOF token
    /// if the end of file was reached. If another error occurs while tokenizing,
    /// does not push a token.
    fn lex_one_token(&mut self) -> Result<(), TokenizerError> {
        loop {
            let next = self.sc.next().map_err(TokenizerError::IOError)?;
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
                    '-' => self.if_equals_next_or(TokenType::MinusEquals, TokenType::Minus)?,
                    '~' => TokenType::BitwiseComplement,
                    '!' => self.lex_exclamation_mark()?,
                    '+' => self.if_equals_next_or(TokenType::PlusEquals, TokenType::Plus)?,
                    '*' => self.if_equals_next_or(TokenType::StarEquals, TokenType::Star)?,
                    '/' => self.if_equals_next_or(TokenType::SlashEquals, TokenType::Divide)?,
                    '^' => self.if_equals_next_or(TokenType::XorEquals, TokenType::BitwiseXor)?,
                    '=' => self.lex_equals_sign()?,
                    '<' => self.lex_less_than()?,
                    '>' => self.lex_greater_than()?,
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
                    '&' => self.lex_and()?,
                    '|' => self.lex_or()?,
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

    fn lex_exclamation_mark(&mut self) -> Result<TokenType, TokenizerError> {
        let next = self.sc.peek().map_err(TokenizerError::IOError)?;
        let out = match next {
            Some(c) if c == '=' => {
                self.sc.advance().map_err(TokenizerError::IOError)?;
                TokenType::NotEqual
            },
            Some(_) | None => TokenType::Negate,
        };
        Ok(out)
    }

    fn lex_equals_sign(&mut self) -> Result<TokenType, TokenizerError> {
        let next = self.sc.peek().map_err(TokenizerError::IOError)?;
        let out = match next {
            Some(c) if c == '=' => {
                self.sc.advance().map_err(TokenizerError::IOError)?;
                TokenType::Equal
            },
            Some(_) | None => TokenType::Assignment,
        };
        Ok(out)
    }

    fn lex_less_than(&mut self) -> Result<TokenType, TokenizerError> {
        let next = self.sc.peek().map_err(TokenizerError::IOError)?;
        let out = match next {
            Some(c) if c == '=' => {
                self.sc.advance().map_err(TokenizerError::IOError)?;
                TokenType::LessThanEq
            },
            Some(c) if c == '<' => {
                self.sc.advance().map_err(TokenizerError::IOError)?;
                self.if_equals_next_or(TokenType::LeftShiftEquals, TokenType::BitwiseShiftLeft)?
            }
            Some(_) | None => TokenType::LessThan,
        };
        Ok(out)
    }

    fn lex_greater_than(&mut self) -> Result<TokenType, TokenizerError> {
        let next = self.sc.peek().map_err(TokenizerError::IOError)?;
        let out = match next {
            Some(c) if c == '=' => {
                self.sc.advance().map_err(TokenizerError::IOError)?;
                TokenType::GreaterThanEq
            },
            Some(c) if c == '>' => {
                self.sc.advance().map_err(TokenizerError::IOError)?;
                self.if_equals_next_or(TokenType::RightShiftEquals, TokenType::BitwiseShiftRight)?
            }
            _ => TokenType::GreaterThan,
        };
        Ok(out)
    }

    fn lex_and(&mut self) -> Result<TokenType, TokenizerError> {
        let next = self.sc.peek().map_err(TokenizerError::IOError)?;
        let out = match next {
            Some(c) if c == '&' => {
                self.sc.advance().map_err(TokenizerError::IOError)?;
                TokenType::LogicalAnd
            },
            Some(_) | None => TokenType::BitwiseAnd,
        };
        Ok(out)
    }

    fn lex_or(&mut self) -> Result<TokenType, TokenizerError> {
        let next = self.sc.peek().map_err(TokenizerError::IOError)?;
        let out = match next {
            Some(c) if c == '|' => {
                self.sc.advance().map_err(TokenizerError::IOError)?;
                TokenType::LogicalOr
            },
            Some(_) | None => TokenType::BitwiseOr,
        };
        Ok(out)
    }

    fn if_equals_next_or(&mut self, val_true: TokenType, val_false: TokenType) -> Result<TokenType, TokenizerError> {
        let next = self.sc.peek().map_err(TokenizerError::IOError)?;
        match next {
            Some(c) if c == '=' => {
                Ok(val_true)
            },
            _ => Ok(val_false),
        }
    }

    /// Extracts an identifier string starting with the given character.
    ///
    /// This function takes an initial character and continues to read characters
    /// from the scanner until it encounters a non-alphanumeric or
    /// underscore character. It returns the collected string as an identifier.
    fn get_identifier_string(&mut self, c: char) -> Result<String, TokenizerError> {
        let mut out = String::new();
        out.push(c);

        loop {
            let next = self.sc.peek().map_err(TokenizerError::IOError)?;
            if let Some(c) = next{
                if c.is_ascii_alphanumeric() || c == '_' {
                    self.sc.advance().map_err(TokenizerError::IOError)?;
                    out.push(c);
                    continue;
                }
            }
            return Ok(out);
        }
    }

    /// Extracts an integer literal starting with the given character.
    fn lex_number(&mut self, c: char) -> Result<i32, TokenizerError> {
        let mut int_str = String::new();
        int_str.push(c);

        loop {
            let next = self.sc.peek().map_err(TokenizerError::IOError)?;
            if let Some(c) = next {
                if c.is_ascii_digit() {
                    self.sc.advance().map_err(TokenizerError::IOError)?;
                    int_str.push(c);
                    continue;
                }
            }
            // TODO: fix this mess
            return int_str.parse().map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "Failed to parse integer")).map_err(TokenizerError::IOError);
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
        matches!(self.token_type, TokenType::Minus | TokenType::BitwiseComplement | TokenType::Negate)
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
    BitwiseAnd,         // &
    BitwiseOr,          // |
    BitwiseShiftLeft,   // <<
    BitwiseShiftRight,  // >>
    BitwiseXor,         // ^

    LogicalAnd,         // &&
    LogicalOr,          // ||
    Equal,              // ==
    NotEqual,           // !=
    LessThan,           // <
    LessThanEq,         // <=
    GreaterThan,        // >
    GreaterThanEq,      // >=

    PlusEquals,         // +=
    MinusEquals,        // -=
    SlashEquals,        // /=
    StarEquals,         // *=
    PercentEquals,      // %=
    LeftShiftEquals,    // <<=
    RightShiftEquals,   // >>=
    AndEquals,          // &=
    OrEquals,           // |=
    XorEquals,          // ^=

    Assignment,         // = NOTE: currently unused
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
    If,
}

impl Keyword {
    fn as_str(&self) -> &str {
        match self {
            Keyword::Int => "int",
            Keyword::Return => "return",
            Keyword::If => "if",
        }
    }

    fn from_str(input: &str) -> Option<Keyword> {
        match input {
            "int" => Some(Keyword::Int),
            "return" => Some(Keyword::Return),
            "if" => Some(Keyword::If),
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
    fn peek_ahead() {
        let mut tokenizer = create_tokenizer("int main() { return 0; }");
        assert_eq!(tokenizer.peek_ahead(0).unwrap(), Token::new(TokenType::Keyword(Keyword::Int), 1));
        assert_eq!(tokenizer.peek_ahead(100).unwrap(), Token::new(TokenType::EOF, 1));
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
}