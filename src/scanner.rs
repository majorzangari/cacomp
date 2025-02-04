use std::collections::VecDeque;
use std::io::{BufReader, Read, Result};

pub struct Scanner<R: Read> {
    br: BufReader<R>,
    buffer: VecDeque<char>,
}

impl<R: Read> Scanner<R> {
    /// Create a new scanner from a reader.
    pub fn new(reader: R) -> Scanner<R> {
        Scanner {
            br: BufReader::new(reader),
            buffer: VecDeque::new(),
        }
    }

    /// Advance to the next character.
    pub fn advance(&mut self) -> Result<()> {
        self.next()?;
        Ok(())
    }

    /// Advance to the end of the line, do nothing if at end of stream
    pub fn advance_line(&mut self) -> Result<()> {
        while let Some(c) = self.next()? {
            if c == '\n' {
                return Ok(())
            }
        }
        Ok(())
    }

    /// Peek at the next character without consuming it.
    pub fn peek(&mut self) -> Result<Option<char>> {
        if self.buffer.is_empty() {
            let mut buffer = [0; 1];
            if self.br.read(&mut buffer)? == 0 {
                return Ok(None);
            }
            self.buffer.push_back(buffer[0] as char);
        }
        Ok(self.buffer.front().cloned())
    }

    /// Get the next character, consuming it.
    pub fn next(&mut self) -> Result<Option<char>> {
        if self.buffer.is_empty() {
            let mut buffer = [0; 1];
            if self.br.read(&mut buffer)? == 0 {
                return Ok(None);
            }
            return Ok(Some(buffer[0] as char));
        }
        Ok(self.buffer.pop_front())
    }
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;
    use super::*;

    fn create_scanner(input: &str) -> Scanner<Cursor<&[u8]>> {
        Scanner::new(Cursor::new(input.as_bytes()))
    }

    #[test]
    fn peek() {
        let mut scanner = create_scanner("abc");
        assert_eq!(scanner.peek().unwrap(), Some('a'));
        assert_eq!(scanner.peek().unwrap(), Some('a'));
    }

    #[test]
    fn next() {
        let mut scanner = create_scanner("abc");
        assert_eq!(scanner.next().unwrap(), Some('a'));
        assert_eq!(scanner.next().unwrap(), Some('b'));
        assert_eq!(scanner.next().unwrap(), Some('c'));
        assert_eq!(scanner.next().unwrap(), None);
    }

    #[test]
    fn advance_line() {
        let mut scanner = create_scanner("1hello\n2\n3hello");
        assert_eq!(scanner.next().unwrap(), Some('1'));
        scanner.advance_line().unwrap();
        assert_eq!(scanner.next().unwrap(), Some('2'));
        scanner.advance_line().unwrap();
        assert_eq!(scanner.next().unwrap(), Some('3'));
        scanner.advance().unwrap();
        scanner.advance().unwrap();
        scanner.advance().unwrap();
    }
}