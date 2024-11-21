use std::collections::VecDeque;
use std::io::{BufReader, Read, Result};

// TODO: add support for multi-byte characters

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