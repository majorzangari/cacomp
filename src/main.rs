use std::env;

mod tokenizer;
mod scanner;
mod parser;
mod ast;
mod generator;


fn main() {
    let args: Vec<String> = env::args().collect();
    todo!("parse args");
}