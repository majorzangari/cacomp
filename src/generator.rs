use crate::ast::{Expression, Function, Program};

// TODO: get better name for this
pub struct Generator {
    instr: Vec<String>,
}

impl Generator {
    pub fn new(program: Program) -> Generator {
        let mut out = Generator { instr: Vec::new() };

        out
    }

    fn generate_instructions(&mut self, program: Program) {
        self.instr.push("global _start".to_string());
        self.instr.push("_start:".to_string());
    }

    fn generate_function_instructions(&mut self, function: Function) {
        todo!();
        match function.id.as_str() {
            "main" => {

            }
            _ => panic!(),
        }
    }

}