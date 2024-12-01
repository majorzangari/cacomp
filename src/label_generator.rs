use std::collections::HashMap;

#[derive(Debug)]
pub(crate) struct LabelGenerator {
    label_count: u32,
    max_offset: u32,
    var_map: HashMap<String, u32>,
}

impl LabelGenerator {
    pub fn new() -> LabelGenerator {
        LabelGenerator {
            label_count: 0,
            max_offset: 0,
            var_map: HashMap::new(),
        }
    }

    pub(crate) fn generate_label(&mut self) -> String {
        let label = format!(".L{}", self.label_count);
        self.label_count += 1;
        label
    }

    pub(crate) fn generate_or_get_offset(&mut self, id: String) -> u32 {
        let element = self.var_map.get(&id);
        match element {
            Some(v) => v.clone(),
            None => {
                let new_index = self.max_offset + 4;
                self.max_offset = new_index;
                self.var_map.insert(id, new_index);
                new_index
            }
        }
    }

    pub(crate) fn is_declared(&mut self, id: String) -> bool {
        self.var_map.contains_key(&id)
    }

    pub(crate) fn reset_vars(&mut self) {
        self.var_map.clear();
        self.max_offset = 0;
    }
}