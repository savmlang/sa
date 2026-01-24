use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Function {
  pub instructions: Vec<String>,
}

pub struct AssemblyParser {
  functions: HashMap<String, Function>,
}

impl AssemblyParser {
  pub fn new() -> Self {
    AssemblyParser {
      functions: HashMap::new(),
    }
  }

  pub fn parse(&mut self, content: &str) {
    let mut current_function: Option<String> = None;
    let mut current_instructions: Vec<String> = Vec::new();

    for line in content.lines() {
      let trimmed = line.trim();

      // Skip empty lines and comments
      if trimmed.is_empty() || trimmed.starts_with('#') {
        continue;
      }

      // Detect function definition (e.g., ".main:")
      if trimmed.starts_with('.') && trimmed.ends_with(':') {
        if let Some(func_name) = current_function.take() {
          self.functions.insert(
            func_name,
            Function {
              instructions: current_instructions.clone(),
            },
          );
        }
        current_function = Some(trimmed[1..trimmed.len() - 1].to_string());
        current_instructions.clear();
      } else if let Some(_) = &current_function {
        // Add instruction to current function
        current_instructions.push(trimmed.to_string());
      }
    }

    // Don't forget the last function
    if let Some(func_name) = current_function {
      self.functions.insert(
        func_name,
        Function {
          instructions: current_instructions,
        },
      );
    }
  }

  pub fn get_functions(&self) -> &HashMap<String, Function> {
    &self.functions
  }
}
