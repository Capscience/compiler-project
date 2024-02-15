use std::fmt::{self, Display};
use std::{collections::HashMap, mem::discriminant};

#[derive(Default)]
pub struct SymbolTable {
    symbols: HashMap<String, Value>,
    pub parent: Option<Box<SymbolTable>>,
}

impl SymbolTable {
    pub fn new(parent: Option<Box<SymbolTable>>) -> SymbolTable {
        let symbols = HashMap::new();
        Self { symbols, parent }
    }

    pub fn get(&self, symbol: &String) -> Option<&Value> {
        if let Some(value) = self.symbols.get(symbol) {
            Some(value)
        } else if let Some(parent) = &self.parent {
            parent.get(symbol)
        } else {
            None
        }
    }

    pub fn set(&mut self, symbol: String, new_value: Value) -> Result<(), &str> {
        if let Some(value) = self.symbols.get(&symbol) {
            if value != &Value::None && discriminant(value) != discriminant(&new_value) {
                return Err("Incompatible types!");
            }
            self.symbols.insert(symbol, new_value);
            return Ok(());
        }
        if let Some(parent) = &mut self.parent {
            return parent.set(symbol, new_value);
        }
        Ok(())
    }

    pub fn declare(&mut self, symbol: String) -> Result<(), &str> {
        if self.get(&symbol).is_some() {
            Err("Variable already exists!")
        } else {
            self.symbols.insert(symbol, Value::None);
            Ok(())
        }
    }
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum Value {
    Int { value: i64 },
    Bool { value: bool },
    None,
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Int { value } => write!(f, "{}", value),
            Value::Bool { value } => write!(f, "{}", value),
            Value::None => write!(f, "None"),
        }
    }
}
