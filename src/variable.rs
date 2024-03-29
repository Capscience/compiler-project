use std::fmt::{self, Display};
use std::{collections::HashMap, mem::discriminant};

#[derive(Default, Debug)]
pub struct SymbolTable<T> {
    symbols: HashMap<String, T>,
    pub parent: Option<Box<SymbolTable<T>>>,
}

impl<T> SymbolTable<T>
where
    T: Default + PartialEq,
{
    pub fn new(parent: Option<Box<SymbolTable<T>>>) -> SymbolTable<T> {
        let symbols = HashMap::new();
        Self { symbols, parent }
    }

    pub fn get(&self, symbol: &String) -> Option<&T> {
        if let Some(value) = self.symbols.get(symbol) {
            Some(value)
        } else if let Some(parent) = &self.parent {
            parent.get(symbol)
        } else {
            None
        }
    }

    pub fn set(&mut self, symbol: String, new_value: T) -> Result<(), &str> {
        if let Some(value) = self.symbols.get(&symbol) {
            if discriminant(value) != discriminant(&new_value) {
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

    pub fn declare(&mut self, symbol: String, value: T) -> Result<String, &str> {
        if self.symbols.get(&symbol).is_some() {
            Err("Variable already exists!")
        } else {
            self.symbols.insert(symbol.clone(), value);
            Ok(symbol)
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

impl Default for Value {
    fn default() -> Self {
        Self::None
    }
}
