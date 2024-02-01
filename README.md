# Compiler project
This is a course project for University of Helsinki course CSM14204 Compilers.
The project is a simple end-to-end compiler for a simple language specified in the [course material](https://hy-compilers.github.io/spring-2024/).

## Steps
The project consists of the following steps:
1. Tokenizer
2. Parser
3. Interpreter
4. Type checker
5. IR generator
6. Assembly generator

My plan is to implement some functionality for each step first. After completing the whole pipeline for a small subset of the language, I will continue adding language features while refactoring and improving the code.

### 1. Tokenizer
Tokenizer code can be found in [tokenizer.rs](https://github.com/Capscience/compiler-project/blob/main/src/tokenizer.rs)

### 2. Parser
Parser code can be found in [parser.rs](https://github.com/Capscience/compiler-project/blob/main/src/parser.rs). Data structure for abstract syntax tree (AST) is in separate file [ast.rs](https://github.com/Capscience/compiler-project/blob/main/src/parser.rs)
