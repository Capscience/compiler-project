use std::path::Path;

use compiler_project::compile;

fn main() {
    let source_code: &'static str = "
    // This is placeholder stuff for testing tokenization.
    fn testfunc(a/* inline comment */) {
        if a > 10 then print_int(a);
        // comment
        b = a;
        /* this
            is
            multiline comment */
        return b;
    }";
    compile(source_code, Path::new("executable"));
}
