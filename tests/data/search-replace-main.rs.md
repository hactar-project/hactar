To modify `main.rs` to take a name and say hello to that name, we need to make the following changes:

1. Import the necessary modules for command-line argument handling.
2. Parse the command-line arguments to get the name.
3. Print a greeting message using the provided name.

Here are the *SEARCH/REPLACE* blocks:

```rust main.rs
<<<<<<< SEARCH
fn main() {
    println!("Hello, World!");
}
=======
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() > 1 {
        let name = &args[1];
        println!("Hello, {}!", name);
    } else {
        println!("Please provide a name.");
    }
}
>>>>>>> REPLACE
```
