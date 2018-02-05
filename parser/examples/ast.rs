extern crate parser;

use std::fs::File;
use std::env::args;
use std::io::Read;

fn main() {
    let filename = args().skip(1).next().unwrap();
    let mut f = File::open(&filename).unwrap();
    let mut s = String::new();
    f.read_to_string(&mut s).unwrap();
    match parser::parse(&s) {
        Ok(_) => println!("Parse OK!"),
        Err(e) => {
            println!("{}", filename);
            println!("{:?}", e);
            println!("remaining = {}",
                &s[e.offset..][..usize::min(100, s.len() - e.offset)]
            );
            panic!("Failed to parse!");
        }
    }
}
