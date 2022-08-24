#![no_main]
use libfuzzer_sys::fuzz_target;
use novelang::fuzz_utils::*;

fuzz_target!(|input: Vec<FuzzItem>| {
        let _ = fuzz_entry_point(input);
});
