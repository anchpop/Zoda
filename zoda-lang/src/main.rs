#[macro_use] extern crate lalrpop_util;


lalrpop_mod!(pub calculator2b); // synthesized by LALRPOP

#[test]
fn calculator1() {
    assert!(calculator1::TermParser::new().parse("22").is_ok());
    assert!(calculator1::TermParser::new().parse("(22)").is_ok());
    assert!(calculator1::TermParser::new().parse("((((22))))").is_ok());
    assert!(calculator1::TermParser::new().parse("((22)").is_err());
}


fn main() {
    println!("{}", calculator1::TermParser::new().parse("(22").is_ok());
    
}