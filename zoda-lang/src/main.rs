//extern crate whitespace;
extern crate regex;

use regex::Regex;

const TEST_STR: &'static str = "\
module adding-utils `Collection of utilities related to adding values`
  author: Andre Popovitch
  user-doc: 
    These utilities are part of the greater \"adding\" library. 
    They're useful in all situations where you need to add something.
    
    -- maybe include more info here on when to use these?
    If you need to add 3, check out the `plus-3` function. Other useful 
    functions include `plus-4` and `plus-5`.           
    
    Examples: 
    
    >> 3.plus-4 
    -> 7
    
    >> 3.plus-5
    -> 8
  devl-doc:
    Research is ongoing whether a `x.plus-6` function is possible. Contributions would be welcomed!
  importing:
    (plus) from generic-plus           -- import the \"plus\" function from the module \"generic-plus\".
    negation:negation                  -- import the \"negation\" module from the \"negation\" package
  exporting:
    plus-3, plus-4, plus-5, plus-negative
              
x.plus-0 = x -- not exported because it's useless
x.plus-3 = x.plus(3) -- these would typically have their own tests, tiny-docs, user-docs, and devl-docs, but I've left them off for the sake of brevity.
x.plus-4 = x.plus(4) 
x.plus-5 `Add {x} and 5` = 
    res
  where:
    res = x.(_.plus(5))
  user-doc: 
    Adds 5 to a value, or adds a value to 5, whatever way you want to think about it 
  devl-doc: 
    This function is unrealistically convoluted, it's designed to showcase many different features within Zoda
x.plus-negative(y) = x.plus(y.negation:negate)         -- namespacing is done with `:`
";
const INDENTATION_CHANGE_THRESH: usize = 2;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum SourcePos {
  SourcePos { line: usize, column: usize }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Token {
  StartLine(SourcePos),
  Indent,
  Dedent,
  Newline(SourcePos),
  EOF(SourcePos),

  Module(SourcePos, String),
  UppercaseIdentifier(SourcePos, String),
  LowercaseIdentifier(SourcePos, String),
  Whitespace(SourcePos, String),
  TinyDoc(SourcePos, String),
  Comment(SourcePos, String),
  Colon(SourcePos, String),
  Match(SourcePos, String),
  OpenParen(SourcePos, String),
  CloseParen(SourcePos, String),
  Dot(SourcePos, String),
  Arrow(SourcePos, String),
  Where(SourcePos, String),
  UserDoc(SourcePos, String),
  DevlDoc(SourcePos, String),
  StringTok(SourcePos, String),
  EqualTok(SourcePos, String),
  DivTok(SourcePos, String),
  PlusTok(SourcePos, String),
  TimesTok(SourcePos, String),
  SubTok(SourcePos, String)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum LexicalError {
    UnknownToken(SourcePos, String),
    BadIndentation(SourcePos)
}


fn tokenizeLine(line: &str, current_column: usize, current_line: usize) -> Result<Vec<Token>, LexicalError>  {
  let mut line_tokens: Vec<Token> = Vec::new();
  if line == "" {
    return Ok(line_tokens);
  }

  let lexes: [(&'static str, fn(SourcePos, String) -> Token); 21] = [
               (r":", Token::Colon)
             , (r"=", Token::EqualTok)
             , (r"\+", Token::PlusTok)
             , (r"/", Token::DivTok)
             , (r"\*", Token::TimesTok)
             , (r"-", Token::SubTok)
             , (r"\(", Token::OpenParen)
             , (r"\)", Token::CloseParen)
             , (r"\.", Token::Dot)
             , (r"->", Token::Arrow)
             , ("\"[^\"]*\"", Token::StringTok)
             , (r"where", Token::Where)
             , (r"user-doc", Token::UserDoc)
             , (r"devl-doc", Token::DevlDoc)
             , (r"match", Token::Match)
             , (r"-- .*$", Token::Comment)
             , (r"`[^`]+`", Token::TinyDoc)
             , (r"\s+", Token::Whitespace)
             , (r"module", Token::Module)
             , (r"[^\s\-+/*\\`=a-z][^\s+/*\\`=]*", Token::UppercaseIdentifier)
             , (r"[^\s\-+/*\\`=A-Z][^\s+/*\\`=]*", Token::LowercaseIdentifier)];
  let mut lexesReg = Vec::new();//[(Regex::new(r"^(?P<token>module)").unwrap(), Token::Module)];
  for (r, m) in lexes.iter() {
    let reg = format!("^(?P<token>{})", r);
    lexesReg.push((Regex::new(&reg).unwrap(), m));
  }

  



  for (reg, tok) in lexesReg.iter() {
    let cap = reg.captures(line);
    let matched = &(match cap {
      Some(m) => m,
      None => continue
    })["token"];
    line_tokens.push(tok(SourcePos::SourcePos { line: current_line, column: current_column }, String::from(matched)));

    let match_len = matched.chars().count();
    let rest_of_line = &line[match_len..];
    let new_column = current_column + match_len;

    let rest_of_tokens = tokenizeLine(rest_of_line, new_column, current_line);
    return match rest_of_tokens {
      Ok(t) => {
        line_tokens.extend(t);
        Ok(line_tokens)
      },
      Err(e) => Err(e)
    }
  }

  
  return Err(LexicalError::UnknownToken(SourcePos::SourcePos { line: current_line, column: current_column }, line.to_string()))
} 


fn tokenize(s: &str) -> Result<Vec<Token>, LexicalError> {

  let mut current_line = 0;
  let mut current_column = 0;
  let mut old_indentation_level = 0;
  let mut tokens = Vec::new();

  for line in s.split("\n")
  {
    current_line += 1;
    current_column = 0;
    let mut line_tokens = Vec::new();
    let mut significant_tokens_on_line = false;

    line_tokens.push(Token::StartLine(SourcePos::SourcePos { line: current_line, column: current_column }));
    let num_spaces = line.chars().take_while(|c| c.is_whitespace()).count();
    if num_spaces % INDENTATION_CHANGE_THRESH != 0 {
      return Err(LexicalError::BadIndentation(SourcePos::SourcePos{line: current_line, column: current_column}));
    }
    let indentation_level = num_spaces / INDENTATION_CHANGE_THRESH;
    
    if indentation_level > old_indentation_level
    {
      line_tokens.extend(vec![Token::Indent; indentation_level - old_indentation_level]);
    }
    else {
      line_tokens.extend(vec![Token::Dedent; old_indentation_level - indentation_level]);
    }
    current_column += num_spaces;

    let rest_of_line = &line[num_spaces..];
    let rest_of_tokens = tokenizeLine(rest_of_line, current_column, current_line);
    match rest_of_tokens 
    {
      Err(e) => return Err(e),
      Ok(r)  => {
        line_tokens.extend(r);
        significant_tokens_on_line = true;
      }
    }

    line_tokens.push(Token::Newline(SourcePos::SourcePos { line: current_line, column: current_column }));

    if significant_tokens_on_line {
      tokens.extend(line_tokens);
      old_indentation_level = indentation_level;
    }
  }
  Ok(tokens)
}

fn main() {
  let mut current_indentation_level = 0;

  
  println!("{:?}", tokenize(TEST_STR));


    

  println!("test!");
  let re = Regex::new(r"(\d{4})-(\d{2})-(\d{2})").unwrap();

  for caps in re.captures_iter(TEST_STR) {
    // Note that all of the unwraps are actually OK for this regex
    // because the only way for the regex to match is if all of the
    // capture groups match. This is not true in general though!
    println!("year: {}, month: {}, day: {}",
              caps.get(1).unwrap().as_str(),
              caps.get(2).unwrap().as_str(),
              caps.get(3).unwrap().as_str());
  }
}

/*
fn main() {
    use std::io::*;

    let mut source = String::new();
    match std::env::args().nth(1) {
        
        Some(filename) => {
            use std::fs::File;

            File::open(&filename)
                .expect(&format!("Can't open {}", &filename))
                .read_to_string(&mut source)
                .expect(&format!("Can't read contents of {}", &filename));
        }

        None => {
            stdin()
                .read_to_string(&mut source)
                .expect("Can't read stdin");
        }
    }

    if source.is_empty() {
        println!("Empty file");
        return;
    }

    whitespace::compile(&source).expect("OH NO").interpret();
}
*/