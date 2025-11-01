use std::fmt;
use std::num::ParseIntError;
use logos::Logos;

#[derive(Default, Debug, Clone, PartialEq)]
pub enum LexicalError {
    InvalidInteger(ParseIntError),
    #[default]
    InvalidToken,
}

impl From<ParseIntError> for LexicalError {
    fn from(err: ParseIntError) -> Self {
        LexicalError::InvalidInteger(err)
    }
}

#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\n\f]+",                     // white spaces
        skip r"//[^\n\r]*",                     // line-comment
        skip r"/\*[^*]*\*+(?:[^/*][^*]*\*+)*/", // block comment
        error = LexicalError)]
pub enum Token {
    // --- キーワード ---
    #[token("type" , priority = 10)] Type,
    #[token("trait", priority = 10)] Trait,
    #[token("impl" , priority = 10)] Impl,
    #[token("let"  , priority = 10)] Let,
    #[token("rec"  , priority = 10)] Rec,
    #[token("if"   , priority = 10)] If,
    #[token("else" , priority = 10)] Else,
    #[token("match", priority = 10)] Match,
    #[token("true" , priority = 10)] True,
    #[token("false", priority = 10)] False,
    #[token("()"   , priority = 10)] Unit,

    // --- 演算子・記号 ---
    #[token("*"    , priority = 6)]  Star,
    #[token("/"    , priority = 6)]  Slash,

    #[token("+"    , priority = 5)]  Plus,
    #[token("-"    , priority = 5)]  Minus,

    #[token("=="   , priority = 4)]  Eq,
    #[token("!="   , priority = 4)]  Neq,
    #[token("<"    , priority = 4)]  Lt,
    #[token("<="   , priority = 4)]  Le,
    #[token(">"    , priority = 4)]  Gt,
    #[token(">="   , priority = 4)]  Ge,

    #[token("&&"   , priority = 3)]  And,
    #[token("||"   , priority = 3)]  Or,

    // --- 記号 ---
    #[token("="    , priority = 3)]  Assign,
    #[token("->"   , priority = 3)]  Arrow,
    #[token("=>"   , priority = 3)]  FatArrow,
    #[token("@"    , priority = 3)]  At,
    #[token("\\"   , priority = 3)]  Backslash,
    #[token("λ"    , priority = 3)]  Lambda,
    #[token("_"    , priority = 3)]  Underscore,
    #[token("."    , priority = 3)]  Dot,
    #[token(";"    , priority = 3)]  Semi,
    #[token(","    , priority = 3)]  Comma,
    #[token(":"    , priority = 3)]  Colon,
    #[token("`"    , priority = 3)]  Backtick,
    #[token("("    , priority = 3)]  LParen,
    #[token(")"    , priority = 3)]  RParen,
    #[token("{"    , priority = 3)]  LBrace,
    #[token("}"    , priority = 3)]  RBrace,
    #[token("["    , priority = 3)]  LBracket,
    #[token("]"    , priority = 3)]  RBracket,
    #[token("|"    , priority = 3)]  VerticalBar,
    #[token("!"    , priority = 3)]  ExclamationMark,

    // --- 識別子・リテラル ---
    #[regex(r"[a-z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Ident(String),

    #[regex(r"[A-Z][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    ConIdent(String),

    #[regex(r"[+-]?[0-9]+", |lex| lex.slice().parse())]
    Int(i64),

    // --- その他の演算子記号列 ---
    #[regex(r"[*+\-/!$%&=^?<>]+", |lex| lex.slice().to_string())]
    InfixOp(String),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
