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
    #[token("mod"  , priority = 10)] Mod,
    #[token("use"  , priority = 10)] Use,
    #[token("as"   , priority = 10)] As,
    #[token("type" , priority = 10)] Type,
    #[token("trait", priority = 10)] Trait,
    #[token("impl" , priority = 10)] Impl,
    #[token("*let" , priority = 10)] Starlet,
    #[token("let"  , priority = 10)] Let,
    #[token("rec"  , priority = 10)] Rec,
    #[token("if"   , priority = 10)] If,
    #[token("else" , priority = 10)] Else,
    #[token("match", priority = 10)] Match,
    #[token("__for__", priority = 10)] For,
    #[token("true" , priority = 10)] True,
    #[token("false", priority = 10)] False,
    #[token("()"   , priority = 10)] Unit,
    #[token("@[]"  , priority = 10)] EmptyArray,
    #[token("u8"   , priority = 10)] U8Type,
    #[token("u16"  , priority = 10)] U16Type,
    #[token("u32"  , priority = 10)] U32Type,
    #[token("u64"  , priority = 10)] U64Type,
    // #[token("i8"   , priority = 10)] I8Type,  // (reserved)
    // #[token("i16"  , priority = 10)] I16Type, // (reserved)
    // #[token("i32"  , priority = 10)] I32Type, // (reserved)
    // #[token("i64"  , priority = 10)] I64Type, // (reserved)

    // --- 演算子・記号 ---
    #[token(".."   , priority = 7)]  DotDot,
    #[token("::"   , priority = 7)]  ColonColon, // path separator
    #[token("|("   , priority = 7)]  SectionStart,

    #[token("*"    , priority = 6)]  Star,
    #[token("/"    , priority = 6)]  Slash,
    #[token("%"    , priority = 6)]  Percent,

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

    #[token(">>"   , priority = 4)]  ComposeR,
    #[token("<<"   , priority = 4)]  ComposeL,
    #[token("|>"   , priority = 4)]  PipeR,
    #[token("<|"   , priority = 4)]  PipeL,

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
    #[token("#"    , priority = 3)]  NumberSign,

    // --- 識別子・リテラル ---
    #[regex(r"[a-z_]([-]*[a-zA-Z0-9_]+)*::", |lex| lex.slice().trim_end_matches("::").to_string())]
    ModIdent(String),

    #[regex(r"[a-z_]([-]*[a-zA-Z0-9_]+)*['?]*", |lex| lex.slice().to_string())]
    Ident(String),

    #[regex(r"[A-Z][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    ConIdent(String),

    #[regex(r"[+-]?[0-9]+", |lex| lex.slice().parse())]
    Int(i64),

    #[regex(r"(0x[0-9a-fA-F]+|[0-9]+)u8", |lex| parse_u8_literal(lex.slice()))]
    LitU8(u8),

    #[regex(r"(0x[0-9a-fA-F]+|[0-9]+)u16", |lex| parse_u16_literal(lex.slice()))]
    LitU16(u16),

    #[regex(r"(0x[0-9a-fA-F]+|[0-9]+)u32", |lex| parse_u32_literal(lex.slice()))]
    LitU32(u32),

    #[regex(r"(0x[0-9a-fA-F]+|[0-9]+)u64", |lex| parse_u64_literal(lex.slice()))]
    LitU64(u64),

    // --- 文字リテラル ---
    #[regex(r"'([^'\\]|\\.)'", parse_char_literal)]
    LitUnicodeScalarValue(u32),

    // --- 文字列リテラル ---
    #[token("\"", parse_string_literal)]
    LitUTF8(Vec<u8>),

    // --- その他の演算子記号列 ---
    #[regex(r"[*+\-/!$%&=^?<>]+", |lex| lex.slice().to_string())]
    InfixOp(String),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

fn parse_u8_literal(s: &str) -> Result<u8, LexicalError> {
    let body = &s[..s.len()-2];
    let parsed = if let Some(hex) = body.strip_prefix("0x") {
        u8::from_str_radix(hex, 16)
    } else {
        body.parse::<u8>()
    };
    parsed.map_err(|_| LexicalError::InvalidToken)
}

fn parse_u16_literal(s: &str) -> Result<u16, LexicalError> {
    let body = &s[..s.len()-3];
    let parsed = if let Some(hex) = body.strip_prefix("0x") {
        u16::from_str_radix(hex, 16)
    } else {
        body.parse::<u16>()
    };
    parsed.map_err(|_| LexicalError::InvalidToken)
}

fn parse_u32_literal(s: &str) -> Result<u32, LexicalError> {
    let body = &s[..s.len()-3];
    let parsed = if let Some(hex) = body.strip_prefix("0x") {
        u32::from_str_radix(hex, 16)
    } else {
        body.parse::<u32>()
    };
    parsed.map_err(|_| LexicalError::InvalidToken)
}

fn parse_u64_literal(s: &str) -> Result<u64, LexicalError> {
    let body = &s[..s.len()-3];
    let parsed = if let Some(hex) = body.strip_prefix("0x") {
        u64::from_str_radix(hex, 16)
    } else {
        body.parse::<u64>()
    };
    parsed.map_err(|_| LexicalError::InvalidToken)
}


// --- Unicode Scalar Value and UTF-8 String literals ---

fn next_char(s: &str, pos: usize) -> Result<(char, usize), LexicalError> {
    let ch = s[pos..].chars().next().ok_or(LexicalError::InvalidToken)?;
    Ok((ch, ch.len_utf8()))
}

fn parse_char_literal(lex: &mut logos::Lexer<Token>) -> Result<u32, LexicalError> {
    let slice = lex.slice(); // 例: "'a'", "'\n'", "'\u{1F600}'"
    let inner = &slice[1..slice.len()-1];

    let mut pos = 0;
    let ch = parse_char_from(inner, &mut pos)?;

    // 1 文字分だけ消費していることを確認（ゴミが残ってないか）
    if pos != inner.len() {
        return Err(LexicalError::InvalidToken);
    }

    Ok(ch as u32)
}

fn parse_char_from(s: &str, pos: &mut usize) -> Result<char, LexicalError> {
    if *pos >= s.len() {
        return Err(LexicalError::InvalidToken);
    }
    let (ch, len) = next_char(s, *pos)?;
    *pos += len;

    if ch == '\\' {
        // エスケープシーケンス
        parse_escape_from(s, pos)
    } else {
        Ok(ch)
    }
}

fn parse_string_literal(lex: &mut logos::Lexer<Token>) -> Result<Vec<u8>, LexicalError> {
    let src = lex.remainder(); // 開きの " の直後から
    let mut pos = 0;
    let mut out = String::new();

    while pos < src.len() {
        let (ch, len) = next_char(src, pos)?;
        pos += len;

        match ch {
            '"' => {
                // ここまで（中身＋閉じ "）を消費したことにする
                lex.bump(pos);
                return Ok(out.into_bytes());
            }
            '\\' => {
                let esc = parse_escape_from(src, &mut pos)?;
                out.push(esc);
            }
            _ => out.push(ch),
        }
    }

    Err(LexicalError::InvalidToken) // 閉じ " が来なかった
}

fn parse_escape_from(s: &str, pos: &mut usize) -> Result<char, LexicalError> {
    let (ch, len) = next_char(s, *pos)?;
    *pos += len;

    match ch {
        'n'  => Ok('\n'),
        'r'  => Ok('\r'),
        't'  => Ok('\t'),
        '0'  => Ok('\0'),
        '\\' => Ok('\\'),
        '"'  => Ok('"'),
        '\'' => Ok('\''),
        'u'  => parse_unicode_escape_from(s, pos),
        _    => Err(LexicalError::InvalidToken),
    }
}

fn parse_unicode_escape_from(s: &str, pos: &mut usize) -> Result<char, LexicalError> {
    let (brace, len) = next_char(s, *pos)?;
    if brace != '{' {
        return Err(LexicalError::InvalidToken);
    }
    *pos += len;

    let start = *pos;
    while *pos < s.len() {
        let (ch, clen) = next_char(s, *pos)?;
        if ch == '}' {
            let hex = &s[start..*pos];
            *pos += clen;

            let value = u32::from_str_radix(hex, 16)
                .map_err(|_| LexicalError::InvalidToken)?;
            return std::char::from_u32(value).ok_or(LexicalError::InvalidToken);
        }
        *pos += clen;
    }

    Err(LexicalError::InvalidToken)
}
