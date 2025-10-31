use super::Expr;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RawConstraint {
    pub name: String,           // trait name (ex. Eq)
    pub params: Vec<RawType>,   // instance type params (ex. [Int])
}

#[derive(Debug, Clone)]
pub struct RawTraitDecl {
    pub name: String,
    pub params: Vec<String>,
    pub members: Vec<RawTraitMemberDecl>,
}

#[derive(Debug, Clone)]
pub struct RawTraitMemberDecl{
    pub name: String,
    pub ty: Box<RawType>,
}

#[derive(Debug, Clone)]
pub struct RawImplDecl {
    pub name: String,
    pub params: Vec<RawType>,
    pub members: Vec<RawImplMemberDecl>,
}

#[derive(Debug, Clone)]
pub struct RawImplMemberDecl {
    pub name: String,
    pub expr: Box<Expr>,
}

/// パース直後の型定義（まだ型変数は名前のまま）
#[derive(Debug, Clone)]
pub enum RawTypeDecl {
    /// 代数的データ型 (sum type)
    SumType {
        name: String,              // 型名 (例: "Option")
        params: Vec<String>,       // 型パラメータ名 (例: ["a"])
        variants: Vec<RawVariant>, // バリアント群
    },
}

/// バリアント(データ構築子)定義
#[derive(Debug, Clone)]
pub enum RawVariant {
    /// 単位バリアント (例: None)
    Unit(String),

    /// タプルバリアント (例: Some a)
    Tuple(String, Vec<RawType>),
}

/// 型式（まだ名前ベース）
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RawType {
    /// 型変数 (例: "a")
    VarName(String),

    /// 型コンストラクタ (例: "Int", "Option")
    ConName(String),

    /// 型適用 (例: "Option a", "Result a b")
    App(Box<RawType>, Box<RawType>),

    /// 関数型 (例: "a -> b")
    Fun(Box<RawType>, Box<RawType>),

    /// タプル型 (例: "(a,)", "(a, b)")
    Tuple(Vec<RawType>),

    /// レコード型 (例; "@{x:a, y:b}")
    Record(Vec<(String, RawType)>),
}
