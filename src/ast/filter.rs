const EQ: &str = "eq";
const GT: &str = "gt";
const GTE: &str = "gte";
const LT: &str = "lt";
const LTE: &str = "lte";
const NEQ: &str = "neq";
const LIKE: &str = "like";
const ILIKE: &str = "ilike";
const MATCH: &str = "match";
const IMATCH: &str = "imatch";
const IN: &str = "in";
const IS: &str = "is";
const ISDISTINCT: &str = "isdistinct";
const FTS: &str = "fts";
const PLFTS: &str = "plfts";
const PHFTS: &str = "phfts";
const WFTS: &str = "wfts";
const CS: &str = "cs";
const CD: &str = "cd";
const OV: &str = "ov";
const SL: &str = "sl";
const SR: &str = "sr";
const NXR: &str = "nxr";
const NXL: &str = "nxl";
const ADJ: &str = "adj";
const NOT: &str = "not";
const OR: &str = "or";
const AND: &str = "and";
const ALL: &str = "all";
const ANY: &str = "any";

pub type Value = String;

#[derive(Debug, PartialEq, Clone)]
pub enum Filter {
    And(Vec<InnerFilter>),
    Or(Vec<InnerFilter>),
    One(InnerFilter),
    Not(Box<Filter>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct InnerFilter {
    path: Path,
    operator: Operator,
    value: Value,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Path {
    Leaf(String),
    Nested(String, Box<Path>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
    Equal,
    GreaterThan,
    GreaterThanEqual,
    LessThan,
    LessThanEqual,
    NotEqual,
    Like,
    ILike,
    Match,
    IMatch,
    In,
    Is,
    IsDistinct,
    FullTextSearch,
    PLFT,
    PHFT,
    WebFullTextSearch,
    Contains,
    Contained,
    Overlap,
    StrictlyLeft,
    StrictlyRight,
    NotExtentToTheRight,
    NotExtentToTheLeft,
    Adjacent,
    Not,
    Or,
    And,
    All,
    Any,
}
