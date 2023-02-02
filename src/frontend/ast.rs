#[derive(Debug)]
pub struct CompUnit {
    pub func_def: FuncDef,
}

#[derive(Debug)]
pub struct FuncDef {
    pub func_type: FuncType,
    pub ident: String,
    pub block: Block,
}

#[derive(Debug)]
pub enum FuncType {
    Int
}

#[derive(Debug)]
pub enum Stmt {
    Ret(Exp),
    Assign(LVal, Exp),
    // 单个 ; 也是合法语句
    Exp(Option<Exp>),
    Block(Block)
}

#[derive(Debug)]
pub struct Exp {
    pub lor_exp: LOrExp
}

#[derive(Debug)]
pub enum PrimaryExp {
    Expression(Box<Exp>),
    LValue(LVal),
    Number(i32)
}

#[derive(Debug)]
pub enum UnaryExp {
    PrimaryExpression(PrimaryExp),
    UnaryExpression(Box<UnaryExp>),
    UnaryOpAndExp(UnaryOp, Box<UnaryExp>)
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    // -
    Neg,
    // !
    Not
}

#[derive(Debug)]
pub enum BinaryOp {
    // +
    Add,
    // -
    Sub,
    // *
    Mul,
    // /
    Div,
    // %
    Mod,
    // <
    Lt,
    // <=
    Le,
    // >
    Gt,
    // >=
    Ge,
    // ==
    Eq,
    // !=
    Neq,
}

// MulExp ::= UnaryExp | MulExp ("*" | "/" | "%") UnaryExp;
#[derive(Debug)]
pub enum MulExp {
    Unary(UnaryExp),
    Mul(Box<MulExp>, BinaryOp, UnaryExp)
}

// AddExp ::= MulExp | AddExp ("+" | "-") MulExp;
#[derive(Debug)]
pub enum AddExp {
    Mul(MulExp),
    Add(Box<AddExp>, BinaryOp, MulExp)
}

#[derive(Debug)]
pub enum RelExp {
    Add(AddExp),
    Rel(Box<RelExp>, BinaryOp, AddExp)
}

#[derive(Debug)]
pub enum EqExp {
    Rel(RelExp),
    Eq(Box<EqExp>, BinaryOp, RelExp)
}

#[derive(Debug)]
pub enum LAndExp {
    Eq(EqExp),
    And(Box<LAndExp>, EqExp)
}

#[derive(Debug)]
pub enum LOrExp {
    And(LAndExp),
    Or(Box<LOrExp>, LAndExp)
}

#[derive(Debug)]
pub enum Decl {
    Const(ConstDecl),
    Var(VarDecl)
}

#[derive(Debug)]
pub struct ConstDecl {
    pub const_defs: Vec<ConstDef>
}

#[derive(Debug)]
pub struct ConstDef {
    pub ident: String,
    pub val: ConstInitVal
}

#[derive(Debug)]
pub struct ConstInitVal {
    pub exp: ConstExp,
}

#[derive(Debug)]
pub struct Block {
    pub block_items: Vec<BlockItem>
}

#[derive(Debug)]
pub enum BlockItem {
    Declare(Decl),
    Statement(Stmt)
}

#[derive(Debug)]
pub struct LVal {
    pub ident: String
}

#[derive(Debug)]
pub struct ConstExp {
    pub exp: Exp
}

#[derive(Debug)]
pub struct VarDecl {
    pub defs: Vec<VarDef>
}

#[derive(Debug)]
pub enum VarDef {
    NotInit { ident: String },
    Init { ident: String, val: InitVal }
}

#[derive(Debug)]
pub struct InitVal {
    pub exp: Exp
}

