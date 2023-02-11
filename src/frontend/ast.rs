#[derive(Debug)]
pub struct CompUnit {
    pub global_defs: Vec<GlobalDef>,
}

#[derive(Debug)]
pub enum GlobalDef {
    Func(FuncDef),
    Decl(Decl)
}

#[derive(Debug)]
pub struct FuncDef {
    pub func_type: FuncType,
    pub ident: String,
    pub block: Block,
    pub params: Option<FuncFParams>
}

#[derive(Debug)]
pub enum FuncType {
    Int,
    Void
}

#[derive(Debug, Clone)]
pub struct FuncFParams {
    pub params: Vec<FuncFParam>
}

#[derive(Debug, Clone)]
pub struct FuncFParam {
    pub ident: String
}

#[derive(Debug)]
pub enum Stmt {
    Ret(Exp),
    Assign(LVal, Exp),
    // 单个 ; 也是合法语句
    Exp(Option<Exp>),
    Block(Block),
    If { cond: Exp, then: Box<Stmt>, else_then: Option<Box<Stmt>> },
    While { cond: Exp, stmt: Box<Stmt> },
    Break,
    Continue
}

#[derive(Debug, Clone)]
pub struct Exp {
    pub lor_exp: LOrExp
}

#[derive(Debug, Clone)]
pub enum PrimaryExp {
    Expression(Box<Exp>),
    LValue(LVal),
    Number(i32)
}

#[derive(Debug, Clone)]
pub enum UnaryExp {
    PrimaryExpression(PrimaryExp),
    UnaryExpression(Box<UnaryExp>),
    UnaryOpAndExp(UnaryOp, Box<UnaryExp>),
    FunctionCall(String, Vec<Exp>)
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    // -
    Neg,
    // !
    Not
}

#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
pub enum MulExp {
    Unary(UnaryExp),
    Mul(Box<MulExp>, BinaryOp, UnaryExp)
}

// AddExp ::= MulExp | AddExp ("+" | "-") MulExp;
#[derive(Debug, Clone)]
pub enum AddExp {
    Mul(MulExp),
    Add(Box<AddExp>, BinaryOp, MulExp)
}

#[derive(Debug, Clone)]
pub enum RelExp {
    Add(AddExp),
    Rel(Box<RelExp>, BinaryOp, AddExp)
}

#[derive(Debug, Clone)]
pub enum EqExp {
    Rel(RelExp),
    Eq(Box<EqExp>, BinaryOp, RelExp)
}

#[derive(Debug, Clone)]
pub enum LAndExp {
    Eq(EqExp),
    And(Box<LAndExp>, EqExp)
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct InitVal {
    pub exp: Exp
}

impl InitVal {
    pub fn into_const(self) -> ConstInitVal {
        ConstInitVal { exp: ConstExp { exp: self.exp } }
    }
}

