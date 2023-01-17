use std::borrow::Borrow;
use std::ops::Deref;
use crate::frontend::ast::PrimaryExp::{Expression, Number};
use crate::frontend::ast::UnaryExp::{PrimaryExpression, UnaryOpAndExp};

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
pub struct Block {
    pub stmt: Stmt
}

#[derive(Debug)]
pub struct Stmt {
    pub exp: Exp
}

#[derive(Debug)]
pub struct Exp {
    pub unary_exp: UnaryExp
}

#[derive(Debug)]
pub enum PrimaryExp {
    Expression(Box<Exp>),
    Number(i32)
}

#[derive(Debug)]
pub enum UnaryExp {
    PrimaryExpression(PrimaryExp),
    UnaryOpAndExp(UnaryOp, Box<UnaryExp>)
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    // -
    Neg,
    // !
    Not
}