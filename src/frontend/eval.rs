use koopa::ir::{Program, ValueKind};
use crate::frontend::ast::{AddExp, BinaryOp, EqExp, Exp, LAndExp, LOrExp, MulExp, PrimaryExp, RelExp, UnaryExp, UnaryOp};
use crate::frontend::context::{Context, CTValue, cur_func};

// 编译期计算表达式的值
pub trait Evaluate {
    fn eval(&self, program: &mut Program, context: &mut Context) -> Option<i32>;
}

impl Evaluate for Exp {
    fn eval(&self, program: &mut Program, context: &mut Context) -> Option<i32> {
        self.lor_exp.eval(program, context)
    }
}

impl Evaluate for LOrExp {
    fn eval(&self, program: &mut Program, context: &mut Context) -> Option<i32> {
        Some(match self {
            Self::And(exp) => {
                exp.eval(program, context)?
            }
            Self::Or(or, and) => {
                let left = or.eval(program, context)?;
                let right = and.eval(program, context)?;
                ((left != 0) || (right != 0)) as i32
            }
        })
    }
}

impl Evaluate for LAndExp {
    fn eval(&self, program: &mut Program, context: &mut Context) -> Option<i32> {
        Some(match self {
            Self::Eq(eq) => eq.eval(program, context)?,
            Self::And(and, eq) => {
                let left = and.eval(program, context)?;
                let right = eq.eval(program, context)?;
                ((left != 0) && (right != 0)) as i32
            }
        })
    }
}

impl Evaluate for EqExp {
    fn eval(&self, program: &mut Program, context: &mut Context) -> Option<i32> {
        Some(match self {
            Self::Eq(eq, op, rel) => {
                let left = eq.eval(program, context)?;
                let right = rel.eval(program, context)?;
                match op {
                    BinaryOp::Eq => (left == right) as i32,
                    BinaryOp::Neq => (left != right) as i32,
                    _ => unreachable!()
                }
            }
            Self::Rel(rel) => rel.eval(program, context)?
        })
    }
}

impl Evaluate for RelExp {
    fn eval(&self, program: &mut Program, context: &mut Context) -> Option<i32> {
        Some(match self {
            Self::Add(add) => add.eval(program, context)?,
            Self::Rel(rel, op, and) => {
                let left = rel.eval(program, context)?;
                let right = and.eval(program, context)?;
                match op {
                    BinaryOp::Gt => (left > right) as i32,
                    BinaryOp::Ge => (left >= right) as i32,
                    BinaryOp::Lt => (left < right) as i32,
                    BinaryOp::Le => (left <= right) as i32,
                    _ => unreachable!()
                }
            }
        })
    }
}

impl Evaluate for AddExp {
    fn eval(&self, program: &mut Program, context: &mut Context) -> Option<i32> {
        Some(match self {
            Self::Mul(mul) => mul.eval(program, context)?,
            Self::Add(add, op, mul) => {
                let left = add.eval(program, context)?;
                let right = mul.eval(program, context)?;
                match op {
                    BinaryOp::Add => left + right,
                    BinaryOp::Sub => left - right,
                    _ => unreachable!()
                }
            }
        })
    }
}

impl Evaluate for MulExp {
    fn eval(&self, program: &mut Program, context: &mut Context) -> Option<i32> {
        Some(match self {
            Self::Unary(unary) => unary.eval(program, context)?,
            Self::Mul(mul, op, unary) => {
                let left = mul.eval(program, context)?;
                let right = unary.eval(program, context)?;
                match op {
                    BinaryOp::Mul => left * right,
                    BinaryOp::Div => left / right,
                    BinaryOp::Mod => left % right,
                    _ => unreachable!()
                }
            }
        })
    }
}

impl Evaluate for UnaryExp {
    fn eval(&self, program: &mut Program, context: &mut Context) -> Option<i32> {
        Some(match self {
            Self::UnaryExpression(unary) => unary.eval(program, context)?,
            Self::UnaryOpAndExp(op, exp) => {
                let value = exp.eval(program, context)?;
                match op {
                    UnaryOp::Neg => -value,
                    UnaryOp::Not => !value
                }
            }
            Self::PrimaryExpression(exp) => exp.eval(program, context)?,
            Self::FunctionCall(..) => panic!("function call in constant val evaluation.")
        })
    }
}

impl Evaluate for PrimaryExp {
    fn eval(&self, program: &mut Program, context: &mut Context) -> Option<i32> {
        match self {
            Self::LValue(val) => {
                // 从本地符号表取
                let value = context.value(val.ident.as_str()).unwrap();
                match value {
                    // 常量值只能跟常量值一起计算
                    CTValue::Const(i32) => Some(i32),
                    _ => None
                }
            }
            Self::Number(i32) => Some(*i32),
            Self::Expression(exp) => exp.eval(program, context)
        }
    }
}