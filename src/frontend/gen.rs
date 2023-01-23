use std::error::Error;
use koopa::back::KoopaGenerator;
use koopa::ir::{BinaryOp, FunctionData, Program, Type, Value};
use koopa::ir::builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder};
use crate::frontend::ast;
use crate::frontend::ast::{AddExp, Block, EqExp, Exp, FuncDef, FuncType, LAndExp, LOrExp, MulExp, PrimaryExp, RelExp, Stmt, UnaryExp, UnaryOp};
use crate::frontend::context::{Context, cur_func, FunctionInfo};
use super::ast::CompUnit;

pub fn compile_koopa(ast: CompUnit) -> Result<String, Box<dyn Error>> {
    let mut program = Program::new();

    // let func = program.new_func(FunctionData::with_param_names(format!("@{}", ast.func_def.ident), vec![], Type::get_i32()));
    // let func_data = program.func_mut(func);
    // let entry = func_data.dfg_mut().new_bb().basic_block(Some("%entry".into()));
    // func_data.layout_mut().bbs_mut().extend([entry]);
    // let ret_value = func_data.dfg_mut().new_value().integer(ast.func_def.block.stmt.num);
    // let ret = func_data.dfg_mut().new_value().ret(Some(ret_value));
    // func_data.layout_mut().bb_mut(entry).insts_mut().push_key_back(ret).unwrap();

    ast.generate(&mut program, &mut Context::new())?;
    let mut gen = KoopaGenerator::new(vec![]);
    gen.generate_on(&program).expect("koopa ir generation failed");
    Ok(std::str::from_utf8(&gen.writer()).unwrap().to_string())
}

trait ProgramGen {
    type Out;
    fn generate(&self, program: &mut Program, context: &mut Context) -> Result<Self::Out, Box<dyn Error>>;
}

impl ProgramGen for CompUnit {
    type Out = ();

    fn generate(&self, program: &mut Program, context: &mut Context) -> Result<Self::Out, Box<dyn Error>> {
        self.func_def.generate(program, context)?;
        Ok(())
    }
}

impl ProgramGen for FuncDef {
    type Out = ();

    fn generate(&self, program: &mut Program, context: &mut Context) -> Result<Self::Out, Box<dyn Error>> {
        let mut func_data = FunctionData::with_param_names(format!("@{}", self.ident), vec![], self.func_type.generate(program, context)?);

        let entry = func_data.dfg_mut().new_bb().basic_block(Some("%entry".into()));

        let func = program.new_func(func_data);
        let mut info = FunctionInfo::new(func, entry);
        info.push_bb(program, entry);
        context.cur_func = Some(info);
        self.block.generate(program, context)
    }
}

impl ProgramGen for FuncType {
    type Out = Type;

    fn generate(&self, _program: &mut Program, _context: &mut Context) -> Result<Self::Out, Box<dyn Error>> {
        Ok(match self {
            Self::Int => Type::get_i32()
        })
    }
}

impl ProgramGen for Block {
    type Out = ();
    fn generate(&self, program: &mut Program, context: &mut Context) -> Result<Self::Out, Box<dyn Error>> {
        self.stmt.generate(program, context)
    }
}

impl ProgramGen for Stmt {
    type Out = ();
    fn generate(&self, program: &mut Program, context: &mut Context) -> Result<Self::Out, Box<dyn Error>> {
        let ret_value = self.exp.generate(program, context)?;
        // 处理返回值
        let ret = cur_func!(context).new_value(program).ret(Some(ret_value));
        cur_func!(context).push_inst(program, ret);
        Ok(())
    }
}

impl ProgramGen for Exp {
    type Out = Value;

    fn generate(&self, program: &mut Program, context: &mut Context) -> Result<Self::Out, Box<dyn Error>> {
        self.lor_exp.generate(program, context)
    }
}

// LOrExp ::= LAndExp | LOrExp "||" LAndExp;
impl ProgramGen for LOrExp {
    type Out = Value;

    fn generate(&self, program: &mut Program, context: &mut Context) -> Result<Self::Out, Box<dyn Error>> {
        Ok(match self {
            LOrExp::And(and) => {
                and.generate(program, context)?
            }
            LOrExp::Or(or, and) => {
                // 利用按位运算实现布尔运算
                // https://juejin.cn/post/7077114074177208351
                // l && r : l | r == (l | r) | 1
                let left = or.generate(program, context)?;
                let right = and.generate(program, context)?;
                let zero = cur_func!(context).new_value(program).integer(0);
                let one = cur_func!(context).new_value(program).integer(1);
                // left != 0
                let left = cur_func!(context).new_value(program).binary(BinaryOp::NotEq, left, zero);
                cur_func!(context).push_inst(program, left);
                // right != 0
                let right = cur_func!(context).new_value(program).binary(BinaryOp::NotEq, right, zero);
                cur_func!(context).push_inst(program, right);
                // l | r
                let l_or_r = cur_func!(context).new_value(program).binary(BinaryOp::Or, left, right);
                cur_func!(context).push_inst(program, l_or_r);
                // (l | r) | 1
                let l_or_r_or_1 = cur_func!(context).new_value(program).binary(BinaryOp::Or, l_or_r, one);
                cur_func!(context).push_inst(program, l_or_r_or_1);
                let value = cur_func!(context).new_value(program).binary(BinaryOp::Eq, l_or_r, l_or_r_or_1);
                cur_func!(context).push_inst(program, value);
                value
            }
        })
    }
}

// LAndExp ::= EqExp | LAndExp "&&" EqExp;
impl ProgramGen for LAndExp {
    type Out = Value;

    // BinaryOp::And 是按位与，不能直接使用
    fn generate(&self, program: &mut Program, context: &mut Context) -> Result<Self::Out, Box<dyn Error>> {
        Ok(match self {
            LAndExp::Eq(eq) => eq.generate(program, context)?,
            LAndExp::And(and, eq) => {
                // 利用按位运算实现布尔运算
                // https://juejin.cn/post/7077114074177208351
                // l || r : l & r == (l | r) | 1
                let left = and.generate(program, context)?;
                let right = eq.generate(program, context)?;
                let zero = cur_func!(context).new_value(program).integer(0);
                let one = cur_func!(context).new_value(program).integer(1);
                // left != 0
                let left = cur_func!(context).new_value(program).binary(BinaryOp::NotEq, left, zero);
                cur_func!(context).push_inst(program, left);
                // right != 0
                let right = cur_func!(context).new_value(program).binary(BinaryOp::NotEq, right, zero);
                cur_func!(context).push_inst(program, right);
                let l_and_r = cur_func!(context).new_value(program).binary(BinaryOp::And, left, right);
                cur_func!(context).push_inst(program, l_and_r);
                // l | r
                let l_or_r = cur_func!(context).new_value(program).binary(BinaryOp::Or, left, right);
                cur_func!(context).push_inst(program, l_or_r);
                // (l | r) | 1
                let l_or_r_or_1 = cur_func!(context).new_value(program).binary(BinaryOp::Or, l_or_r, one);
                cur_func!(context).push_inst(program, l_or_r_or_1);
                let value = cur_func!(context).new_value(program).binary(BinaryOp::Eq, l_and_r, l_or_r_or_1);
                cur_func!(context).push_inst(program, value);
                value
            }
        })
    }
}

// EqExp ::= RelExp | EqExp ("==" | "!=") RelExp;
impl ProgramGen for EqExp {
    type Out = Value;

    fn generate(&self, program: &mut Program, context: &mut Context) -> Result<Self::Out, Box<dyn Error>> {
        Ok(match self {
            EqExp::Rel(rel) => rel.generate(program, context)?,
            EqExp::Eq(eq, op, rel) => {
                let left = eq.generate(program, context)?;
                let right = rel.generate(program, context)?;
                let value = match op {
                    ast::BinaryOp::Eq => cur_func!(context).new_value(program).binary(BinaryOp::Eq, left, right),
                    ast::BinaryOp::Neq => cur_func!(context).new_value(program).binary(BinaryOp::NotEq, left, right),
                    _ => unreachable!()
                };
                cur_func!(context).push_inst(program, value);
                value
            }
        })
    }
}

// RelExp ::= AddExp | RelExp ("<" | ">" | "<=" | ">=") AddExp;
impl ProgramGen for RelExp {
    type Out = Value;

    fn generate(&self, program: &mut Program, context: &mut Context) -> Result<Self::Out, Box<dyn Error>> {
        Ok(match self {
            RelExp::Add(add) => add.generate(program, context)?,
            RelExp::Rel(rel, op, add) => {
                let left = rel.generate(program, context)?;
                let right = add.generate(program, context)?;
                let value = match op {
                    ast::BinaryOp::Lt => cur_func!(context).new_value(program).binary(BinaryOp::Lt, left, right),
                    ast::BinaryOp::Le => cur_func!(context).new_value(program).binary(BinaryOp::Le, left, right),
                    ast::BinaryOp::Gt => cur_func!(context).new_value(program).binary(BinaryOp::Gt, left, right),
                    ast::BinaryOp::Ge => cur_func!(context).new_value(program).binary(BinaryOp::Ge, left, right),
                    _ => unreachable!()
                };
                cur_func!(context).push_inst(program, value);
                value
            },
        })
    }
}

// AddExp ::= MulExp | AddExp ("+" | "-") MulExp;
impl ProgramGen for AddExp {
    type Out = Value;

    fn generate(&self, program: &mut Program, context: &mut Context) -> Result<Self::Out, Box<dyn Error>> {
        Ok(match self {
            AddExp::Add(add_exp, op, mul_exp) => {
                let left_value = add_exp.generate(program, context)?;
                let right_value = mul_exp.generate(program, context)?;
                let value = match op {
                    ast::BinaryOp::Add => {
                        cur_func!(context).new_value(program).binary(BinaryOp::Add, left_value, right_value)
                    }
                    ast::BinaryOp::Sub => {
                        cur_func!(context).new_value(program).binary(BinaryOp::Sub, left_value, right_value)
                    }
                    _ => unreachable!()
                };
                cur_func!(context).push_inst(program, value);
                value
            }
            AddExp::Mul(mul_exp) => {
                mul_exp.generate(program, context)?
            }
        })
    }
}

// MulExp ::= UnaryExp | MulExp ("*" | "/" | "%") UnaryExp;
impl ProgramGen for MulExp {
    type Out = Value;

    fn generate(&self, program: &mut Program, context: &mut Context) -> Result<Self::Out, Box<dyn Error>> {
        Ok(match self {
            MulExp::Unary(unary) => unary.generate(program, context)?,
            MulExp::Mul(mul_exp, op, unary) => {
                let left_value = mul_exp.generate(program, context)?;
                let right_value = unary.generate(program, context)?;
                let value = match op {
                    ast::BinaryOp::Mul => {
                        cur_func!(context).new_value(program).binary(BinaryOp::Mul, left_value, right_value)
                    }
                    ast::BinaryOp::Div => {
                        cur_func!(context).new_value(program).binary(BinaryOp::Div, left_value, right_value)
                    }
                    ast::BinaryOp::Mod => {
                        cur_func!(context).new_value(program).binary(BinaryOp::Mod, left_value, right_value)
                    }
                    _ => unreachable!()
                };
                cur_func!(context).push_inst(program, value);
                value
            }
        })
    }
}

impl ProgramGen for UnaryExp {
    type Out = Value;

    fn generate(&self, program: &mut Program, context: &mut Context) -> Result<Self::Out, Box<dyn Error>> {
        match self {
            UnaryExp::PrimaryExpression(exp) => {
                exp.generate(program, context)
            }
            UnaryExp::UnaryExpression(unary) => {
                unary.generate(program, context)
            }
            UnaryExp::UnaryOpAndExp(op, exp) => {
                let value = exp.generate(program, context)?;
                let zero = cur_func!(context).new_value(program).integer(0);
                let value = match op {
                    UnaryOp::Neg => cur_func!(context).new_value(program).binary(BinaryOp::Sub, zero, value),
                    UnaryOp::Not => cur_func!(context).new_value(program).binary(BinaryOp::Eq, value, zero),
                };
                cur_func!(context).push_inst(program, value);
                Ok(value)
            }
        }
    }
}

impl ProgramGen for PrimaryExp {
    type Out = Value;

    fn generate(&self, program: &mut Program, context: &mut Context) -> Result<Self::Out, Box<dyn Error>> {
        match self {
            PrimaryExp::Expression(exp) => {
                exp.generate(program, context)
            }
            PrimaryExp::Number(num) => {
                let value = cur_func!(context).new_value(program).integer(num.clone());
                Ok(value)
            }
        }
    }
}