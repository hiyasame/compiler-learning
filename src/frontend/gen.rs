use koopa::back::KoopaGenerator;
use koopa::ir::{BinaryOp, FunctionData, Program, Type, Value};
use koopa::ir::builder::{BasicBlockBuilder, GlobalInstBuilder, LocalInstBuilder, ValueBuilder};
use crate::frontend::ast;
use crate::frontend::ast::{AddExp, Block, BlockItem, ConstDecl, ConstDef, ConstExp, ConstInitVal, Decl, EqExp, Exp, FuncDef, FuncType, InitVal, LAndExp, LOrExp, MulExp, PrimaryExp, RelExp, Stmt, UnaryExp, UnaryOp, VarDecl, VarDef};
use crate::frontend::context::{Context, CTValue, cur_func, cur_func_mut, FunctionInfo, LoopInfo, ValueExtension};
use crate::frontend::Error::CannotAssignConstant;
use crate::frontend::eval::Evaluate;
use super::ast::CompUnit;
use super::Result;

pub fn compile_koopa(ast: CompUnit) -> Result<String> {
    let mut program = Program::new();
    ast.generate(&mut program, &mut Context::new())?;
    let mut gen = KoopaGenerator::new(vec![]);
    gen.generate_on(&program).expect("koopa ir generation failed");
    Ok(std::str::from_utf8(&gen.writer()).unwrap().to_string())
}

trait ProgramGen {
    type Out;
    fn generate(&self, program: &mut Program, context: &mut Context) -> Result<Self::Out>;
}

impl ProgramGen for CompUnit {
    type Out = ();

    fn generate(&self, program: &mut Program, context: &mut Context) -> Result<Self::Out> {
        self.func_def.generate(program, context)?;
        Ok(())
    }
}

impl ProgramGen for FuncDef {
    type Out = ();

    fn generate(&self, program: &mut Program, context: &mut Context) -> Result<Self::Out> {
        let mut func_data = FunctionData::with_param_names(format!("@{}", self.ident), vec![], self.func_type.generate(program, context)?);

        let entry = func_data.dfg_mut().new_bb().basic_block(Some("%entry".into()));
        let func = program.new_func(func_data);
        let mut info = FunctionInfo::new(func, entry, program);
        info.push_bb(program, entry);
        context.cur_func = Some(info);
        // 进入函数
        context.enter();
        // 先把 alloc 返回值的语句 push 上去
        cur_func!(context).ret_value().push(program, context);
        self.block.generate(program, context)?;
        // 最后把 end 块 push 上去
        let end = cur_func!(context).end();
        cur_func_mut!(context).push_bb(program, end);
        let ret_value = cur_func!(context).new_value(program)
            .load(cur_func!(context).ret_value())
            .push(program, context);
        // 返回值
        cur_func!(context).new_value(program)
            .ret(Some(ret_value))
            .push(program, context);
        // 退出函数
        context.exit();
        Ok(())
    }
}

impl ProgramGen for FuncType {
    type Out = Type;

    fn generate(&self, _program: &mut Program, _context: &mut Context) -> Result<Self::Out> {
        Ok(match self {
            Self::Int => Type::get_i32()
        })
    }
}

impl ProgramGen for Block {
    type Out = ();
    fn generate(&self, program: &mut Program, context: &mut Context) -> Result<Self::Out> {
        self.block_items.iter().for_each(|item| {
            item.generate(program, context).expect("generation failed");
        });
        Ok(())
    }
}

impl ProgramGen for BlockItem {
    type Out = ();

    fn generate(&self, program: &mut Program, context: &mut Context) -> Result<Self::Out> {
        match self {
            BlockItem::Declare(decl) => {
                decl.generate(program, context)?
            }
            BlockItem::Statement(stmt) => {
                stmt.generate(program, context)?
            }
        };
        Ok(())
    }
}

impl ProgramGen for Decl {
    type Out = ();

    fn generate(&self, program: &mut Program, context: &mut Context) -> Result<Self::Out> {
        match self {
            Self::Const(decl) => {
                decl.generate(program, context)
            }
            Self::Var(decl) => {
                decl.generate(program, context)
            }
        }

    }
}

impl ProgramGen for ConstDecl {
    type Out = ();

    fn generate(&self, program: &mut Program, context: &mut Context) -> Result<Self::Out> {
        for def in &self.const_defs {
            def.generate(program, context)?
        }
        Ok(())
    }
}

impl ProgramGen for ConstDef {
    type Out = ();

    fn generate(&self, program: &mut Program, context: &mut Context) -> Result<Self::Out> {
        let value = CTValue::Const(self.val.generate(program, context)?);
        context.new_value(self.ident.clone(), value)
    }
}

impl ProgramGen for ConstInitVal {
    type Out = i32;

    fn generate(&self, program: &mut Program, context: &mut Context) -> Result<Self::Out> {
        self.exp.generate(program, context)
    }
}

impl ProgramGen for ConstExp {
    type Out = i32;

    fn generate(&self, program: &mut Program, context: &mut Context) -> Result<Self::Out> {
        Ok(self.exp.eval(program, context).unwrap())
    }
}

impl ProgramGen for Stmt {
    type Out = ();
    fn generate(&self, program: &mut Program, context: &mut Context) -> Result<Self::Out> {
        match self {
            Stmt::Assign(lval, exp) => {
                // 更新变量的值
                let value = exp.generate(program, context)?;
                if let CTValue::Runtime(dest) = context.value(lval.ident.as_str())? {
                    cur_func!(context)
                        .new_value(program)
                        .store(value, dest)
                        .push(program, context);
                } else {
                    return Err(CannotAssignConstant)
                }
            }
            Stmt::Ret(exp) => {
                let ret_value = exp.generate(program, context)?.into_int(context, program);
                // 设置返回值，并且跳转到 end 块
                cur_func!(context).new_value(program)
                    .store(ret_value, cur_func!(context).ret_value())
                    .push(program, context);
                cur_func!(context).new_value(program)
                    .jump(cur_func!(context).end())
                    .push(program, context);
                // 之后的内容不应继续 push 进去了，它们是 unreachable code，设置一下 unreachable 状态
                // 当 push 下一个基本块时 unreachable 状态会被刷新
                cur_func_mut!(context).set_unreachable();
            }
            Stmt::Exp(exp) => {
                if let Some(exp) = exp {
                    exp.generate(program, context)?;
                }
            }
            Stmt::Block(block) => {
                context.enter();
                block.generate(program, context)?;
                context.exit();
            }
            Stmt::If { cond, then, else_then } => {
                let cond = cond.generate(program, context)?.into_int(context, program);
                let then_bb = cur_func!(context).new_bb(program, Some("%if_then"));
                let else_bb = cur_func!(context).new_bb(program, Some("%if_else"));
                cur_func!(context)
                    .new_value(program)
                    .branch(cond, then_bb, else_bb)
                    .push(program, context);
                // 进入 then 块
                cur_func_mut!(context).push_bb(program, then_bb);
                then.generate(program, context)?;
                // 最后写一段跳转到 end 块的逻辑
                let end_bb = cur_func!(context).new_bb(program, Some("%if_end"));
                cur_func!(context).new_value(program)
                    .jump(end_bb)
                    .push(program, context);
                // 开始写 else_bb
                cur_func_mut!(context).push_bb(program, else_bb);
                // 如果有 else 则生成
                if let Some(else_then) = else_then {
                    else_then.generate(program, context)?;
                }
                // 也写一段跳转到 end 块的代码
                cur_func!(context)
                    .new_value(program)
                    .jump(end_bb)
                    .push(program, context);
                // 进入 end 块
                cur_func_mut!(context).push_bb(program, end_bb);
            }
            Stmt::While { cond, stmt } => {
                let while_entry = cur_func!(context).new_bb(program, Some("%while_entry"));
                let while_body = cur_func!(context).new_bb(program, Some("%while_body"));
                let while_end = cur_func!(context).new_bb(program, Some("%while_end"));
                let loop_info = LoopInfo::new(while_entry, while_end);

                cur_func!(context).new_value(program)
                    .jump(while_entry)
                    .push(program, context);
                cur_func_mut!(context).push_bb(program, while_entry);

                // 这一步要在进入while_entry之后
                let cond = cond.generate(program, context)?.into_int(context, program);

                cur_func!(context).new_value(program)
                    .branch(cond, while_body, while_end)
                    .push(program, context);
                cur_func_mut!(context).push_bb(program, while_body);
                // 在进到 body 之后再把 loop info push 上去
                cur_func_mut!(context).loops().push(loop_info);
                stmt.generate(program, context)?;
                // body 里面的语句满了就出栈
                cur_func_mut!(context).loops().pop();
                cur_func!(context).new_value(program)
                    .jump(while_entry)
                    .push(program, context);
                cur_func_mut!(context).push_bb(program, while_end);
            },
            Stmt::Break => {
                cur_func!(context).new_value(program)
                    .jump(cur_func_mut!(context).loops().last().unwrap().end())
                    .push(program, context);
                cur_func_mut!(context).set_unreachable();
            },
            Stmt::Continue => {
                cur_func!(context).new_value(program)
                    .jump(cur_func_mut!(context).loops().last().unwrap().entry())
                    .push(program, context);
                cur_func_mut!(context).set_unreachable()
            }
        }
        Ok(())
    }
}

impl ProgramGen for Exp {
    type Out = Value;

    fn generate(&self, program: &mut Program, context: &mut Context) -> Result<Self::Out> {
        self.lor_exp.generate(program, context)
    }
}

// LOrExp ::= LAndExp | LOrExp "||" LAndExp;
impl ProgramGen for LOrExp {
    type Out = Value;

    fn generate(&self, program: &mut Program, context: &mut Context) -> Result<Self::Out> {
        Ok(match self {
            LOrExp::And(and) => {
                and.generate(program, context)?
            }
            LOrExp::Or(or, and) => {
                // 利用按位运算实现布尔运算
                // https://juejin.cn/post/7077114074177208351
                // l && r : l | r == (l | r) | 1
                let left = or.generate(program, context)?.into_int(context, program);
                let right = and.generate(program, context)?.into_int(context, program);
                let zero = cur_func!(context).new_value(program).integer(0);
                let one = cur_func!(context).new_value(program).integer(1);
                // left != 0
                let left = cur_func!(context)
                    .new_value(program)
                    .binary(BinaryOp::NotEq, left, zero)
                    .push(program ,context);
                // right != 0
                let right = cur_func!(context)
                    .new_value(program)
                    .binary(BinaryOp::NotEq, right, zero)
                    .push(program, context);
                // l | r
                let l_or_r = cur_func!(context)
                    .new_value(program)
                    .binary(BinaryOp::Or, left, right)
                    .push(program, context);
                // (l | r) | 1
                let l_or_r_or_1 = cur_func!(context)
                    .new_value(program)
                    .binary(BinaryOp::Or, l_or_r, one)
                    .push(program, context);
                let value = cur_func!(context)
                    .new_value(program)
                    .binary(BinaryOp::Eq, l_or_r, l_or_r_or_1)
                    .push(program, context);
                value
            }
        })
    }
}

// LAndExp ::= EqExp | LAndExp "&&" EqExp;
impl ProgramGen for LAndExp {
    type Out = Value;

    // BinaryOp::And 是按位与，不能直接使用
    fn generate(&self, program: &mut Program, context: &mut Context) -> Result<Self::Out> {
        Ok(match self {
            LAndExp::Eq(eq) => eq.generate(program, context)?,
            LAndExp::And(and, eq) => {
                // 利用按位运算实现布尔运算
                // https://juejin.cn/post/7077114074177208351
                // l || r : l & r == (l | r) | 1
                let left = and.generate(program, context)?.into_int(context, program);
                let right = eq.generate(program, context)?.into_int(context, program);
                let zero = cur_func!(context).new_value(program).integer(0);
                let one = cur_func!(context).new_value(program).integer(1);
                // left != 0
                let left = cur_func!(context)
                    .new_value(program)
                    .binary(BinaryOp::NotEq, left, zero)
                    .push(program, context);
                // right != 0
                let right = cur_func!(context)
                    .new_value(program)
                    .binary(BinaryOp::NotEq, right, zero)
                    .push(program, context);
                let l_and_r = cur_func!(context)
                    .new_value(program)
                    .binary(BinaryOp::And, left, right)
                    .push(program, context);
                // l | r
                let l_or_r = cur_func!(context)
                    .new_value(program)
                    .binary(BinaryOp::Or, left, right)
                    .push(program, context);
                // (l | r) | 1
                let l_or_r_or_1 = cur_func!(context)
                    .new_value(program)
                    .binary(BinaryOp::Or, l_or_r, one)
                    .push(program, context);
                let value = cur_func!(context)
                    .new_value(program)
                    .binary(BinaryOp::Eq, l_and_r, l_or_r_or_1)
                    .push(program, context);
                value
            }
        })
    }
}

// EqExp ::= RelExp | EqExp ("==" | "!=") RelExp;
impl ProgramGen for EqExp {
    type Out = Value;

    fn generate(&self, program: &mut Program, context: &mut Context) -> Result<Self::Out> {
        Ok(match self {
            EqExp::Rel(rel) => rel.generate(program, context)?,
            EqExp::Eq(eq, op, rel) => {
                let left = eq.generate(program, context)?.into_int(context, program);
                let right = rel.generate(program, context)?.into_int(context, program);
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

    fn generate(&self, program: &mut Program, context: &mut Context) -> Result<Self::Out> {
        Ok(match self {
            RelExp::Add(add) => add.generate(program, context)?,
            RelExp::Rel(rel, op, add) => {
                let left = rel.generate(program, context)?.into_int(context, program);
                let right = add.generate(program, context)?.into_int(context, program);
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

    fn generate(&self, program: &mut Program, context: &mut Context) -> Result<Self::Out> {
        Ok(match self {
            AddExp::Add(add_exp, op, mul_exp) => {
                let left_value = add_exp.generate(program, context)?.into_int(context, program);
                let right_value = mul_exp.generate(program, context)?.into_int(context, program);
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

    fn generate(&self, program: &mut Program, context: &mut Context) -> Result<Self::Out> {
        Ok(match self {
            MulExp::Unary(unary) => unary.generate(program, context)?,
            MulExp::Mul(mul_exp, op, unary) => {
                let left_value = mul_exp.generate(program, context)?.into_int(context, program);
                let right_value = unary.generate(program, context)?.into_int(context, program);
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

    fn generate(&self, program: &mut Program, context: &mut Context) -> Result<Self::Out> {
        match self {
            UnaryExp::PrimaryExpression(exp) => {
                exp.generate(program, context)
            }
            UnaryExp::UnaryExpression(unary) => {
                unary.generate(program, context)
            }
            UnaryExp::UnaryOpAndExp(op, exp) => {
                let value = exp.generate(program, context)?.into_int(context, program);
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

    fn generate(&self, program: &mut Program, context: &mut Context) -> Result<Self::Out> {
        match self {
            PrimaryExp::Expression(exp) => {
                exp.generate(program, context)
            }
            PrimaryExp::Number(num) => {
                let value = cur_func!(context).new_value(program).integer(*num);
                Ok(value)
            }
            PrimaryExp::LValue(lval) => {
                // 得从符号表里面去取
                let ct_value = context.value(lval.ident.as_str())?;
                Ok(match ct_value {
                    CTValue::Const(i32) => cur_func!(context).new_value(program).integer(i32),
                    CTValue::Runtime(value) => value,
                })
            }
        }
    }
}


impl ProgramGen for VarDecl {
    type Out = ();

    fn generate(&self, program: &mut Program, context: &mut Context) -> Result<Self::Out> {
        for def in &self.defs {
            def.generate(program, context)?
        }
        Ok(())
    }
}

impl ProgramGen for VarDef {
    type Out = ();

    fn generate(&self, program: &mut Program, context: &mut Context) -> Result<Self::Out> {
        match self {
            VarDef::NotInit { ident } => {
                let value = if context.is_global() {
                    let value = program.new_value().zero_init(Type::get_i32());
                    program.borrow_value(value);
                    // global的变量不用push
                    program.new_value()
                        .global_alloc(value)
                } else {
                    cur_func!(context)
                        .new_value(program)
                        .alloc(Type::get_i32())
                        .push(program, context)
                };
                context.new_value(ident.to_string(), CTValue::Runtime(value))?;
            }
            VarDef::Init { ident, val } => {
                let val = val.generate(program, context)?;
                let value = if context.is_global() {
                    program.new_value().global_alloc(val)
                } else {
                    let alloc = cur_func!(context)
                        .new_value(program)
                        .alloc(Type::get_i32())
                        .push(program, context);
                    cur_func!(context)
                        .new_value(program)
                        .store(val, alloc)
                        .push(program, context);
                    alloc
                };
                context.new_value(ident.to_string(), CTValue::Runtime(value))?;
            }
        }
        Ok(())
    }
}

impl ProgramGen for InitVal {
    type Out = Value;

    fn generate(&self, program: &mut Program, context: &mut Context) -> Result<Self::Out> {
        self.exp.generate(program, context)
    }
}