use std::error::Error;
use koopa::back::KoopaGenerator;
use koopa::ir::{BinaryOp, FunctionData, Program, Type, Value};
use koopa::ir::builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder};
use crate::frontend::ast::{Block, Exp, FuncDef, FuncType, PrimaryExp, Stmt, UnaryExp, UnaryOp};
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
        self.unary_exp.generate(program, context)
    }
}

impl ProgramGen for UnaryExp {
    type Out = Value;

    fn generate(&self, program: &mut Program, context: &mut Context) -> Result<Self::Out, Box<dyn Error>> {
        match self {
            UnaryExp::PrimaryExpression(exp) => {
                exp.generate(program, context)
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