use std::collections::HashMap;
use std::ops::Deref;
use koopa::ir::{BasicBlock, Function, Program, Value, ValueKind};
use koopa::ir::builder::{BasicBlockBuilder, LocalBuilder};
use koopa::ir::entities::ValueData;
use super::{Result, Error};

// 在生成IR的时候需要一个结构体来存储上下文
pub struct Context {
    pub cur_func: Option<FunctionInfo>,
    // 符号表
    pub vals: Vec<HashMap<String, CTValue>>
}

/// Returns a reference to the current function information.
macro_rules! cur_func {
  ($scopes:expr) => {
    $scopes.cur_func.as_ref().unwrap()
  };
}
pub(crate) use cur_func;

/// Returns a mutable reference to the current function information.
macro_rules! cur_func_mut {
  ($scopes:expr) => {
    $scopes.cur_func.as_mut().unwrap()
  };
}
pub(crate) use cur_func_mut;

pub struct FunctionInfo {
    func: Function,
    entry: BasicBlock,
    cur: BasicBlock
}

impl Context {
    pub fn new() -> Self {
        Self {
            cur_func: None,
            vals: vec![HashMap::new()]
        }
    }

    /// 进入一个代码块，push一个新的常量表
    pub fn enter(&mut self) {
        self.vals.push(HashMap::new());
    }

    /// 退出一个代码块，所以就扔掉当前代码块的常量表
    pub fn exit(&mut self) {
        self.vals.pop();
    }

    /// Returns `true` if is currently in global scope.
    pub fn is_global(&self) -> bool {
        self.cur_func.is_none()
    }

    /// Inserts a new value to the current scope.
    pub fn new_value(&mut self, id: String, value: CTValue) -> Result<()> {
        // let is_global = self.is_global();
        let cur = self.vals.last_mut().unwrap();
        if cur.contains_key(&id) {
            // || (is_global && self.funcs.contains_key(&id))
            Err(Error::DuplicatedDef)
        } else {
            cur.insert(id, value);
            Ok(())
        }
    }

    // 肯定只有变量才能更新，常量想更新是不可能的
    pub fn update_value(&mut self, id: String, value: Value) -> Result<()> {
        let cur = self.vals.last_mut().unwrap();
        if cur.contains_key(&id) {
            // || (is_global && self.funcs.contains_key(&id))
            cur.insert(id, CTValue::Runtime(value));
            Ok(())
        } else {
            Err(Error::InvalidVarDef)
        }
    }

    /// Returns the value by the given identifier.
    pub fn value(&self, id: &str) -> Result<CTValue> {
        let mut cur = self.vals.len() as i32 - 1;
        while cur >= 0 {
            if let Some(value) = self.vals[cur as usize].get(id) {
                return Ok(value.clone());
            }
            cur -= 1;
        }
        Err(Error::SymbolNotFound)
    }
}

impl FunctionInfo {

    /// Creates a new function information.
    pub fn new(func: Function, entry: BasicBlock) -> Self {
        Self {
            func,
            entry,
            cur: entry,
        }
    }

    pub fn func(&self) -> Function {
        self.func
    }

    /// Creates a new basic block in function.
    pub fn new_bb(&self, program: &mut Program, name: Option<&str>) -> BasicBlock {
        program
            .func_mut(self.func)
            .dfg_mut()
            .new_bb()
            .basic_block(name.map(|s| s.into()))
    }

    /// Creates a new value in function.
    pub fn new_value<'p>(&self, program: &'p mut Program) -> LocalBuilder<'p> {
        program.func_mut(self.func).dfg_mut().new_value()
    }

    /// Pushes the basic block to the function,
    /// updates the current basic block.
    pub fn push_bb(&mut self, program: &mut Program, bb: BasicBlock) {
        program
            .func_mut(self.func)
            .layout_mut()
            .bbs_mut()
            .push_key_back(bb)
            .unwrap();
        self.cur = bb;
    }

    /// Pushes the instruction to the back of the given basic block.
    pub fn push_inst_to(&self, program: &mut Program, bb: BasicBlock, inst: Value) {
        program
            .func_mut(self.func)
            .layout_mut()
            .bb_mut(bb)
            .insts_mut()
            .push_key_back(inst)
            .unwrap();
    }

    /// Pushes the instruction to the back of the current basic block.
    pub fn push_inst(&self, program: &mut Program, inst: Value) {
        self.push_inst_to(program, self.cur, inst);
    }

    /// 获取 value 对应的 value_data
    pub fn value(&self, program: &mut Program, value: Value) -> ValueData {
        program
            .func_mut(self.func)
            .dfg()
            .value(value)
            .clone()
    }

}

// Compiler Time Value
#[derive(Clone)]
pub enum CTValue {
    Const(i32),
    Runtime(Value)
}

pub trait ValueExtension {
    fn push(self, program: &mut Program, ctx: &mut Context) -> Value;
}

impl ValueExtension for Value {
    fn push(self, program: &mut Program, ctx: &mut Context) -> Value {
        cur_func!(ctx).push_inst(program, self);
        self
    }
}