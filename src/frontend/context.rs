use koopa::ir::{BasicBlock, Function, Program, Value};
use koopa::ir::builder::{BasicBlockBuilder, LocalBuilder};
use koopa::ir::entities::ValueData;

// 在生成IR的时候需要一个结构体来存储上下文
pub struct Context {
    pub cur_func: Option<FunctionInfo>
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
            cur_func: None
        }
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