use std::collections::HashMap;
use koopa::ir::{BasicBlock, Function, Program, Type, TypeKind, Value, ValueKind};
use koopa::ir::builder::{BasicBlockBuilder, LocalBuilder, LocalInstBuilder, ValueBuilder};
use koopa::ir::entities::ValueData;
use super::{Result, Error};

// 在生成IR的时候需要一个结构体来存储上下文
pub struct Context {
    pub cur_func: Option<FunctionInfo>,
    // 符号表
    pub vals: Vec<HashMap<String, CTValue>>,
    // 函数表
    pub funcs: HashMap<String, Function>
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
    cur: BasicBlock,
    ret_value: Option<Value>,
    end_bb: BasicBlock,
    loops: Vec<LoopInfo>,
    // 为 true 时表明这段代码是 unreachable 的，不应当继续插入代码
    unreachable: bool
}

impl Context {
    pub fn new() -> Self {
        Self {
            cur_func: None,
            // global
            vals: vec![HashMap::new()],
            funcs: HashMap::new()
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
    pub fn new_ct_value(&mut self, id: String, value: CTValue) -> Result<()> {
        let is_global = self.is_global();
        let cur = self.vals.last_mut().unwrap();
        if cur.contains_key(&id)
            // 全局变量 且与方法重名
            || (is_global && self.funcs.contains_key(&id)) {
            Err(Error::DuplicatedDef)
        } else {
            cur.insert(id, value);
            Ok(())
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

    pub fn function(&self, id: &str) -> Result<&Function> {
        self.funcs.get(id).ok_or(Error::SymbolNotFound)
    }

    pub fn ty(&self, program: &Program, value: Value) -> Type {
        if value.is_global() {
            program.borrow_value(value).ty().clone()
        } else {
            program
                .func(cur_func!(self).func())
                .dfg()
                .value(value)
                .ty()
                .clone()
        }
    }

    pub fn new_func(&mut self, id: &str, func: Function) -> Result<()> {
        if self.funcs.contains_key(id) || self.vals.first().unwrap().contains_key(id) {
            Err(Error::DuplicatedDef)
        } else {
            self.funcs.insert(id.into(), func);
            Ok(())
        }
    }
}

impl FunctionInfo {

    /// Creates a new function information.
    pub fn new(func: Function, entry: BasicBlock, program: &mut Program) -> Self {
        Self {
            func,
            entry,
            cur: entry,
            // 事先把 要 ret 的地方 alloc 一下
            ret_value: if matches!(program.func(func).ty().kind(), TypeKind::Function(_, ret) if ret.is_i32()) {
                Some(
                    program.func_mut(func)
                        .dfg_mut()
                        .new_value()
                        .alloc(Type::get_i32())
                )
            } else {
                None
            },
            end_bb: program.func_mut(func)
                .dfg_mut()
                .new_bb()
                .basic_block(Some("%end".into())),
            loops: vec![],
            unreachable: false
        }
    }

    pub fn end(&self) -> BasicBlock {
        self.end_bb
    }

    pub fn ret_value(&self) -> Option<Value> {
        self.ret_value
    }

    pub fn func(&self) -> Function {
        self.func
    }

    pub fn loops(&mut self) -> &mut Vec<LoopInfo> {
        &mut self.loops
    }
    
    pub fn set_unreachable(&mut self) {
        self.unreachable = true;
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
        self.unreachable = false;
    }

    /// Pushes the instruction to the back of the given basic block.
    pub fn push_inst_to(&self, program: &mut Program, bb: BasicBlock, inst: Value) {
        // unreachable 时什么也不做
        if self.unreachable {
            return;
        }
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
#[derive(Clone, Debug)]
pub enum CTValue {
    Const(i32),
    Runtime(Value)
}

impl CTValue {
    fn print(&self, context: &mut Context, program: &mut Program) {
        match self {
            Self::Runtime(value) => {
                println!("{:?}", cur_func!(context).value(program, *value))
            },
            Self::Const(i32) => println!("{:?}", i32)
        }
    }

    fn as_runtime(&self) -> Value {
        match self {
            Self::Runtime(value) => *value,
            _ => unreachable!()
        }
    }
}

pub struct LoopInfo {
    entry_bb: BasicBlock,
    end_bb: BasicBlock
}

impl LoopInfo {
    pub fn new(entry_bb: BasicBlock, end_bb: BasicBlock) -> Self {
        Self { entry_bb, end_bb }
    }

    pub fn entry(&self) -> BasicBlock {
        self.entry_bb
    }

    pub fn end(&self) -> BasicBlock {
        self.end_bb
    }
}

pub trait ValueExtension {
    fn push(self, program: &mut Program, ctx: &mut Context) -> Value;
    fn into_int(self, context: &mut Context, program: &mut Program) -> Value;
}

impl ValueExtension for Value {
    fn push(self, program: &mut Program, ctx: &mut Context) -> Value {
        cur_func!(ctx).push_inst(program, self);
        self
    }

    fn into_int(self, context: &mut Context, program: &mut Program) -> Value {
        let mut value = self;
        let value_data = if value.is_global() {
            program.borrow_value(value).clone()
        } else {
            cur_func!(context).value(program, value)
        };
        if matches!(&value_data.kind(), ValueKind::Alloc(..))
            || matches!(&value_data.kind(), ValueKind::GlobalAlloc(..)) {
            value = cur_func!(context)
                .new_value(program)
                .load(value)
                .push(program, context);
        }
        value
    }
}