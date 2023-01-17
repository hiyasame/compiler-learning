use koopa::ir::{BinaryOp, FunctionData, Program, ValueKind};
use koopa::ir::entities::ValueData;
use koopa::ir::layout::BasicBlockNode;

// 根据内存形式 Koopa IR 生成汇编
trait GenerateAsm {
    fn generate(&self, codes: &mut Vec<String>) {
        unimplemented!()
    }

    fn visit_with_func_data(&self, codes: &mut Vec<String>, func_data: &FunctionData) {
        unimplemented!()
    }
}

impl GenerateAsm for Program {
    fn generate(&self, codes: &mut Vec<String>) {
        codes.push("  .text".into());
        codes.push("  .globl main".into());
        // 遍历所有函数
        for &func in self.func_layout() {
            self.func(func).generate(codes);
        }
    }
}

impl GenerateAsm for FunctionData {
    fn generate(&self, codes: &mut Vec<String>) {
        codes.push(format!("{}:", self.name().strip_prefix("@").unwrap()));
        // 遍历所有基本块
        for (&_bb, node) in self.layout().bbs() {
            node.visit_with_func_data(codes, self)
        }
    }
}

impl GenerateAsm for BasicBlockNode {
    fn visit_with_func_data(&self, codes: &mut Vec<String>, func_data: &FunctionData) {
        for &inst in self.insts().keys() {
            let value_data = func_data.dfg().value(inst);
            value_data.visit_with_func_data(codes, func_data)
        }
    }
}

impl GenerateAsm for ValueData {
    fn visit_with_func_data(&self, codes: &mut Vec<String>, func_data: &FunctionData) {
        match self.kind() {
            ValueKind::Integer(int) => {
                // 处理 integer 指令
                codes.push(format!("  li a0, {}", int.value()));
            }
            ValueKind::Return(ret) => {
                if let Some(value) = ret.value() {
                    let value_data = func_data.dfg().value(value);
                    value_data.visit_with_func_data(codes, func_data);
                }
                codes.push("  ret".into());
            }
            ValueKind::Binary(binary) => {
                match binary.op() {
                    BinaryOp::Eq => {
                        // li t0, 6
                        // xor t0, t0, x0
                        // seqz t0, t0

                    }
                    BinaryOp::Sub => {
                        // sub t1, x0, t0
                    }
                    // 未实现
                    _ => unimplemented!()
                }
            }
            // 未实现
            _ => unimplemented!()
        }
    }
}

pub trait Gen {
    fn generate_riscv(&self) -> String;
}

impl Gen for Program {
    fn generate_riscv(&self) -> String {
        let mut vec = vec![];
        self.generate(&mut vec);
        vec.join("\n")
    }
}