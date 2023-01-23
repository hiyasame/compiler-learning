use lalrpop_util::lalrpop_mod;
use std::env::args;
use std::error::Error;
use std::fs::{File, read_to_string};
use std::io::{Write};
use koopa::front::Driver;
use crate::backend::gen::Gen;
use crate::frontend::ast::CompUnit;
use crate::frontend::gen::compile_koopa;

mod backend;
mod frontend;

// 引用 lalrpop 生成的解析器
// 因为我们刚刚创建了 sysy.lalrpop, 所以模块名是 sysy
lalrpop_mod!(sysy);

fn main() -> Result<(), Box<dyn Error>> {

    // 解析命令行参数
    let mut args = args();
    args.next();
    let mode = args.next().unwrap(); // -koopa / -riscv
    let input = args.next().unwrap(); // hello.c
    args.next(); // -o
    let output = args.next().unwrap(); // hello.koopa
    println!("{}", input);
    // 读取输入文件
    let input = read_to_string(input)?;
    // 创建输出文件
    let mut file = File::create(output)?;
    // 创建AST
    let ast: CompUnit = sysy::CompUnitParser::new().parse(&input).unwrap();
    let koopa_source = compile_koopa(ast)?;

    if mode == "-koopa" {
        // 编译到 koopa ir
        // 调用 lalrpop 生成的 parser 解析输入文件
        file.write(koopa_source.as_bytes())?;
    } else {
        // 将 c 编译到 riscv
        let driver: Driver<_> = koopa_source.into();
        let program = driver.generate_program().expect("koopa ir 解析失败");
        let source = program.generate_riscv();
        file.write(source.as_bytes())?;
    }
    Ok(())
}
