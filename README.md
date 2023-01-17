# compiler-learning

> 未完成

跟着 [北大编译实践在线文档](https://pku-minic.github.io/online-doc/) 用 Rust 敲一个 SysY 编译器，目的是学习 & 实践编译原理。

## testing

~~~bash
$ docker run -it --rm -v $(pwd):/root/compiler maxxing/compiler-dev \ bash
$ autotest -koopa -s lv1 /root/compiler
~~~

autotest 的参数可变

## usage

~~~bash
# 编译到 riscv
$ cargo run -- -riscv hello.c -o hello.riscv
# 编译到 koopa ir
$ cargo run -- -koopa hello.c -o hello.koopa
~~~