# pascal-compiler

本程序为Pascal语言子集的源到源编译器。

Grammar位于项目根目录的grammar.bnf。

##### 使用方法

运行程序，可进入交互式命令行，键入Pascal源代码文件（后缀名不限。本目录下的sample.txt可供测试）的绝对路径后回车，即可进行编译
。程序会输出为变量添加了类型声明、为变量与过程添加了作用域嵌套层级后缀的源代码。

