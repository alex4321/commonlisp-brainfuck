This is a simple brainfuck interpreter written with common lisp.

You can run it this way:
```
clisp brainfuck.lsp program.bf
```
Where `program.bf` - file with your brainfuck-code.

E.g. for 

```
++++++++++
[
    >+++++++>++++++++++>+++>+<<<<-
]
>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.
```
it should give
```
clisp brainfuck.pl program.bf
Hello World!
```