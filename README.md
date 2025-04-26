# miniforth

A small Forth subset interpreter written as part of Nonprocedural programming
course at MFF CUNI.

## Features

- Single stack for values
- Basic arithmetic operations
- Basic comparison operations
- Output to standard output
- Load code from file

## Usage

To run the interpreter, run:

```bash
stack run
```

## REPL intrinsics

- `:h|:help` prints help
- `:q|:quit` quits the interpreter
- `:l|:load <filename>` loads a file with Forth code, executes it and brings
  all defined words into scope
- `:clear` clears the stack
- `:words` lists all defined words

## Miniforth builtins

By typing numbers into the REPL, you can push them onto the stack.

```
>>> 1 2 3
ok
>>> .s
[1, 2, 3] <- TOP
>>>
```

### `.s` - stack dump

Prints the contents of the stack. The top of the stack is marked with `<- TOP`
(on the right).

### `.` - pop and print

Pops the top of the stack and prints it.

```
>>> 1 2 3
ok
>>> .
3
```

### `."` - print string

Prints a string. The string must be enclosed in double quotes.

```
>>> ." Hello, world!"
Hello, world!
>>>
```

Note the space after the `."` -- this is required to separate the command from the string.

### `emit` - print character

Prints the top of the stack as an ASCII character.

### `cr` - print newline

Prints a newline.

### `+`, `-`, `*`, `/`, `mod` - arithmetic operations

Performs arithmetic operations on the top two elements of the stack. The result is pushed back onto the stack.

### `<`, `>`, `=` - comparison operations

Performs comparison operations on the top two elements of the stack. The result is pushed back onto the stack as a boolean value (-1 if true, 0 if false).

### `dup` - duplicate top

Duplicates the top element of the stack.

```
>>> 1 dup
ok
>>> .s
[1, 1] <- TOP
```

### `drop` - drop top

Drops the top element of the stack.

```
>>> 1 2 3 drop
ok
>>> .s
[1, 2] <- TOP
```

### `swap` - swap top two

Swaps the top two elements of the stack.

```
>>> 1 2 swap
ok
>>> .s
[2, 1] <- TOP
```

### `over` - copy second from top

Copies the second element from the top of the stack and pushes it onto the top.

```
>>> 1 2 over
ok
>>> .s
[1, 2, 1] <- TOP
```

### `rot` - rotate top three

Rotates the top three elements of the stack. The bottom element is moved to the
top.

```
>>> 1 2 3 rot
ok
>>> .s
[2, 3, 1] <- TOP
```

### `:` - define word

Defines a new word. The word is defined by the code that follows it until the
`;` character. The new word is added to the current scope and can be called
afterwards.

```
>>> : foo  1 2 + ;
ok
>>> foo
ok
>>> .s
[3] <- TOP
```

### `if`, `else`, `then` - conditional execution

Executes the code in the `if` branch if the top of the stack is true (-1).
Otherwise, it executes the code in the `else` branch. The `then` keyword
is used to mark the end of the conditional block. The `else` branch is optional.

```
>>> : istopzero  0 = if ." Top was zero" else ." Top was not zero" then ;
ok
>>> 2 istopzero
Top was not zero
>>> 0 istopzero
Top was zero
```

### `do` - loop

Loops over a range of the top two numbers (top is the start index, second is the end).
The index is automatically pushed onto the stack at the beginning of each iteration.
The loop body is terminated by the `loop` keyword.

```
>>> : sum  dup 0 do + loop . ;
ok
>>> 5 sum
15
>>> 6 sum
21
```

### `exit` - break execution

Breaks the execution of the current word and returns to the REPL.

## Examples

There are some examples in the `examples` directory. You can load them into the
interpreter using the `:l` command. Then call the custom word.

- [`examples/fib.forth`](examples/fib.forth) - calculates the n'th Fibonacci number
- [`examples/factorial.forth`](examples/factorial.forth) - calculates the factorial of n
