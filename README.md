clip
====

clip is an aspiring Python implementation written in Haskell that generates
Common Lisp which can then be compiled to, say, x86 machine code. The premise is
that Python as a language is a subset of Common Lisp, and that since there are
already sophisticated compilers available for Common Lisp we should be able to
piggyback on their hard work in order to improve performance of Python programs
over interpreters or byte code execution.

Wins
====

* Tail call optimization
* Native machine code execution
* Multi-threaded code with no GIL

Flow
====

     source.py         def f(x):
        |                return x + 2
     +------+
     | clip |
     +------+
        |
        V
     source.lisp       (defun f (x)
        |                (+ x 2))
        V
     +------+
     | sbcl |
     +------+
        |
        V
     x86 binary:

     ; 8A:       8B55FC           MOV EDX, [EBP-4]   ; no-arg-parsing entry point
     ; 8D:       BF08000000       MOV EDI, 8
     ; 92:       E8B9EA4FF2       CALL #x4000150     ; GENERIC-+
     ; 97:       7302             JNB L0
     ; 99:       8BE3             MOV ESP, EBX
     ; 9B: L0:   8BE5             MOV ESP, EBP
     ; 9D:       F8               CLC
     ; 9E:       5D               POP EBP
     ; 9F:       C3               RET
     ; A0:       0F0B0A           BREAK 10           ; error trap
     ; A3:       02               BYTE #X02
     ; A4:       18               BYTE #X18          ; INVALID-ARG-COUNT-ERROR
     ; A5:       4F               BYTE #X4F          ; ECX

Example
-

    x = 42
    print x

Raw tokens from lexer:

    [Identifier "x", Whitespace " ", OpAssign, Whitespace " ", NumLiteral 42, Newline 1,
     Print, Whitespace " ", Identifier "x", Newline 2, Newline 3]

"Newline n" means that the newline appears at the end of line n. This
allows the parser to include the line number for syntax errors.

Tokens after cleanup pass (Indent/dedent logic, combine multi-line
string literals, remove comments and whitespace):

    [Identifier "x", OpAssign, NumLiteral 42, Newline 1,
     Print, Identifier "x", Newline 2, Newline 3]

AST built by parser:

    [Assign {destinations = [VariableRef {identifier = "x"}], values = [Literal "42"]},
     Format {destination = T, values = [VariableRef {identifier = "x"}], newline = True}]

Common Lisp code output:

    (psetf x 42)
    (format t "~a~%" x)

Python Reference
-

    http://docs.python.org/2/reference/lexical_analysis.html

    http://docs.python.org/2/reference/grammar.html

Lisp-1 versus Lisp-2
-

Common Lisp has separate namespaces for most variables and functions
(a Lisp-2), but Python doesn't (a Lisp-1).

Quick fix: bind every function/method in both namespaces, e.g.

    (defun f ...)
    (setf f #'f)

Now you can pass "f" as a parameter to other functions without knowing
whether it is a function or not.

Decorators
-

Ideas:

    (let ((tmp-function-name (gensym)))
      (setf (symbol-function tmp-function-name) (lambda ...))
      (setf (symbol-function 'real-function-name (decorator #'tmp-function-name))))

or

    (flet ((tmp-function-name ...))
      (setf (symbol-function 'real-function-name (decorator #'tmp-function-name))))
