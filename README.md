# pa_debug

A syntax extension to make ocaml debugging easier.

## Compiling

Just make!

    make pa_debug_ob


## Usage

While in the project's root directory, fire up the ocaml interpreter with these flags :

    ocaml -I _build -I _build/debug_lib

Similarly if you're using utop:

    utop -I _build -I _build/debug_lib

and execute these commands:

    #use "topfind";;
    #camlp4o;;
    #load "pa_debug.cmo";;
    #load "str.cma";;
    #load "globals.cmo";;
    #load "error.cmo";;
    #load "gen.cmo";;
    #load "debug.cmo";;
    #load "pa_debug.cmo";;

Make sure you have [ocamlfind](http://opam.ocamlpro.com/pkg/ocamlfind.1.3.3.html) installed.

Once you have loaded the libraries and the syntax extension to the interpreter, you can start using it in the interpreter.

    let debug rec fib n =
        if n < 1 then
          1
        else (fib (n- 1) + (fib (n - 2)))
    ;;

    let debug rec fib n =
        if n < 1 then
          1
        else (fib (n- 1) + (fib (n - 2)))
    ;;

The syntax extension adds a `debug` annotation to functions. With the debug annotation, the debug library will print the argument and the output of each invocation.

    utop[12]> fact 3;;

    fact@351@350@349@348
    fact inp1 :0
    fact@351 EXIT:1

    fact@350@349@348
    fact inp1 :1
    fact@350 EXIT:1

    fact@349@348
    fact inp1 :2
    fact@349 EXIT:2

    fact@348
    fact inp1 :3
    fact@348 EXIT:6
    - : int = 6
    utop[13]>
