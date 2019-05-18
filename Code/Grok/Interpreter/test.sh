echo "---------"
racket -t main.rkt -m -- ../../../Test/Parses/simple_Lit.parse expr
echo "---------"
racket -t main.rkt -m -- ../../../Test/Parses/simple_Add.parse expr
echo "---------"
racket -t main.rkt -m -- ../../../Test/Parses/simple_Symb_Add.parse expr
echo "---------"
racket -t main.rkt -m -- ../../../Test/Parses/multi_Symb_Add.parse expr
echo "---------"
racket -t main.rkt -m -- ../../../Test/Parses/if_else.parse expr
echo "---------"
racket -t main.rkt -m -- ../../../Test/Parses/fun_Add.parse decl
echo "---------"
racket -t main.rkt -m -- ../../../Test/Parses/fun_if_else.parse decl
echo "---------"
racket -t main.rkt -m -- ../../../Test/Parses/fun_if_else_recursion.parse decl
echo "Done!"
