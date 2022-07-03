package main

import (
	"fmt"
	"vm/token"
)

//notes
// - keep in mind it's a virtual machine,
// - and more specifically it's a store machine, so not all real hardware
//   is necessary to simulate
// - it's not a typed language, so our language needs a runtime
// for example:
// 1) no need for registers, it's a store machine after all
// 2) for function calls, dont generate assembly for function calls prologue/epilogue as in ASM
//    instead we can use a store to keep track of that
// 3) implement comparison operators as in javascript (meaning they are like other operators)
//    it's just that we will have 0 or 1's depending on the comparison result and then it's allowed to chain other operators
//

/*
func_decl = 'func' '(' func_args ')' '{' body '}'
func_args = (t_var | t_var ',')* $')'
body = (expression | for | if )*

for = 'for' '(' assign ; cmp ; assign ')' '{' body '}'

if = 'if' '(' cmp ')' '{' body '}' | elseif | else
elseif = ('else' if)+
else = else '{' body '}'

vdcl = 'let' var '=' express ';'
express = (t_unary? one_lit){1} | (t_unary? lit op t_unary? lit)+
one_lit = lit ($';' | $')'){1}
lit = (t_num | t_string | f_call | t_var)* | ('(' lit ')')
f_call = t_func_call '(' ( fc_args )* ')'
fc_args = (express | express ',')* $')'

*/
var script2 = `
fn main(){
 let s = 45;
 let mm = [];
 for (n=0; n<10; n = n + 1){
    mm[n] = n * 3;
 }

 for (j=2; j<=120; j = j + 1){
	for (kk=3; kk<5; kk = kk + 1){
		for (kkk=3; kkk<5; kkk = kkk + 1){
				s = s + 1+kkk+kk+j;
				if (kk<kkk){
					s = s + kk*2;
				}
		}
	}
 }
 xx = printf(s);
 return s;
}
`

var script3 = `
func main(){
	let xx = [];
	let yy = [];
    let n1 = 11 + 33;
    let n2 = 22;
    let n3 = 44;
	return cc(0);
    let number = 18;
    for (let i = 2; i < number; i = i + 1) {
		xx[i] = n2+n1;
		yy[i] = n2+n1+xx[i]+3;
        n3 = n1 + n2;
        n1 = n2 + aa(n1, n2, n3);
        n2 = n3;
    }
    return n1+xx[4]+yy[3];
}

func aa(a1,b1,c1){ 
	return 13
}

func cc(is){
	let s = 1;
	if (is < 4){
		s = s + cc(is+1);
	}
	return s;
}`

func main() {

	a1 := Amez{
		tok:   token.STRING,
		value: "aa",
	}

	a2 := Amez{
		tok:   token.STRING,
		value: "_bb",
	}

	aa := Arith(token.ADD, a1, a2)
	fmt.Println(fmt.Sprintf("T=%T, V=%v", aa.value, aa.value))
	return
	lex := new(Lexer)
	tt, err := lex.Run([]byte(script2))
	if err != nil {
		fmt.Println("err" + err.Error())
	}
	if ok, tree := parseScript(tt); ok {
		fmt.Println("OK")
		cpl := Compiler{}
		cpl.Run(tree)
	} else {
		fmt.Println("NOK")
	}
	return
}

const (
	T_IF        = "__IF__"
	T_ELSE      = "__ELSE__"
	T_RETURN    = "__RETURN__"
	T_LBRACE    = "__{__"
	T_RBRACE    = "__}__"
	T_LINDEX    = "__[__"
	T_RINDEX    = "__]__"
	T_ARRAY     = "__[]__"
	T_NOT       = "__!__"
	T_AND       = "__and__"
	T_OR        = "__or__"
	T_COMA      = "__;__"
	T_SEMI      = "__,__"
	T_ASSIGN    = "__=__"
	T_EQ        = "__==__"
	T_LT        = "<"
	T_GT        = ">"
	T_LTE       = "<="
	T_GTE       = ">="
	T_LET       = "__LET__"
	T_FOR       = "__FOR__"
	T_LPAR      = "__(__"
	T_RPAR      = "__)__"
	T_FUNC_CALL = "__FUNC_CALL__"
	T_FUNC_DEC  = "__FUNC_DEC__"
	T_PLUS      = "__+__"
	T_MINUS     = "__-__"
	T_MULT      = "__*__"
	T_DIV       = "__/__"
	T_MOD       = "__%__"
	T_VAR       = "__VAR__"
	T_NUMBER    = "__NUMBER__"
	T_UNARY     = "__UNARY__"
	T_IFUNC     = "__I_FUNC__"
	T_STRING    = "__STRING__"
)

type Token struct {
	Kind  string
	Value string
	Pos   int
	Line  int
	Col   int
}

func (t *Token) IsOp() bool {
	switch t.Value {
	case "+", "-", "*", "/":
		return true
	}
	return false
}

func (t *Token) String() string {
	return fmt.Sprintf(`Token kind=%s, value=%s, pos=%d`, t.Kind, t.Value, t.Pos)
}

type astStack []astNode

func (a *astStack) push(value astNode) {
	*a = append(*a, value)
}

func (a *astStack) pop() astNode {
	ret := (*a)[len(*a)-1]
	*a = (*a)[:len(*a)-1]
	return ret
}

func (a *astStack) tail() astNode {
	return (*a)[len(*a)-1]
}

func (a *astStack) atIndex(index int) astNode {
	return (*a)[index]
}

func (a *astStack) len() int {
	return len(*a)
}

type StackFloat []float32

func (s *StackFloat) Push(value float32) {
	*s = append(*s, value)
}

func (s *StackFloat) Pop() float32 {
	if len(*s) == 0 {
		return .0
	}
	ret := (*s)[len(*s)-1]
	*s = (*s)[:len(*s)-1]
	return ret
}

func (s *StackFloat) Tail() float32 {
	return (*s)[len(*s)-1]
}

func (s *StackFloat) Len() int {
	return len(*s)
}

// interface{} store

type IStack []interface{}

func (s *IStack) Push(value interface{}) {
	*s = append(*s, value)
}

func (s *IStack) Pop() interface{} {
	ret := (*s)[len(*s)-1]
	*s = (*s)[:len(*s)-1]
	return ret
}

func (s *IStack) Tail() interface{} {
	return (*s)[len(*s)-1]
}

func (s *IStack) AtIndex(index int) interface{} {
	return (*s)[index]
}

func (s *IStack) Len() int {
	return len(*s)
}
