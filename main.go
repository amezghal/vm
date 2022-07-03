package main

import (
	"fmt"
)

//notes
// - keep in mind it's a virtual machine,
// - and more specifically it's a store machine, so not all real hardware
//   is necessary to simulate
// - it's not a typed language, so our language needs a runtime
// for example:
// 1) no need for registers, it's a store machine after all
// 2) for function calls, don't generate assembly for function calls prologue/epilogue as in ASM
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

var scriptNestedLoops = `
fn main() {
	let s = 12;
	for (j=2; j<=120; j = j + 1){
		for (kk=3; kk<5; kk = kk + 1){
			let xxx = 10;
			if (xxx<5){
				_ = printf("case 1");
				for (kkk=0; kkk<2; kkk = kkk + 1){
					let g = 3 + -2;
					let koko = 3 + 2;
					let hello = 12;
					
					s = s + 1+kkk+kk+j+add(g, koko, hello);
				}
			} else if (xxx<15){
				_ = printf("case 2");
				for (kkk=1; kkk<4; kkk = kkk + 1){
					let g = -3 + 4;
					let koko = 3 + 2;
					let hello = 12;
					
					s = s + 1+kkk+kk+j+add(g, koko, hello);
				}
			} else if (xxx<11){
				_ = printf("case 3");
				for (kkk=1; kkk<4; kkk = kkk + 1){
					let g = 3 + 4;
					let koko = 3 + 2;
					let hello = 12;
					
					s = s + 1+kkk+kk+j+add(g, koko, hello);
				}
			} else {
				_ = printf("case 4");
				for (kkk=1; kkk<4; kkk = kkk + 1){
					let g = 3 + 2;
					let koko = 3 + 2;
					let hello = 12;
					
					s = s + 1+kkk+kk+j+add(g, koko, hello);
				}
			}
		}
	 }
	_=printf(s);
}

fn add(a, b, c){
	return a + b * c;
}
`

var scriptArray = `
fn main(){
	let store = [];
	for (let i=0; i<10; i = i+1){
		store[i] = i*-i;
	}
	
	let total = 0;
	for (let j=0; j<10; j = j+1){
		total = total + store[j] * 3;
		_=printf(total);
	}
 	return 0;
}
`

func main() {

	// Hello, world

	lex := new(Lexer)
	tt, err := lex.Run([]byte(scriptNestedLoops))
	if err != nil {
		fmt.Println("err" + err.Error())
	}
	if ok, tree := parseScript(tt); ok {
		fmt.Println("OK")
		cpl := Compiler{}
		cpl.Run(tree)
	} else {
		fmt.Println("NOK")
		printAllErrors()
	}
	return
}

// @todo need to move this to the Stack generic
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
