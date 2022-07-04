package vm

import (
	"fmt"
	"testing"
)

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

func TestVM_Run(t *testing.T) {

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
