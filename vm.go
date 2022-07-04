package vm

import (
	"fmt"
	"strings"
)

const stackSize = 1024

type VMStack struct {
	data [stackSize]any
	rbp  int // stack base pointer
	rsp  int // stack pointer
}

func (s *VMStack) Push(value any) {
	if s.rsp > stackSize-1 {
		panic("out of stack")
	}
	s.data[s.rsp] = value
	s.rsp++
}

func (s *VMStack) Pop() any {
	s.rsp--
	ret := s.data[s.rsp]
	s.data[s.rsp] = nil
	return ret
}

func (s *VMStack) GetAtRealIndex(index int) any {
	return s.data[index]
}

func (s *VMStack) GetAtIndex(index int) any {
	return s.data[s.rbp+index]
}

func (s *VMStack) SetAtRealIndex(index int, value any) {
	s.data[index] = value
}

func (s *VMStack) SetAtIndex(index int, value any) {
	dx := s.rbp + index
	s.data[dx] = value
}

type funcCall struct {
	pc  int
	rbp int
	rsp int
}

type VM struct {
	pc        int // program counter
	stack     VMStack
	heap      map[string]map[int]any // heap is where we store array's
	callStack Stack[any]
	output    strings.Builder
}

var i = 0

func (vm *VM) display(cc command, isVM bool) {
	var args []any
	args = append(args, i)
	i++

	cmd := cc.code
	switch cmd {
	case opSUB, opADD, opMUL, opQUO, opLT, opLTE, opGT, opGTE:
		cc.op1 = nil
		cc.op2 = nil
	}
	args = append(args, opNames[cmd])
	if c1, ok := cc.op1.(astValue); ok {
		args = append(args, c1.Value.Value)
	} else if c1, ok := cc.op1.(Token); ok {
		args = append(args, c1.Value)
	} else if c1, ok := cc.op1.(RBP); ok {
		args = append(args, fmt.Sprintf(`RBP[%d]`, c1))
	} else {
		if cmd == opRETURN {
			args = append(args, *(cc.op1.(*int)))
		} else if cmd == opCMP {
			args = append(args, *(cc.op1.(*int)))
		} else if cmd == opJUMP {
			args = append(args, *(cc.op1.(*int)))
		} else {
			args = append(args, cc.op1)
		}
	}

	if c1, ok := cc.op2.(astValue); ok {
		args = append(args, c1.Value.Value)
	} else if c1, ok := cc.op2.(Token); ok {
		args = append(args, c1.Value)
	} else if c1, ok := cc.op2.(RBP); ok {
		args = append(args, fmt.Sprintf(`RBP[%d]`, c1))
	} else {
		args = append(args, cc.op2)
	}

	var ss []string

	ex := fmt.Sprintf(`RBP=%d:%d | RSP=%d:%d %s`, vm.stack.rbp, cc.rbp, vm.stack.rsp, cc.rsp, strings.Join(ss, "|"))
	args = append(args, ex)
	format := `%-4d %-10s %-10v %-10v %-10v`
	s := fmt.Sprintf(format, args...)
	fmt.Println(s)
}

func (vm *VM) Run(bytecode []command, funcs map[string]funcFrame) {
	vm.stack = VMStack{
		data: [stackSize]any{},
		rbp:  0,
		rsp:  0,
	}
	vm.callStack = Stack[any]{}
	// init heap
	vm.heap = map[string]map[int]any{}
	for i := 0; i < len(bytecode); i++ {
		cmd := bytecode[i]
		vm.display(cmd, true)
	}

	fmt.Println(`=====================================`)
	lk := 0
mainLoop:
	for vm.pc < len(bytecode) {
		lk++
		if lk > 300000 {
			fmt.Println("possible VM infinite loop")
			break
		}
		cmd := bytecode[vm.pc]
		op1, op2 := cmd.op1, cmd.op2

		switch cmd.code {
		case opPOP:
			value := vm.stack.Pop()
			switch op1.(type) {
			case RBP:
				v := int(op1.(RBP))
				vm.stack.SetAtIndex(v, value)
			case RSP:
				v := int(op1.(RSP))
				if v == 0 {
					vm.stack.SetAtRealIndex(vm.stack.rsp-1, value)
				}
			}
		case opPUSH:
			switch op1.(type) {
			case RBP:
				v := int(op1.(RBP))
				vm.stack.Push(vm.stack.GetAtIndex(v))
			case localVar:
				v := int(op2.(RBP))
				vm.stack.Push(vm.stack.GetAtIndex(v))
			case arrIndex:
				vm.stack.Push(VMValue{kind: T_STRING, value: op1.(arrIndex)})
			case nilValue:
				vm.stack.Push(VMValue{kind: T_UNDEFINED, value: nil})
			case VMValue:
				vm.stack.Push(op1.(VMValue))
			default:
				vm.stack.Push(op1.(VMValue))
			}
		case opARRAY:
			arrayName := op1.(string)
			vm.heap[arrayName] = make(map[int]any, 100)
		case opADD, opSUB, opMUL, opQUO, opMOD, opLT, opLTE, opGT, opGTE, opEQ, opOR, opAND:
			operand2 := vm.stack.Pop()
			operand1 := vm.stack.Pop()

			value := Arith(cmd.code, operand1.(VMValue), operand2.(VMValue))

			vm.stack.Push(value)
		case opCALL:
			switch op1.(string) {
			case "runtime.array":

				args := op2.(astFunCallArgs)
				arrayName := string(args[0][0].(astFuncArgValue))
				method := string(args[1][0].(astFuncArgValue))

				getIndex := func(indexRaw any) int {
					var index int
					switch indexRaw.(type) {
					case VMValue:
						index = indexRaw.(VMValue).value.(int)
					case int:
						index = indexRaw.(int)
					default:
						panic("array index type not a an INT")
					}
					return index
				}

				self := vm.heap[arrayName]
				switch method {
				case "get":
					index := getIndex(vm.stack.GetAtRealIndex(vm.stack.rsp - 2))
					value := self[index]
					if value == nil {
						fmt.Println(fmt.Sprintf(`index out of range array:%s index:%d`, arrayName, index))
						value = VMValue{kind: T_UNDEFINED, value: nil}
					}
					vm.stack.Pop()
					vm.stack.Pop()
					vm.stack.Push(value)
				case "set":
					index := getIndex(vm.stack.Pop())
					value := vm.stack.Pop()
					self[index] = value
				}
			case "printf":
				ret := vm.stack.Pop()
				val := vm.stack.Pop()
				//vm.output.WriteString(fmt.Sprintf(`%v `, val))
				fmt.Println(fmt.Sprintf(` VM -> %v `, val))
				// to align with function calls conventions, we need tow push to the stack
				vm.stack.Push(ret)
				vm.stack.Push(ret)
			default:
				if calledFunc, ok := funcs[op1.(string)]; ok {
					vm.callStack.Push(funcCall{
						pc:  vm.pc + 1,    // next instruction
						rbp: vm.stack.rbp, // current store rbp
						rsp: vm.stack.rsp,
					})

					vm.stack.rbp = vm.stack.rsp

					funcPC := calledFunc.pc
					vm.pc = funcPC
					goto exitLoop
				} else {
					panic(fmt.Sprintf(`call to undefined function %s`, op1.(string)))
				}
			}

		case opCMP:
			operand1 := vm.stack.Pop()
			if operand1.(VMValue).value.(int) <= 0 {
				jumpTo := *op1.(*int)
				vm.pc = jumpTo
				goto exitLoop
			}
		case opRETURN:
			exitPC := op1.(*int)
			vm.pc = *exitPC
			goto exitLoop
		case opSUBUnary:
			operand1 := vm.stack.Pop()
			o1 := Arith(opMUL, VMValue{kind: T_NUMBER, value: -1}, operand1.(VMValue))
			vm.stack.Push(o1)
		case opNOT:
			operand1 := vm.stack.Pop()
			o1 := Arith(opNOT, operand1.(VMValue), VMValue{kind: T_NUMBER, value: -1})
			vm.stack.Push(o1)
		case opLEAVE:
			if vm.callStack.Len() > 0 {
				metadata := vm.callStack.Pop().(funcCall)
				vm.stack.rsp = metadata.rsp
				vm.stack.rbp = metadata.rbp
				vm.pc = metadata.pc
				goto exitLoop
			}
		case opJUMP:
			op := op1.(*int)
			vm.pc = *op
			goto exitLoop
		case opHALT:
			break mainLoop
		}
		vm.pc++
	exitLoop:
		//vm.display(cmd, false)
	}
	fmt.Println(vm.output.String())
}
