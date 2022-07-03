package main

import (
	"fmt"
	"math"
	"strconv"
	"strings"
)

const stackSize = 1024

type VMStack struct {
	data     [stackSize]interface{}
	rbp      int
	rsp      int
	maxStack int
}

func (s *VMStack) Push(value interface{}) {
	s.data[s.rsp] = value
	s.rsp++
	if s.rsp > s.maxStack {
		s.maxStack = s.rsp
	}
}

func (s *VMStack) Pop() interface{} {
	s.rsp--
	ret := s.data[s.rsp]
	s.data[s.rsp] = nil
	return ret
}

func (s *VMStack) GetAtRealIndex(index int) interface{} {
	return s.data[index]
}

func (s *VMStack) GetAtIndex(index int) interface{} {
	return s.data[s.rbp+index]
}

func (s *VMStack) SetAtRealIndex(index int, value interface{}) {
	s.data[index] = value
}

func (s *VMStack) SetAtIndex(index int, value interface{}) {
	dx := s.rbp + index
	if dx < 0 {
		fmt.Println("here")
	}
	s.data[dx] = value
}

func (s *VMStack) Len() int {
	return len(s.data)
}

type funcCall struct {
	pc  int
	rbp int
	rsp int
}

type VM struct {
	pc        int
	stack     VMStack
	heap      map[string]map[int]interface{}
	callStack IStack
	output    strings.Builder
}

var i = 0

func (vm *VM) display(cc command, isVM bool) {
	return
	var args []interface{}
	args = append(args, i)
	i++

	cmd := cc.code
	switch cmd {
	case opSUB, opADD, opMUL, opDIV, opLT, opLTE, opGT, opGTE:
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
	if !isVM {
		for i := vm.stack.rbp; i < vm.stack.rsp; i++ {
			val := vm.stack.GetAtRealIndex(i)
			if val == nil {
				val = "X"
			} else if _, ok := val.(nilValue); ok {
				val = "@"
			}
			ss = append(ss, fmt.Sprintf(`%v`, val))
		}
	}

	ex := fmt.Sprintf(`RBP=%d:%d | RSP=%d:%d %s`, vm.stack.rbp, cc.rbp, vm.stack.rsp, cc.rsp, strings.Join(ss, "|"))
	args = append(args, ex)
	format := `%-4d %-10s %-10v %-10v %-10v`
	s := fmt.Sprintf(format, args...)
	fmt.Println(s)
}

func (vm *VM) Run(bytecode []command, funcs map[string]funcFrame) {
	vm.stack = VMStack{
		data: [stackSize]interface{}{},
		rbp:  0,
		rsp:  0,
	}
	vm.callStack = IStack{}
	// init heap
	vm.heap = map[string]map[int]interface{}{}
	for i := 0; i < len(bytecode); i++ {
		cmd := bytecode[i]
		vm.display(cmd, true)
	}

	fmt.Println(`=====================================`)
	lk := 0
mainLoop:
	for vm.pc < len(bytecode) {
		lk++
		if lk > 30000 {
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
				vm.stack.Push(op1.(arrIndex))
			default:
				vm.stack.Push(op1)
			}
		case opARRAY:
			arrayName := op1.(string)
			vm.heap[arrayName] = make(map[int]interface{}, 100)
		case opADD, opSUB, opMUL, opDIV, opLT, opLTE, opGT, opGTE, opEQ, opMOD:
			operand2 := vm.stack.Pop()
			operand1 := vm.stack.Pop()

			var o1, o2 = f32(operand1), f32(operand2)

			var value float32
			if cmd.code == opADD {
				value = o1 + o2
			} else if cmd.code == opSUB {
				value = o1 - o2
			} else if cmd.code == opMUL {
				value = o1 * o2
			} else if cmd.code == opDIV {
				value = o1 / o2
			} else if cmd.code == opMOD {
				value = f32(math.Mod(float64(o1), float64(o2)))
			} else if cmd.code == opLT {
				if o1 < o2 {
					value = 1
				} else {
					value = 0
				}
			} else if cmd.code == opLTE {
				if o1 <= o2 {
					value = 1
				} else {
					value = 0
				}
			} else if cmd.code == opGT {
				if o1 > o2 {
					value = 1
				} else {
					value = 0
				}
			} else if cmd.code == opGTE {
				if o1 >= o2 {
					value = 1
				} else {
					value = 0
				}
			} else if cmd.code == opEQ {
				if o1 == o2 {
					value = 1
				} else {
					value = 0
				}
			}
			vm.stack.Push(value)
		case opCALL:
			switch op1.(string) {
			case "runtime.array":

				args := op2.(astFunCallArgs)
				arrayName := string(args[0][0].(astFuncArgValue))
				method := string(args[1][0].(astFuncArgValue))

				self := vm.heap[arrayName]
				switch method {
				case "get":
					index := int(f32(vm.stack.GetAtRealIndex(vm.stack.rsp - 2)))
					value := self[index]
					vm.stack.Pop()
					vm.stack.Pop()
					vm.stack.Push(value)
				case "set":
					index := int(f32(vm.stack.Pop()))
					value := vm.stack.Pop()
					self[index] = value
				}
			case "printf":
				ret := vm.stack.Pop()
				val := vm.stack.Pop()
				vm.output.WriteString(fmt.Sprintf(`%v `, val))
				vm.stack.Push(ret)
			default:
				vm.callStack.Push(funcCall{
					pc:  vm.pc + 1,    // next instruction
					rbp: vm.stack.rbp, // current store rbp
					rsp: vm.stack.rsp,
				})

				vm.stack.rbp = vm.stack.rsp

				funcPC := funcs[op1.(string)].pc
				vm.pc = funcPC
				goto exitLoop
			}

		case opCMP:
			operand1 := vm.stack.Pop()
			o1 := f32(operand1)
			jumpTo := *op1.(*int)
			if o1 <= 0 {
				vm.pc = jumpTo
				goto exitLoop
			}
		case opRETURN:
			exitPC := op1.(*int)
			vm.pc = *exitPC
			goto exitLoop
		case opSUBUnary:
			operand1 := vm.stack.Pop()
			o1 := f32(operand1)
			o1 *= -1
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
			vm.display(cmd, false)
			break mainLoop
		}
		vm.pc++
	exitLoop:
		vm.display(cmd, false)
	}
	fmt.Println("RESULT = " + vm.output.String())
}

func f32(val interface{}) float32 {
	switch val.(type) {
	case float64:
		return float32(val.(float64))
	case float32:
		return val.(float32)
	case int:
		return float32(val.(int))
	default:
		f, err := strconv.ParseFloat(fmt.Sprintf(`%d`, val), 32)
		if err == nil {
			panic("cannot parse float")
		}
		return float32(f)
	}
}
