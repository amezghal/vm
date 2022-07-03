package main

import "C"
import (
	"fmt"
	"strconv"
)

// region consts
const (
	opPOP int = iota
	opPUSH
	opSET
	opMOD
	opADD
	opSUB
	opSUBUnary
	opMUL
	opDIV
	opCALL
	opLABEL
	opLT
	opGT
	opLTE
	opGTE
	opEQ
	opJUMP
	opRETURN
	opLEAVE
	opHALT
	opCMP
	opARRAY
)

var opNames = map[int]string{
	opPOP:      "POP",
	opPUSH:     "PUSH",
	opSET:      "SET",
	opMOD:      "MOD",
	opADD:      "ADD",
	opSUB:      "SUB",
	opSUBUnary: "SUBUnary",
	opMUL:      "MUL",
	opDIV:      "DIV",
	opCALL:     "CALL",
	opLABEL:    "__LABEL__",
	opLT:       "LT",
	opGT:       "GT",
	opLTE:      "LTE",
	opGTE:      "GTE",
	opEQ:       "EQ",
	opJUMP:     "JUMP",
	opRETURN:   "RETURN",
	opLEAVE:    "LEAVE",
	opHALT:     "HALT",
	opCMP:      "CMP",
	opARRAY:    "ARRAY",
}

// endregion

// region types
type funcFrame struct {
	numArgs int
	pc      int // when func is created, memorise current `pc` in order to be able to call this function
	len     int
}

type vmLocals map[localVar]interface{}

// linked list is better, but let's try with a stack (since we love stack's)
type locals struct {
	store IStack
}

func (l *locals) get(name localVar) (interface{}, bool) {
	for i := l.store.Len() - 1; i >= 0; i-- {
		ll := l.store[i].(vmLocals)
		if el, ok := ll[name]; ok {
			return el, true
		}
	}
	return nil, false
}

// set will always use the last item as store
func (l *locals) set(name localVar, value interface{}) {
	ll := l.store[l.store.Len()-1].(vmLocals)
	ll[name] = value
}

func (l *locals) pop() vmLocals {
	return l.store.Pop().(vmLocals)
}

type stackFrame struct {
	locals       locals
	name         string
	funcReturnPC *int
	callStack    IStack
	rsp          int
}

type Compiler struct {
	cursor    int
	pc        int // program counter
	program   []command
	funcs     map[string]funcFrame
	funcCalls IStack // caller/callee and also to save pc address
	currRSP   *int
	currRBP   int // register stack base pointer
	bytecode  []command
}

type command struct {
	code int
	op1  interface{}
	op2  interface{}
	op3  interface{}
	op4  interface{}
	op5  interface{}
	rbp  interface{}
	rsp  interface{}
}

func (c *Compiler) Push(cmd command) {
	dd := fmt.Sprintf(`RBP=%d RSP=%d`, c.currRBP, *c.currRSP)

	c.pc++ // increment program counter

	// tune-in the `rsp`,
	// we need to compute at every instruction the state of rbp, exactly as it should be during runtime
	// in order to be able to use local variables

	// so decrease/increase the value of `rsp` according to the impact of the instruction
	// example: PUSH will increment %rsp while POP will decrement.
	if c.currRSP != nil {
		switch cmd.code {
		case opPUSH:
			*c.currRSP++
		case opPOP, opCMP:
			*c.currRSP--

		case opSUB, opADD, opMUL, opDIV, opLT, opLTE, opGT, opGTE:
			// POP then POP then PUSH ( in 2 operands => out 1 operand)
			*c.currRSP--
		}
	}
	cmd.op5 = dd
	cmd.rbp = c.currRBP
	cmd.rsp = *c.currRSP
	c.program = append(c.program, cmd)
}

// endregion

func (c *Compiler) Run(tree []astNode) {
	c.pc = 0

	// init func map
	c.funcs = map[string]funcFrame{}

	c.currRBP = 0
	c.currRSP = intP()

	// init global store frame
	stack := &stackFrame{
		locals:    locals{store: IStack{}},
		callStack: IStack{},
	}

	stack.locals.store.Push(vmLocals{})

	for c.cursor < len(tree) {
		n := tree[c.cursor]
		if node, ok := n.(astFuncDecl); ok {
			c.genFunc(stack, node)
		} else if node, ok := n.(astAssign); ok {
			c.genAssign(stack, node)
		}
		c.cursor++
	}
	vm := VM{}
	vm.Run(c.program, c.funcs)
}

type localVar string
type RBP int
type RSP int
type arrIndex string

type nilValue struct {
}

func (c *Compiler) genFunc(frame *stackFrame, node astFuncDecl) {
	if _, ok := c.funcs[node.Name]; ok {
		panic(fmt.Sprintf(`function %s already declared`, node.Name))
	}
	// save func
	pc := c.pc
	c.Push(command{
		code: opLABEL,
		op1:  node.Name,
	})

	frame.callStack.Push(node.Name)
	frame.funcReturnPC = intP()

	// save callee store frame
	oldRBP := c.currRBP
	oldRSP := *c.currRSP

	// set new store frame
	c.currRBP = *c.currRSP

	// function arguments
	for i := 0; i < len(node.Args); i++ {
		arg := node.Args[i].(astVarDec)
		// reverse order
		c.Push(command{
			code: opPUSH,
			op1:  localVar(arg.Value),
			op2:  RBP(-(-i + 1 + len(node.Args))),
		})

		frame.locals.set(localVar(arg.Value), RBP(i))
	}

	c.genBody(frame, node.Body)

	c.funcs[node.Name] = funcFrame{
		numArgs: len(node.Args),
		pc:      pc,
		len:     c.pc - pc,
	}

	*frame.funcReturnPC = c.pc

	if node.Name != "main" {
		c.Push(command{
			code: opPOP,
			op1:  RBP(-1),
		})
	}

	c.Push(command{
		code: opLEAVE,
		op1:  len(node.Args),
	})

	if node.Name == "main" {
		c.Push(command{
			code: opHALT,
			op1:  node.Name,
		})
	}

	frame.callStack.Pop()

	// destroy func stack frame
	c.currRBP = oldRBP
	*c.currRSP = oldRSP
}

func (c *Compiler) getValue(frame *stackFrame, node astNode, pf *astStack) {

	switch node.(type) {
	case astLitGroup:
		tt := node.(astLitGroup)
		c.getValue(frame, tt.Value.(astNode), pf)
		if tt.Unary != nil {
			pf.push(tt.Unary)
		}
	case astOp:
		pf.push(node.(astOp))
	case astExpression:
		nodePostFix := c.genExpressionHelper(frame, node.(astExpression))
		pf.push(astCapturedGroupNode(nodePostFix))
	case astLiteral:
		tt := node.(astLiteral)
		c.getValue(frame, tt.Value, pf)
		if tt.Unary != nil {
			pf.push(tt.Unary)
		}
	case astFuncCall:
		tt := node.(astFuncCall)
		for i := 0; i < len(tt.Args); i++ {
			nodePostFix := c.genExpressionHelper(frame, tt.Args[i])
			pf.push(astCapturedGroupNode(nodePostFix))
		}
		pf.push(tt)
	case astVarArrayIndex:
		n := node.(astVarArrayIndex)
		nodePostFix := c.genExpressionHelper(frame, n.index)
		pf.push(astCapturedGroupNode(nodePostFix))
		pf.push(astFuncCall{
			Name: "runtime.array",
			Args: astFunCallArgs{
				[]astNode{astFuncArgValue(node.(astVarArrayIndex).value.Value)}, // self array
				[]astNode{astFuncArgValue("get")},                               // method name
			},
		})
		*c.currRSP--
	case astValue:
		tt := node.(astValue)
		pf.push(tt)
	}
	return
}

func (c *Compiler) genNode(frame *stackFrame, node astNode) astStack {
	pf := astStack{}
	c.getValue(frame, node, &pf)
	return pf
}

func expand(s astCapturedGroupNode) astStack {
	expanded := astStack{}
	ss := astStack(s)
	for i := 0; i < ss.len(); i++ {
		node := s[i]
		switch node.(type) {
		case astCapturedGroupNode:
			elems := expand(node.(astCapturedGroupNode))
			for j := 0; j < elems.len(); j++ {
				n := elems[j]
				expanded.push(n)
			}
		default:
			expanded.push(node)
		}
	}
	return expanded
}

func (c *Compiler) genPostfix(frame *stackFrame, postfix astStack) {
	// every expression inside parentheses will be converted to postfix notation

	ss := StackFloat{}
	for i := 0; i < postfix.len(); i++ {
		node := postfix.atIndex(i)
		switch node.(type) {
		case astUnary, *astUnary:
			op1 := ss.Pop()
			ss.Push(-op1)
			c.Push(command{
				code: opSUBUnary,
				op1:  op1,
			})
		case astOp:
			n := node.(astOp)
			op2 := ss.Pop()
			op1 := ss.Pop()

			switch n.Token.Kind {
			case T_PLUS:
				ss.Push(op1)
				c.Push(command{
					code: opADD,
					op1:  op1,
					op2:  op2,
				})
			case T_MINUS:
				ss.Push(op1)
				c.Push(command{
					code: opSUB,
					op1:  op1,
					op2:  op2,
				})
			case T_MULT:
				ss.Push(op1)
				c.Push(command{
					code: opMUL,
					op1:  op1,
					op2:  op2,
				})
			case T_DIV:
				c.Push(command{
					code: opDIV,
					op1:  op1,
					op2:  op2,
				})
				ss.Push(op1)
			case T_MOD:
				c.Push(command{
					code: opMOD,
					op1:  op1,
					op2:  op2,
				})
				ss.Push(op1)
			case T_LT:
				c.Push(command{
					code: opLT,
					op1:  op1,
					op2:  op2,
				})
				ss.Push(op1)
			case T_GT:
				c.Push(command{
					code: opGT,
					op1:  op1,
					op2:  op2,
				})
				ss.Push(op1)
			case T_LTE:
				c.Push(command{
					code: opLTE,
					op1:  op1,
					op2:  op2,
				})
				ss.Push(op1)
			case T_GTE:
				c.Push(command{
					code: opGTE,
					op1:  op1,
					op2:  op2,
				})
				ss.Push(op1)
			case T_EQ:
				c.Push(command{
					code: opEQ,
					op1:  op1,
					op2:  op2,
				})
				ss.Push(op1)
			}
		case astFuncCall:
			n := node.(astFuncCall)

			c.Push(command{
				code: opPUSH,
				op1:  nilValue{},
				op2:  "ret val",
			})

			c.Push(command{
				code: opCALL,
				op1:  n.Name,
				op2:  n.Args, // keep it
			})
			if n.Name != "runtime.array" {
				for i := 0; i < len(n.Args); i++ {
					c.Push(command{
						code: opPOP,
						op1:  RSP(0),
					})
				}
			}

		case astValue:
			n := node.(astValue)
			var source interface{}
			var desc interface{}
			nKind := n.Value.Kind
			switch nKind {
			case T_VAR:
				varName := n.Value.Value
				if location, ok := frame.locals.get(localVar(varName)); ok {
					source = location
					desc = fmt.Sprintf(`ploc %s`, varName)
				} else {
					desc = fmt.Sprintf(`call undef var %s`, varName)
				}
			case T_NUMBER:
				source, _ = strconv.ParseFloat(n.Value.Value, 32)
				desc = fmt.Sprintf(`NUMBER`)
			case T_STRING:
				source = n.Value.Value
				desc = fmt.Sprintf(`STRING`)
			default:
				panic("got unknown astValue kind")
			}
			c.Push(command{
				code: opPUSH,
				op1:  source,
				op2:  desc,
			})
			val, _ := strconv.ParseFloat(n.Value.Value, 32)
			ss.Push(float32(val))
		}
	}
}

func convertToPostfix(stack astStack) astStack {
	isWeak := func(op astOp) bool {
		return op.Token.Kind == T_PLUS || op.Token.Kind == T_MINUS
	}

	postfixStack := astStack{}
	pStack := astStack{}

	cursor := 0
	for cursor < stack.len() {
		node := stack.atIndex(cursor)
		cursor++
		switch node.(type) {
		case astValue:
			postfixStack.push(node)
		case astOp:
			for pStack.len() > 0 {
				last := pStack.tail()
				if lastOp, ok := last.(astOp); ok {
					k := lastOp.Token.Kind
					if k == T_LT || k == T_LTE || k == T_GT || k == T_GTE {
						break
					}

					if !(!isWeak(node.(astOp)) && isWeak(lastOp)) {
						postfixStack.push(pStack.pop())
					} else {
						break
					}
				}
			}
			pStack.push(node)
		case astFuncCall:
			postfixStack.push(node)
		case astCapturedGroupNode:
			els := expand(node.(astCapturedGroupNode))
			for i := 0; i < els.len(); i++ {
				postfixStack.push(els[i])
			}
		case astUnary, *astUnary:
			postfixStack.push(node)
		default:
			fmt.Println("unrecognized")
		}
	}
	for pStack.len() > 0 {
		postfixStack.push(pStack.pop())
	}
	return postfixStack
}

func (c *Compiler) genExpressionHelper(frame *stackFrame, node astExpression) astStack {
	pf := astStack{}
	for i := 0; i < len(node); i++ {
		stmt := node[i]
		postFix := c.genNode(frame, stmt)
		for i := 0; i < postFix.len(); i++ {
			pf.push(postFix.atIndex(i))
		}
	}
	return convertToPostfix(pf)
}

func (c *Compiler) genExpression(frame *stackFrame, node astExpression) {
	pf := c.genExpressionHelper(frame, node)
	c.genPostfix(frame, pf)
}

func (c *Compiler) genAssignArrayIndex(frame *stackFrame, node astAssign) {
	arrayName := node.Left.(astVarArrayIndex).value.Value
	if _, ok := frame.locals.get(localVar(arrayName)); ok {
		// gen right expression
		c.genExpression(frame, node.Right.(astExpression))
		// gen array index expression
		c.genExpression(frame, node.Left.(astVarArrayIndex).index)

		// array set value: get index from store[rsp-1]
		// get set value from store[rsp-2]

		c.Push(command{
			code: opCALL,
			op1:  "runtime.array",
			op2: astFunCallArgs{
				[]astNode{astFuncArgValue(arrayName)}, // self array
				[]astNode{astFuncArgValue("set")},     // method name
			},
		})
		*c.currRSP -= 2 // array_name + array_index
	} else {
		panic("try to assign array index for non existent array")
	}
}

func (c *Compiler) genAssign(frame *stackFrame, node astAssign) {
	if _, ok := node.Left.(astVarArrayIndex); ok {
		c.genAssignArrayIndex(frame, node)
		return
	}
	// astVarDec.Value = var name
	name := node.Left.(astVarDec).Value
	// book store space for the given var
	if n, ok := frame.locals.get(localVar(name)); ok && !node.IsLet {
		c.genExpression(frame, node.Right.(astExpression))
		c.Push(command{
			code: opPOP,
			op1:  n,
		})
	} else {
		vName := localVar(name)
		switch node.Right.(type) {
		case astExpression:

			c.Push(command{
				code: opPUSH,
				op1:  nilValue{},
			})
			c.genExpression(frame, node.Right.(astExpression))
			c.Push(command{
				code: opPOP,
				op1:  RSP(0),
			})
			frame.locals.set(vName, RBP(*c.currRSP-c.currRBP-1))
		case astArrayDecl:
			c.Push(command{
				code: opARRAY,
				op1:  name,
			})
			c.Push(command{
				code: opPUSH,
				op1:  arrIndex(name),
				op2:  fmt.Sprintf(`arr loc %s`, name),
			})
			frame.locals.set(vName, nil)
		}
	}
}

func (c *Compiler) genReturn(frame *stackFrame, node astReturn) {
	doesReturn := false
	if node.Stmt != nil {
		c.genExpression(frame, *node.Stmt) // result will be on top of store
		doesReturn = true                  // so tell the vm to use it a return value
	}

	c.Push(command{
		code: opRETURN,
		op1:  frame.funcReturnPC,
		op2:  doesReturn, // tell the VM it's plain return, so we need to push UNDEFINED as a return value
	})
}

func (c *Compiler) genBody(frame *stackFrame, body astBody) {
	for i := 0; i < len(body); i++ {
		node := body[i]
		switch node.(type) {
		case astAssign:
			c.genAssign(frame, node.(astAssign))
		case astReturn:
			c.genReturn(frame, node.(astReturn))
		case astFor:
			c.genFor(frame, node.(astFor))
		case astIf:
			c.genIf(frame, node.(astIf))
		case astExpression:
			c.genExpression(frame, node.(astExpression))
		}
	}
}

func (c *Compiler) genFor(frame *stackFrame, body astFor) {
	c.startCxt(frame)
	switch body.Pre.(type) {
	case astAssign:
		c.genAssign(frame, body.Pre.(astAssign))
	case astExpression:
		c.genExpression(frame, body.Pre.(astExpression))
	}

	endOfFor := intP()
	condPC := c.pc
	switch body.Cond.(type) {
	case astAssign:
		c.genAssign(frame, body.Cond.(astAssign))
	case astExpression:
		c.genExpression(frame, body.Cond.(astExpression))
	}

	c.Push(command{
		code: opCMP,
		op1:  endOfFor,
	})

	c.genBody(frame, body.Body)

	switch body.Post.(type) {
	case astAssign:
		c.genAssign(frame, body.Post.(astAssign))
	case astExpression:
		c.genExpression(frame, body.Post.(astExpression))
	}
	c.Push(command{
		code: opJUMP,
		op1:  &condPC,
	})

	*endOfFor = c.pc
	c.endCtx(frame)
}

func intP() *int {
	i := 0
	return &i
}

func (c *Compiler) genIf(frame *stackFrame, node astIf) {
	c.startCxt(frame)
	// condition
	c.genExpression(frame, node.Cond.(astExpression))
	if1 := intP()
	ifOnly := true
	c.Push(command{
		code: opCMP,
		op1:  if1,
	})

	c.genBody(frame, node.Body)
	endOfIf := intP()
	c.Push(command{
		code: opJUMP,
		op1:  endOfIf,
	})

	if node.ElseIfs != nil {
		*if1 = c.pc
		ifOnly = false
		for i := 0; i < len(node.ElseIfs); i++ {
			c.startCxt(frame)
			elseIf := intP()
			n := node.ElseIfs[i]
			c.genExpression(frame, n.Cond.(astExpression))
			c.Push(command{
				code: opCMP,
				op1:  elseIf,
			})
			c.genBody(frame, n.Body)
			c.Push(command{
				code: opJUMP,
				op1:  endOfIf,
			})
			c.endCtx(frame)
			*elseIf = c.pc
		}
	}

	if node.Else.Body != nil {
		ifOnly = false
		if node.ElseIfs == nil {
			*if1 = c.pc
		}
		c.startCxt(frame)
		c.genBody(frame, node.Else.Body)
		c.endCtx(frame)
	}
	c.endCtx(frame)
	*endOfIf = c.pc

	if ifOnly {
		*if1 = c.pc
	}
}
func (c *Compiler) startCxt(frame *stackFrame) {
	frame.locals.store.Push(vmLocals{})
}
func (c *Compiler) endCtx(frame *stackFrame) {
	dd := frame.locals.pop()
	// clean context local vars
	for i := 0; i < len(dd); i++ {
		c.Push(command{
			code: opPOP,
		})
	}
}
