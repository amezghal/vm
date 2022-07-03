package main

import (
	"fmt"
	"math"
	"strconv"
	"strings"
	"vm/token"
)

/*
Number logic
*/
var mainCursor = 0
var maxCursor = 300
var tokens []Token

const (
	funcDeclNode = iota
	forNode
	funcCallNode
	assignNode
	unaryNode
	literalNode
	literalGroupNode
	ifNode
	elseIfNode
	elseNode
	funcCallArgsNode
	funcDeclArgsNode
	varDecNode
	funcArgValueNode
	expressionNode
	opNode
	valueNode
	bodyNode
	returnNode
	capturedGroupNode
	arrayDeclNode
	arrayIndexNode
)

type Amez struct {
	tok   token.Token
	value interface{}
}

/*
 same as javascript
	variants
"5"  ADD           "4"            = "54"
"5"  ADD           int(4)         = "54"
"5"  ADD           float(12.34)   = "512.34"
"5" MUL|QUO|SUB   ?              = NAN
"aa" MUL|QUO|SUB   ?              = NAN
*/

type Number interface {
	int64 | float64
}

func doArith(op token.Token, operand1, operand2 interface{}) interface{} {
	// accept only int64, float64, string|+operation
	var (
		op1IsInt, op1IsFloat, op1IsString bool
		op2IsInt, op2IsFloat, op2IsString bool
	)

	switch operand1.(type) {
	case int64:
		op1IsInt = true
	case float64:
		op1IsFloat = true
	case string:
		op1IsString = true
	default:
		panic(fmt.Sprintf("unvalid provided type %T", operand1))
	}

	switch operand2.(type) {
	case int64:
		op2IsInt = true
	case float64:
		op2IsFloat = true
	case string:
		op2IsString = true
	default:
		panic(fmt.Sprintf("unvalid provided type %T", operand2))
	}

	if (op1IsString && !op2IsString) || (!op1IsString && op2IsString) || ((op1IsString || op2IsString) && op != token.ADD) {
		panic("both params should be string if one is string and only + is available")
	}

	// only accept + for string
	if (op1IsString && op2IsString) && op == token.ADD {
		return operand1.(string) + operand2.(string)
	}

	calcInts := func(operator token.Token, op1, op2 int64) int64 {
		switch operator {
		case token.ADD:
			return op1 + op2
		case token.SUB:
			return op1 - op2
		case token.MUL:
			return op1 * op2
		case token.QUO:
			return op1 / op2
		default:
			return 0
		}
	}

	calcFloats := func(operator token.Token, op1, op2 float64) float64 {
		switch operator {
		case token.ADD:
			return op1 + op2
		case token.SUB:
			return op1 - op2
		case token.MUL:
			return op1 * op2
		case token.QUO:
			return op1 / op2
		case token.REM:
			return math.Mod(op1, op2)
		default:
			return .0
		}
	}

	if (op1IsInt && !op2IsInt) || (op == token.REM && op1IsInt) {
		operand1 = float64(operand1.(int64))
		op1IsFloat = true
	}

	if (!op1IsInt && op2IsInt) || (op == token.REM && op2IsInt) {
		operand2 = float64(operand2.(int64))
		op2IsFloat = true
	}

	if op == token.REM || op1IsFloat || op2IsFloat {
		return calcFloats(op, operand1.(float64), operand2.(float64))
	}

	// can be only int
	resultInt := calcInts(op, operand1.(int64), operand2.(int64))
	if op == token.QUO {
		resultFloat := calcFloats(op, float64(operand1.(int64)), float64(operand2.(int64)))
		if float64(resultInt) < resultFloat {
			return resultFloat
		}
	}

	return resultInt
}

func Arith(op token.Token, n1, n2 Amez) Amez {
	if n1.tok == token.NAN || n2.tok == token.NAN {
		return Amez{
			tok:   token.NAN,
			value: "NAN",
		}
	}

	isInt := func(src interface{}) bool {
		switch src.(type) {
		case int8, int16, int32, int64, int, uint8, uint16, uint32, uint64, uint:
			return true
		}
		return false
	}
	isFloat := func(src interface{}) bool {
		switch src.(type) {
		case float32, float64:
			return true
		}
		return false
	}

	asInt := func(src interface{}) int64 {
		switch src.(type) {
		case int8:
			return int64(src.(int8))
		case int16:
			return int64(src.(int16))
		case int32:
			return int64(src.(int32))
		case int64:
			return src.(int64)
		case int:
			return int64(src.(int))
		case uint8:
			return int64(src.(uint8))
		case uint16:
			return int64(src.(uint16))
		case uint32:
			return int64(src.(uint32))
		case uint64:
			return int64(src.(uint64))
		case uint:
			return int64(src.(uint))
		}
		return int64(0)
	}
	asFloat := func(src interface{}) float64 {
		switch src.(type) {
		case int8:
			return float64(src.(int8))
		case int16:
			return float64(src.(int16))
		case int32:
			return float64(src.(int32))
		case int64:
			return float64(src.(int64))
		case int:
			return float64(src.(int))
		case uint8:
			return float64(src.(uint8))
		case uint16:
			return float64(src.(uint16))
		case uint32:
			return float64(src.(uint32))
		case uint64:
			return float64(src.(uint64))
		case uint:
			return float64(src.(uint))
		case float32:
			return float64(src.(float32))
		case float64:
			return src.(float64)
		}
		return float64(0)
	}

	if op.IsOperator() {
		switch {
		case n1.tok == token.STRING:
			if op == token.ADD {
				result := strings.Builder{}
				result.WriteString(n1.value.(string))
				switch {
				case n2.tok == token.STRING:
					result.WriteString(n2.value.(string))
				case isInt(n2.value):
					result.Write(strconv.AppendInt([]byte{}, asInt(n2.value), 10))
				case isFloat(n2.value):
					result.WriteString(fmt.Sprintf("%.2f", asFloat(n2.value)))
				}
				return Amez{
					tok:   token.STRING,
					value: result.String(),
				}
			} else {
				// other ops: operand1 should be convertible to NUMBER and operand2 should be of type number
				var result interface{}
				if v, err := strconv.ParseFloat(n1.value.(string), 64); err == nil {
					if n2.tok == token.NUMBER {
						result = doArith(op, asFloat(v), asFloat(n2.value))
					} else if v2, err := strconv.ParseFloat(n2.value.(string), 64); err == nil {
						result = doArith(op, asFloat(v), asFloat(v2))
					}
				}

				if result != nil {
					return Amez{
						tok:   token.NUMBER,
						value: result,
					}
				}
			}

		case isFloat(n1.value):
			var rr interface{}
			switch {
			case isInt(n2.value), isFloat(n2.value):
				rr = doArith(op, asFloat(n1.value), asFloat(n2.value))
			case n2.tok == token.STRING:
				if v, err := strconv.ParseFloat(n2.value.(string), 64); err == nil {
					rr = doArith(op, asFloat(n1.value), asFloat(v))
				} else if op == token.ADD {
					result := fmt.Sprintf(`%.2f%s`, asFloat(n1.value), n2.value.(string))
					return Amez{
						tok:   token.STRING,
						value: result,
					}
				}
			}

			if rr != nil {
				return Amez{
					tok:   token.NUMBER,
					value: rr,
				}
			}
		case isInt(n1.value):
			var rr interface{}
			switch {
			case isInt(n2.value):
				rr = doArith(op, asInt(n1.value), asInt(n2.value))
			case isFloat(n2.value):
				rr = doArith(op, asFloat(n1.value), asFloat(n2.value))
			case n2.tok == token.STRING:
				if v, err := strconv.ParseInt(n2.value.(string), 10, 64); err == nil {
					rr = doArith(op, asInt(n1.value), asInt(v))
				} else if v, err := strconv.ParseFloat(n2.value.(string), 64); err == nil {
					rr = doArith(op, asFloat(n1.value), asFloat(v))
				} else {
					if op == token.ADD {
						result := strings.Builder{}
						result.Write(strconv.AppendInt([]byte{}, asInt(n1.value), 10))
						result.WriteString(n2.value.(string))
						return Amez{
							tok:   token.STRING,
							value: result.String(),
						}
					}
				}
			}

			if rr != nil {
				return Amez{
					tok:   token.NUMBER,
					value: rr,
				}
			}
		}

	}

	return Amez{
		tok:   token.NAN,
		value: "NAN",
	}
}

type astNode interface {
	Kind() int
}

type astCapturedGroupNode astStack

func (astCapturedGroupNode) Kind() int {
	return capturedGroupNode
}

type astArrayDecl struct{}

func (astArrayDecl) Kind() int {
	return arrayDeclNode
}

type astVarArrayIndex struct {
	index astExpression
	value astVarDec
}

func (astVarArrayIndex) Kind() int {
	return arrayIndexNode
}

type astFuncDecl struct {
	Type   int
	Name   string
	Args   astFuncDeclArgs
	Body   astBody
	Return astExpression
}

func (astFuncDecl) Kind() int {
	return funcDeclNode
}

type astAssign struct {
	Left  astNode
	Right astNode
	IsLet bool
}

func (astAssign) Kind() int {
	return assignNode
}

type astLiteral struct {
	Unary *astUnary
	Value astNode
}

func (astLiteral) Kind() int {
	return literalNode
}

type astReturn struct {
	Stmt *astExpression
}

func (astReturn) Kind() int {
	return returnNode
}

type astUnary string

func (astUnary) Kind() int {
	return unaryNode
}

type astExpression []astNode

func (astExpression) Kind() int {
	return expressionNode
}

type astBody []astNode

func (astBody) Kind() int {
	return bodyNode
}

type astFuncCall struct {
	Name string
	Args astFunCallArgs
}

func (astFuncCall) Kind() int {
	return funcCallNode
}

type astIf struct {
	Cond    astNode
	Body    astBody
	ElseIfs []astElseIf
	Else    astElse
}

func (astIf) Kind() int {
	return ifNode
}

type astElseIf struct {
	Cond astNode
	Body astBody
}

func (astElseIf) Kind() int {
	return elseIfNode
}

type astElse struct {
	Body astBody
}

func (astElse) Kind() int {
	return elseNode
}

type astFor struct {
	Pre  astNode
	Cond astNode
	Post astNode
	Body astBody
}

func (astFor) Kind() int {
	return forNode
}

type astVarDec struct {
	Token
}

func (astVarDec) Kind() int {
	return varDecNode
}

type astFuncArgValue string

func (astFuncArgValue) Kind() int {
	return funcArgValueNode
}

type astOp struct {
	Token Token
}

func (astOp) Kind() int {
	return opNode
}

type astLitGroup struct {
	Unary *astUnary
	Value astNode
}

func (astLitGroup) Kind() int {
	return literalGroupNode
}

type astValue struct {
	Value Token
}

func (astValue) Kind() int {
	return valueNode
}

type astFunCallArgs []astExpression

func (astFunCallArgs) Kind() int {
	return funcCallArgsNode
}

type astFuncDeclArgs []astNode

func (astFuncDeclArgs) Kind() int {
	return funcDeclArgsNode
}

func ntk() string {
	if mainCursor >= maxCursor {
		return ""
	}
	ret := tokens[mainCursor]
	mainCursor++
	return ret.Kind
}

func peek() string {
	if mainCursor >= maxCursor {
		return ""
	}
	ret := tokens[mainCursor]
	return ret.Kind
}

func current() string {
	if mainCursor-1 < 0 {
		return ""
	}
	ret := tokens[mainCursor-1]
	return ret.Kind
}

func parseScript(tt []Token) (bool, []astNode) {
	tokens = tt
	mainCursor = 0
	maxCursor = len(tt)
	found := false
	var tree []astNode
	for mainCursor < maxCursor {
		if node := parseFuncDecl(); node != nil {
			tree = append(tree, node)
			found = true
		} else if node := parseAssign(); node != nil {
			tree = append(tree, node)
			found = true
		} else {
			return false, nil
		}
	}
	return found, tree
}

func parseReturnStmt() astNode {
	cursor := mainCursor
	node := astReturn{}

	if ntk() == T_RETURN {
		if stmt := parseExpression(); stmt != nil {
			s := stmt.(astExpression)
			node.Stmt = &s
			if peek() == T_COMA {
				_ = ntk()
			}
		}
		return node
	}
	mainCursor = cursor
	return nil
}

func parseFuncDecl() astNode {

	cursor := mainCursor
	node := astFuncDecl{}
	if ntk() == T_FUNC_DEC {
		if ntk() == T_FUNC_CALL {
			node.Name = tokens[mainCursor-1].Value
			if ntk() == T_LPAR {
				if n := parseFuncDeclArgs(); n != nil {
					if !(ntk() == T_RPAR && ntk() == T_LBRACE) {
						goto fail
					}

					node.Args = n.(astFuncDeclArgs)

					if nBody := parseBody(); nBody != nil {
						node.Body = nBody.(astBody)
					} else {
						goto fail
					}

					if ntk() == T_RBRACE {
						return node
					}
				}
			}
		}
	}
fail:
	mainCursor = cursor
	return nil
}

func parseAssign() astNode {
	node := parseAssignOnly()
	if node != nil {
		if peek() == T_COMA {
			_ = ntk()
		}
	}
	return node
}

func parseAssignOnly() astNode {
	c := mainCursor
	node := astAssign{}
	if peek() == T_LET {
		node.IsLet = true
		_ = ntk()
	} else {
		if t := parseVarArrayIndex(); t != nil {
			if ntk() == T_ASSIGN {
				node.Left = t

				if t := parseExpression(); t != nil {
					node.Right = t
					return node
				}
			}
		}
	}

	if t := parseVar(); t != nil {
		if ntk() == T_ASSIGN {
			node.Left = t

			if t := parseArrayDecl(); t != nil {
				node.Right = t
				return node
			}

			if t := parseExpression(); t != nil {
				node.Right = t
				return node
			}
		}
	}

	mainCursor = c
	return nil
}

func parseVar() astNode {
	if peek() == T_VAR {
		_ = ntk()
		return astVarDec{
			tokens[mainCursor-1],
		}
	}
	return nil
}

func parseVarArrayIndex() astNode {
	c := mainCursor

	if peek() == T_VAR {
		_ = ntk()
		varDecl := astVarDec{
			tokens[mainCursor-1],
		}

		if ntk() == T_LINDEX {
			if node := parseExpression(); node != nil && ntk() == T_RINDEX {
				return astVarArrayIndex{
					index: node.(astExpression),
					value: varDecl,
				}
			}
		}
	}

	mainCursor = c
	return nil
}

func parseBody() astNode {
	node := astBody{}
	for mainCursor < maxCursor {
		//TODO this might be wrong if ";" is missing
		if e := parseAssign(); e != nil {
			node = append(node, e)
		} else if e := parseFor(); e != nil {
			node = append(node, e)
		} else if e := parseIf(); e != nil {
			node = append(node, e)
		} else if e := parseExpression(); e != nil {
			node = append(node, e)
		} else if e := parseReturnStmt(); e != nil {
			node = append(node, e)
		} else {
			break
		}
	}
	return node
}

func parseExpression() astNode {
	c := mainCursor
	node := astExpression{}
	_ = `
5 + 4 * 3 => 4 3 * 5 +
// mine
5 + 4 * 3 * (5 + 4 * 3) => 4 3 4 3 * 5 + * 5 +
// actually
5 + 4 * 3 * (5 + 4 * 3) => 4 3 4 3 * 5 + * 5 +
// it's associative :)
`
	for mainCursor < maxCursor {
		cc := mainCursor
		nLitNode := parseLit()
		if (nLitNode != nil && peek() == T_COMA) || nLitNode != nil {
			if nOpNode := parseIsOp(); nOpNode != nil {
				node = append(node, nLitNode)
				node = append(node, nOpNode)
				continue
			}
		}
		// rollback
		mainCursor = cc
		if nLit := parseLit(); nLit != nil {
			node = append(node, nLit)
			return node
		}
		break
	}

	mainCursor = c
	return nil
}

func isUnary(tokenType string) bool {
	// todo "!" is unary as well
	switch tokenType {
	case T_MINUS, T_NOT:
		return true
	}
	return false
}

func parseIsOp() astNode {
	t := peek()
	node := astOp{}
	switch t {
	case T_MINUS, T_PLUS, T_MULT, T_DIV, T_EQ, T_GT, T_GTE, T_LTE, T_LT, T_MOD:
		node.Token = tokens[mainCursor]
		_ = ntk()
		return node
	}
	return nil
}

func parseArrayDecl() astNode {
	if peek() == T_ARRAY {
		_ = ntk()
		return astArrayDecl{}
	}
	return nil
}

func parseLit0() astNode {
	c := mainCursor
	node := astLiteral{}

	if arrayIndexNode := parseVarArrayIndex(); arrayIndexNode != nil {
		return arrayIndexNode
	}

	tok := peek()
	if tok == T_NUMBER || tok == T_STRING || tok == T_VAR {
		node.Value = astValue{Value: tokens[mainCursor]}
		_ = ntk()
		return node
	}

	if gNode := parseGroup(); gNode != nil {
		return gNode
	}

	if fcNode := parseFuncCall(); fcNode != nil {
		node.Value = fcNode
		return node
	}
	mainCursor = c
	return nil
}

func parseLit() astNode {
	c := mainCursor
	var u *astUnary

	for {
		if isUnary(peek()) {
			if u != nil {
				u = nil
				_ = ntk()
				// the problem here is that next token can be a unary as well
				// the funny thing is that golang does not support successive unary operators
				// but javascript does support it, ex: ----+-5 is a valid expression in javascript but not in golang
				// since this a dynamic language, it's better to align with javascript
			} else {
				t := astUnary(ntk())
				u = &t
			}
		} else {
			break
		}
	}

	lit0 := parseLit0()
	if lit0 == nil {
		goto fail
	}
	switch lit0.(type) {
	case astLiteral:
		lit := lit0.(astLiteral)
		if lit.Unary != nil && u != nil {
			lit.Unary = nil
		} else if u != nil {
			lit.Unary = u
		}
		return lit
	case astLitGroup:
		lit := lit0.(astLitGroup)
		if lit.Unary != nil && u != nil {
			lit.Unary = nil
		} else if u != nil {
			lit.Unary = u
		}
		return lit
	case astVarArrayIndex:
		return lit0.(astVarArrayIndex)
	default:
		return nil
	}
fail:
	mainCursor = c
	return nil
}

func parseGroup() astNode {
	c := mainCursor
	if ntk() == T_LPAR {
		nGroup := astLitGroup{}
		if n := parseExpression(); n != nil && ntk() == T_RPAR {
			nGroup.Value = n
			return nGroup
		}
	}
	mainCursor = c
	return nil
}

func parseFuncCall() astNode {
	c := mainCursor
	node := astFuncCall{}
	tok := tokens[mainCursor]
	if ntk() == T_FUNC_CALL && ntk() == T_LPAR {
		if n := parseFuncCallArgs(); n != nil && ntk() == T_RPAR {
			node.Args = n.(astFunCallArgs)
			node.Name = tok.Value
			return node
		}
	}
	mainCursor = c
	return nil
}

func parseFuncCallArgs() astNode {
	c := mainCursor
	node := astFunCallArgs{}
	if peek() == T_RPAR {
		return node
	}
	found := false
	for mainCursor < maxCursor {
		if n := parseExpression(); n != nil {
			node = append(node, n.(astExpression))
			found = true
			if peek() == T_SEMI {
				_ = ntk()
				continue
			}
		}
		break
	}
	if !found {
		mainCursor = c
		return nil
	}
	return node
}

func parseElseIf() astNode {
	c := mainCursor
	node := astElseIf{}
	if ntk() == T_ELSE && ntk() == T_IF && ntk() == T_LPAR {
		if cond := parseExpression(); cond != nil {
			node.Cond = cond
			if ntk() == T_RPAR && ntk() == T_LBRACE {
				if body := parseBody(); body != nil && ntk() == T_RBRACE {
					node.Body = body.(astBody)
					return node
				}
			}
		}
	}

	mainCursor = c
	return nil
}

func parseElse() astNode {
	c := mainCursor
	node := astElse{}

	if ntk() == T_ELSE && ntk() == T_LBRACE {
		if body := parseBody(); body != nil && ntk() == T_RBRACE {
			node.Body = body.(astBody)
			return node
		}
	}

	mainCursor = c
	return nil
}

func parseIf() astNode {
	c := mainCursor
	node := astIf{}
	if ntk() == T_IF && ntk() == T_LPAR {
		if cond := parseExpression(); cond != nil {
			node.Cond = cond
			if ntk() == T_RPAR && ntk() == T_LBRACE {
				if body := parseBody(); body != nil && ntk() == T_RBRACE {
					node.Body = body.(astBody)
					// check for elseif's
					for mainCursor < maxCursor {
						if nElseIf := parseElseIf(); nElseIf != nil {
							node.ElseIfs = append(node.ElseIfs, nElseIf.(astElseIf))
						} else {
							break
						}
					}

					if nElse := parseElse(); nElse != nil {
						node.Else = nElse.(astElse)
					}

					return node
				}
			}
		}
	}

	mainCursor = c
	return nil
}

func parseFor() astNode {
	c := mainCursor
	node := astFor{}
	if ntk() == T_FOR && ntk() == T_LPAR {
		if nAssign := parseAssignOnly(); nAssign != nil && parseComa() {
			node.Pre = nAssign
			if nExpress := parseExpression(); nExpress != nil {
				if parseComa() {
					node.Cond = nExpress
					if nAssign := parseAssignOnly(); nAssign != nil && ntk() == T_RPAR && ntk() == T_LBRACE {
						node.Post = nAssign
						if nBody := parseBody(); nBody != nil && ntk() == T_RBRACE {
							node.Body = nBody.(astBody)
							return node
						}
					}
				}
			}
		}

	}
	mainCursor = c
	return nil
}

func parseComa() bool {
	c := mainCursor
	if peek() == T_COMA {
		_ = ntk()
		return true
	}
	mainCursor = c
	return false
}

func parseFuncDeclArgs() astNode {

	c := mainCursor
	node := astFuncDeclArgs{}
	if current() == T_LPAR && peek() == T_RPAR {
		return node
	}
	for mainCursor < maxCursor {
		if ntk() == T_VAR {
			node = append(node, astVarDec{
				tokens[mainCursor-1],
			})
			if peek() == T_RPAR {
				return node
			}

			if peek() != T_SEMI && ntk() == T_LPAR {
				return node
			}

			if peek() == T_SEMI {
				_ = ntk()
				continue
			}
		}
		break
	}

	mainCursor = c
	return nil
}
