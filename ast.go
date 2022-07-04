package vm

import (
	"fmt"
	"math"
)

var mainCursor = 0
var maxCursor = math.MaxInt - 1
var tokens []Token

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

// all astNodes must be of type astNode
type astNode interface {
	Kind()
}

type astCapturedGroupNode astStack

func (astCapturedGroupNode) Kind() {}

type astArrayDecl struct{}

func (astArrayDecl) Kind() {}

type astVarArrayIndex struct {
	index astExpression
	value astVarDec
}

func (astVarArrayIndex) Kind() {}

type astFuncDecl struct {
	Type   int
	Name   string
	Args   astFuncDeclArgs
	Body   astBody
	Return astExpression
}

func (astFuncDecl) Kind() {}

type astAssign struct {
	Left  astNode
	Right astNode
	IsLet bool
}

func (astAssign) Kind() {}

type astLiteral struct {
	Unary *astUnary
	Value astNode
}

func (astLiteral) Kind() {}

type astReturn struct {
	Stmt *astExpression
}

func (astReturn) Kind() {}

type astUnary string

func (astUnary) Kind() {}

type astExpression []astNode

func (astExpression) Kind() {}

type astBody []astNode

func (astBody) Kind() {}

type astFuncCall struct {
	Name string
	Args astFunCallArgs
}

func (astFuncCall) Kind() {}

type astIf struct {
	Cond    astNode
	Body    astBody
	ElseIfs []astElseIf
	Else    astElse
}

func (astIf) Kind() {}

type astElseIf struct {
	Cond astNode
	Body astBody
}

func (astElseIf) Kind() {}

type astElse struct {
	Body astBody
}

func (astElse) Kind() {}

type astFor struct {
	Pre  astNode
	Cond astNode
	Post astNode
	Body astBody
}

func (astFor) Kind() {}

type astVarDec struct {
	Token
}

func (astVarDec) Kind() {}

type astFuncArgValue string

func (astFuncArgValue) Kind() {}

type astOp struct {
	Token Token
}

func (astOp) Kind() {}

func (op astOp) Prec() int {
	switch op.Token.Kind {
	case T_OR:
		return 1
	case T_AND:
		return 2
	case T_EQ, T_LT, T_LTE, T_GT, T_GTE:
		return 3
	case T_ADD, T_SUB:
		return 4
	case T_MUL, T_QUO, T_MOD:
		return 5
	}
	return 0
}

type astLitGroup struct {
	Unary *astUnary
	Value astNode
}

func (astLitGroup) Kind() {}

type astValue struct {
	Value Token
}

func (astValue) Kind() {}

type astFunCallArgs []astExpression

func (astFunCallArgs) Kind() {}

type astFuncDeclArgs []astNode

func (astFuncDeclArgs) Kind() {}

// get the next token from the lexer and make progress
func ntk() string {
	if mainCursor >= maxCursor {
		return ""
	}
	ret := tokens[mainCursor]
	mainCursor++
	return ret.Kind
}

// peek the next token, look-ahead
func peek() string {
	if mainCursor >= maxCursor {
		return ""
	}
	ret := tokens[mainCursor]
	return ret.Kind
}

// get the current token
func current() string {
	if mainCursor-1 < 0 {
		return ""
	}
	ret := tokens[mainCursor-1]
	return ret.Kind
}

var errors []string

func errorExpect(expected string) {
	current := tokens[mainCursor]
	errors = append(errors, fmt.Sprintf(`Expected %s at %d:%d got %s`, expected, current.Line, current.Col, current.Kind))
}

func printAllErrors() {
	for _, err := range errors {
		fmt.Println(err)
	}
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

func parseFuncDecl() astNode {

	cursor := mainCursor
	node := astFuncDecl{} // the expected `astNode` type to be returned

	// as we make progress parsing, we store parse errors here
	var parseErr string

	if ntk() == T_FUNC_DEC { // once we have the `func` declaration keyword, we should expect a valid function decl
		if peek() != T_FUNC_CALL {
			parseErr = T_FUNC_CALL
			goto fail
		}
		ntk() // read next token to make progress

		node.Name = tokens[mainCursor-1].Value

		if peek() != T_LPAREN {
			parseErr = T_LPAREN
			goto fail
		}
		ntk() // make progress

		if n := parseFuncDeclArgs(); n != nil {
			if peek() != T_RPAREN {
				parseErr = T_RPAREN
				goto fail
			}
			ntk()

			if peek() != T_LBRACE {
				parseErr = T_LBRACE
				goto fail
			}
			ntk()

			node.Args = n.(astFuncDeclArgs)

			if nBody := parseBody(); nBody != nil {
				node.Body = nBody.(astBody)
			} else {
				goto fail
			}

			if peek() != T_RBRACE {
				parseErr = T_RBRACE
				goto fail
			}
			ntk()
			return node
		}
	}
fail:
	errorExpect(parseErr)
	mainCursor = cursor
	return nil
}

func parseAssign() astNode {
	node := parseAssignOnly()
	if node != nil {
		if peek() == T_SEMICOLON {
			_ = ntk()
		}
	}
	return node
}

func parseReturnStmt() astNode {
	cursor := mainCursor
	node := astReturn{}

	if ntk() == T_RETURN {
		if peek() == T_SEMICOLON {
			_ = ntk()
			return node
		}
		if stmt := parseExpression(); stmt != nil {
			s := stmt.(astExpression)
			node.Stmt = &s
			if peek() == T_SEMICOLON {
				_ = ntk()
			}
		}
		return node
	}
	mainCursor = cursor
	return nil
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

		if ntk() == T_LBRACK {
			if node := parseExpression(); node != nil && ntk() == T_RBRACK {
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

	for mainCursor < maxCursor {
		cc := mainCursor
		nLitNode := parseLit()
		if (nLitNode != nil && peek() == T_SEMICOLON) || nLitNode != nil {
			if nOpNode := parseOp(); nOpNode != nil {
				node = append(node, nLitNode)
				node = append(node, nOpNode)
				continue //
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
	switch tokenType {
	case T_SUB, T_NOT:
		return true
	}
	return false
}

// cleanup this, not very clean
// this should be supported in the lexer
func parseOp() astNode {
	t := tokens[mainCursor]
	if t.IsOp() {
		node := astOp{}
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

// rename to basicLiteral
func parseBasicLiteral() astNode {
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
	// keep parsing Unary ops
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

	lit0 := parseBasicLiteral()
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
	if ntk() == T_LPAREN {
		nGroup := astLitGroup{}
		if n := parseExpression(); n != nil && ntk() == T_RPAREN {
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
	if ntk() == T_FUNC_CALL && ntk() == T_LPAREN {
		if n := parseFuncCallArgs(); n != nil && ntk() == T_RPAREN {
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
	if peek() == T_RPAREN {
		return node
	}
	found := false
	for mainCursor < maxCursor {
		if n := parseExpression(); n != nil {
			node = append(node, n.(astExpression))
			found = true
			if peek() == T_COMMA {
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
	if ntk() == T_ELSE && ntk() == T_IF && ntk() == T_LPAREN {
		if cond := parseExpression(); cond != nil {
			node.Cond = cond
			if ntk() == T_RPAREN && ntk() == T_LBRACE {
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
	if ntk() == T_IF && ntk() == T_LPAREN {
		if cond := parseExpression(); cond != nil {
			node.Cond = cond
			if ntk() == T_RPAREN && ntk() == T_LBRACE {
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
	if ntk() == T_FOR && ntk() == T_LPAREN {
		if nAssign := parseAssignOnly(); nAssign != nil && parseComa() {
			node.Pre = nAssign
			if nExpress := parseExpression(); nExpress != nil {
				if parseComa() {
					node.Cond = nExpress
					if nAssign := parseAssignOnly(); nAssign != nil && ntk() == T_RPAREN && ntk() == T_LBRACE {
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
	if peek() == T_SEMICOLON {
		_ = ntk()
		return true
	}
	mainCursor = c
	return false
}

func parseFuncDeclArgs() astNode {

	c := mainCursor
	node := astFuncDeclArgs{}
	if current() == T_LPAREN && peek() == T_RPAREN {
		return node
	}
	for mainCursor < maxCursor {
		if ntk() == T_VAR {
			node = append(node, astVarDec{
				tokens[mainCursor-1],
			})
			if peek() == T_RPAREN {
				return node
			}

			if peek() != T_COMMA && ntk() == T_LPAREN {
				return node
			}

			if peek() == T_COMMA {
				_ = ntk()
				continue
			}
		}
		break
	}

	mainCursor = c
	return nil
}
