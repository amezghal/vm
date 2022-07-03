package main

import (
	"fmt"
	"regexp"
	"strings"
)

type Lexer struct {
	cursor int
	col    int
	line   int
	source []byte
	errors []error
}

func (l *Lexer) Run(code []byte) ([]Token, error) {
	l.cursor = 0
	l.source = code
	l.line = 1

	var tokens []Token
	for ; l.cursor < len(l.source); l.cursor++ {
		col := l.col
		line := l.line
		char := l.getCurrentChar()
		if char == '"' {
			tokens = append(tokens, Token{
				Kind:  T_STRING,
				Value: l.parseString(),
				Pos:   0,
				Line:  line,
				Col:   col,
			})
		} else if char == '=' {
			tValue := string(char)
			tKind := T_ASSIGN
			if l.peek() == '=' {
				l.readChar()
				tKind = T_EQ
				tValue = "=="
			}

			tokens = append(tokens, Token{
				Kind:  tKind,
				Value: tValue,
				Pos:   0,
				Line:  line,
				Col:   col,
			})

		} else if char == '<' || char == '>' {
			tValue := string(char)
			tKind := T_LT

			peek := l.peek()

			switch char {
			case '<':
				tKind = T_LT
				if peek == '=' {
					tKind = T_LTE
					tValue = "<="
					l.readChar()
				}
			case '>':
				tKind = T_GT
				if peek == '=' {
					tKind = T_GTE
					tValue = ">="
					l.readChar()
				}
			}
			tokens = append(tokens, Token{
				Kind:  tKind,
				Value: tValue,
				Pos:   0,
				Line:  line,
				Col:   col,
			})
		} else if isNumber(char) {
			tokens = append(tokens, Token{
				Kind:  T_NUMBER,
				Value: l.parseNumber(),
				Pos:   0,
				Line:  line,
				Col:   col,
			})
		} else if char == '(' {
			tokens = append(tokens, Token{
				Kind:  T_LPAR,
				Value: string(char),
				Pos:   0,
				Line:  line,
				Col:   col,
			})
		} else if char == ')' {
			tokens = append(tokens, Token{
				Kind:  T_RPAR,
				Value: string(char),
				Pos:   0,
				Line:  line,
				Col:   col,
			})
		} else if isOp, opValue := parseOp(char); isOp {
			tokens = append(tokens, Token{
				Kind:  opValue,
				Value: string(char),
				Pos:   0,
				Line:  line,
				Col:   col,
			})
		} else if char == ';' {
			tokens = append(tokens, Token{
				Kind:  T_COMA,
				Value: string(char),
				Pos:   0,
				Line:  line,
				Col:   col,
			})
		} else if char == ',' {
			tokens = append(tokens, Token{
				Kind:  T_SEMI,
				Value: string(char),
				Pos:   0,
				Line:  line,
				Col:   col,
			})
		} else if char == '{' {
			tokens = append(tokens, Token{
				Kind:  T_LBRACE,
				Value: string(char),
				Pos:   0,
				Line:  line,
				Col:   col,
			})
		} else if char == '}' {
			tokens = append(tokens, Token{
				Kind:  T_RBRACE,
				Value: string(char),
				Pos:   0,
				Line:  line,
				Col:   col,
			})
		} else if char == '!' {
			tokens = append(tokens, Token{
				Kind:  T_NOT,
				Value: string(char),
				Pos:   0,
				Line:  line,
				Col:   col,
			})
		} else if char == '[' {
			tKind := T_LINDEX
			tValue := string(char)
			if l.peek() == ']' {
				l.readChar()
				tKind = T_ARRAY
				tValue = "[]"
			}
			tokens = append(tokens, Token{
				Kind:  tKind,
				Value: tValue,
				Pos:   0,
				Line:  line,
				Col:   col,
			})
		} else if char == ']' {
			tokens = append(tokens, Token{
				Kind:  T_RINDEX,
				Value: string(char),
				Pos:   0,
				Line:  line,
				Col:   col,
			})
		} else if isLiteral(string(char)) {
			kind := T_FUNC_CALL
			value := l.parseLiteral()

			peekNonWSValue := l.peekNonWS()
			peek := l.peek()

			switch {
			case value == "let" && isWS(peek):
				kind = T_LET
			case value == "for" && peekNonWSValue == '(':
				kind = T_FOR
			case value == "fn" && isWS(peek):
				kind = T_FUNC_DEC
			case value == "print" && peekNonWSValue == '(':
				kind = T_IFUNC
			case value == "return" && isWS(peek):
				kind = T_RETURN
			case value == "if" && peekNonWSValue == '(':
				kind = T_IF
			case value == "else":
				kind = T_ELSE
			case value == "and":
				kind = T_AND
			case value == "or":
				kind = T_OR

			case peekNonWSValue != '(':
				kind = T_VAR
			}

			tokens = append(tokens, Token{
				Kind:  kind,
				Value: value,
				Pos:   0,
				Line:  line,
				Col:   col,
			})
		} else if char == ' ' || char == '\t' {
			l.col++
		} else if char == '\n' {
			l.line++
			l.col = 1
		} else {
			return nil, fmt.Errorf(`unexpected char %s at col=%d line=%d`, string(char), l.col, l.line)
		}
	}
	return tokens, nil
}

func (l *Lexer) getChar() byte {
	l.cursor++
	l.col++
	char := l.source[l.cursor]

	return char
}
func (l *Lexer) readChar() {
	l.cursor++
	l.col++
	char := l.source[l.cursor]
	if char == '\n' {
		l.line++
	}
}

func (l *Lexer) getCurrentChar() byte {
	return l.source[l.cursor]
}

func (l *Lexer) peek() byte {
	return l.source[l.cursor+1]
}

func (l *Lexer) peekNonWS() byte {
	pCursor := l.cursor + 1
	for pCursor < len(l.source) {
		char := l.source[pCursor]
		if char == ' ' || char == '\t' || char == 'n' || char == '\r' {
			pCursor++
			continue
		}
		break
	}
	if pCursor >= len(l.source) {
		return ' '
	}
	return l.source[pCursor]
}

func (l *Lexer) parseString() string {
	var result strings.Builder

	for l.cursor < len(l.source) {
		c := l.getChar()
		// TODO support multiline
		if c == '"' {
			break
		}
		result.Write([]byte{c})
	}
	return result.String()
}

func (l *Lexer) parseLiteral() string {
	var result string
	result += string(l.getCurrentChar())

	for l.cursor < len(l.source) {
		c := string(l.getChar())

		if !isLiteral(result + c) {
			l.cursor--
			l.col--
			break
		}
		result += c
	}
	return result
}

func (l *Lexer) parseNumber() string {
	comaFound := false
	var result []byte
	// push first char
	result = append(result, l.getCurrentChar())
	for l.cursor < len(l.source)-1 {
		char := l.getChar()
		if isNumber(char) {
			result = append(result, char)
		} else if char == '.' && !comaFound {
			comaFound = true
			result = append(result, char)
		} else {
			l.cursor--
			l.col--
			break
		}
	}
	return string(result)
}

func isNumber(char byte) bool {
	return char >= '0' && char <= '9'
}

func parseOp(char byte) (bool, string) {
	switch char {
	case '+':
		return true, T_PLUS
	case '-':
		return true, T_MINUS
	case '*':
		return true, T_MULT
	case '/':
		return true, T_DIV
	case '%':
		return true, T_MOD
	}
	return false, ""
}

// this is stupid: use ASCII checks
var validLiteral = regexp.MustCompile(`^([a-zA-Z_]+)([a-zA-Z0-9_\.]*)$`)

func isLiteral(in string) bool {
	return validLiteral.MatchString(in)
}

func isWS(char byte) bool {
	return char == ' ' || char == '\t' || char == '\r'
}
