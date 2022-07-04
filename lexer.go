package vm

import (
	"fmt"
	"strings"
	"unicode"
)

const (
	T_NAN       = "__NAN__"
	T_UNDEFINED = "__UNDEFINED__"
	T_IF        = "__IF__"
	T_ELSE      = "__ELSE__"
	T_RETURN    = "__RETURN__"
	T_LBRACE    = "__{__"
	T_RBRACE    = "__}__"
	T_LBRACK    = "__[__"
	T_RBRACK    = "__]__"
	T_LPAREN    = "__(__"
	T_RPAREN    = "__)__"

	T_ARRAY     = "__[]__"
	T_NOT       = "__!__"
	T_AND       = "__and__"
	T_OR        = "__or__"
	T_SEMICOLON = "__;__"
	T_COMMA     = "__,__"
	T_ASSIGN    = "__=__"
	T_EQ        = "__==__"
	T_LT        = "<"
	T_GT        = ">"
	T_LTE       = "<="
	T_GTE       = ">="
	T_LET       = "__LET__"
	T_FOR       = "__FOR__"

	T_FUNC_CALL = "__FUNC_CALL__"
	T_FUNC_DEC  = "__FUNC_DEC__"
	T_ADD       = "__+__"
	T_SUB       = "__-__"
	T_MUL       = "__*__"
	T_QUO       = "__/__"
	T_MOD       = "__%__"
	T_VAR       = "__VAR__"
	T_NUMBER    = "__NUMBER__"
	T_UNARY     = "__UNARY__"
	T_STRING    = "__STRING__"
)

type Token struct {
	Kind  string
	Value string
	Pos   int
	Line  int
	Col   int
}

func (t *Token) String() string {
	return fmt.Sprintf(`Token kind=%s, value=%s, pos=%d`, t.Kind, t.Value, t.Pos)
}

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
		} else if unicode.IsDigit(rune(char)) {
			tokens = append(tokens, Token{
				Kind:  T_NUMBER,
				Value: l.parseNumber(),
				Pos:   0,
				Line:  line,
				Col:   col,
			})
		} else if char == '(' {
			tokens = append(tokens, Token{
				Kind:  T_LPAREN,
				Value: string(char),
				Pos:   0,
				Line:  line,
				Col:   col,
			})
		} else if char == ')' {
			tokens = append(tokens, Token{
				Kind:  T_RPAREN,
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
				Kind:  T_SEMICOLON,
				Value: string(char),
				Pos:   0,
				Line:  line,
				Col:   col,
			})
		} else if char == ',' {
			tokens = append(tokens, Token{
				Kind:  T_COMMA,
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
			tKind := T_LBRACK
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
				Kind:  T_RBRACK,
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
	commaFound := false
	var result []byte
	// push first char
	result = append(result, l.getCurrentChar())
	for l.cursor < len(l.source)-1 {
		char := l.getChar()
		if unicode.IsDigit(rune(char)) {
			result = append(result, char)
		} else if char == '.' && !commaFound {
			commaFound = true
			result = append(result, char)
		} else {
			l.cursor--
			l.col--
			break
		}
	}
	return string(result)
}

func parseOp(char byte) (bool, string) {
	switch char {
	case '+':
		return true, T_ADD
	case '-':
		return true, T_SUB
	case '*':
		return true, T_MUL
	case '/':
		return true, T_QUO
	case '%':
		return true, T_MOD
	}
	return false, ""
}

func isLiteral(in string) bool {
	for i, c := range in {
		if !unicode.IsLetter(c) && c != '_' && (i == 0 || !unicode.IsDigit(c)) {
			return false
		}
	}
	return in != ""
}

func isKeyword(word string) bool {
	keywords := []string{"break", "fn", "for", "let"}
	for _, k := range keywords {
		if k == word {
			return true
		}
	}
	return false
}

func isWS(char byte) bool {
	return char == ' ' || char == '\t' || char == '\r'
}
