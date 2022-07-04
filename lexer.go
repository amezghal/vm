package vm

import (
	"fmt"
	"strings"
	"unicode"
)

const (
	T_NAN       = "T_NAN"
	T_UNDEFINED = "T_UNDEFINED"
	T_IF        = "T_IF"
	T_ELSE      = "T_ELSE"
	T_RETURN    = "T_RETURN"
	T_LBRACE    = "T_{"
	T_RBRACE    = "T_}"
	T_LBRACK    = "T_["
	T_RBRACK    = "T_]"
	T_LPAREN    = "T_("
	T_RPAREN    = "T_)"

	T_ARRAY     = "T_[]"
	T_NOT       = "T_!"
	T_AND       = "T_and"
	T_OR        = "T_or"
	T_SEMICOLON = "T_;"
	T_COMMA     = "T_,"
	T_ASSIGN    = "T_="
	T_EQ        = "T_=="
	T_LT        = "<"
	T_GT        = ">"
	T_LTE       = "<="
	T_GTE       = ">="
	T_LET       = "T_LET"
	T_FOR       = "T_FOR"

	T_FUNC_CALL = "T_FUNC_CALL"
	T_FUNC_DEC  = "T_FUNC_DEC"
	T_ADD       = "T_+"
	T_SUB       = "T_-"
	T_MUL       = "T_*"
	T_QUO       = "T_/"
	T_MOD       = "T_%"
	T_VAR       = "T_VAR"
	T_NUMBER    = "T_NUMBER"
	T_STRING    = "T_STRING"
	T_COMMENT   = "T_COMMENT"
)

type Token struct {
	Kind  string
	Value string
	Line  int
	Col   int
}

func (t *Token) String() string {
	return fmt.Sprintf(`Token kind=%s, value=%s`, t.Kind, t.Value)
}

func (t *Token) IsOp() bool {
	switch t.Kind {
	case T_SUB, T_ADD, T_MUL, T_QUO, T_EQ, T_GT, T_GTE, T_LTE, T_LT, T_MOD, T_OR, T_AND:
		return true
	}
	return false
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
	lastTokensLen := 0
	shouldAddSemiColon := true
	for ; l.cursor < len(l.source); l.cursor++ {
		col := l.col
		line := l.line
		char := l.getCurrentChar()

		switch char {
		case '/':
			if l.peek() == '/' {
				_ = l.parseLineComment()
				//tokens = append(tokens, Token{
				//	Kind:  T_COMMENT,
				//	Value: l.parseLineComment(),
				//	Line:  line,
				//	Col:   col,
				//})
				shouldAddSemiColon = false
			}
		case '"':
			tokens = append(tokens, Token{
				Kind:  T_STRING,
				Value: l.parseString(),
				Line:  line,
				Col:   col,
			})
		case '=':
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
				Line:  line,
				Col:   col,
			})

		case '<', '>':
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
				Line:  line,
				Col:   col,
			})
		case '(':
			tokens = append(tokens, Token{
				Kind:  T_LPAREN,
				Value: string(char),
				Line:  line,
				Col:   col,
			})
		case ')':
			tokens = append(tokens, Token{
				Kind:  T_RPAREN,
				Value: string(char),

				Line: line,
				Col:  col,
			})
		case ';':
			tokens = append(tokens, Token{
				Kind:  T_SEMICOLON,
				Value: string(char),
				Line:  line,
				Col:   col,
			})
			shouldAddSemiColon = false
		case ',':
			tokens = append(tokens, Token{
				Kind:  T_COMMA,
				Value: string(char),
				Line:  line,
				Col:   col,
			})
		case '{':
			tokens = append(tokens, Token{
				Kind:  T_LBRACE,
				Value: string(char),
				Line:  line,
				Col:   col,
			})
			shouldAddSemiColon = false
		case '}':
			tokens = append(tokens, Token{
				Kind:  T_RBRACE,
				Value: string(char),
				Line:  line,
				Col:   col,
			})
			shouldAddSemiColon = false
		case '!':
			tokens = append(tokens, Token{
				Kind:  T_NOT,
				Value: string(char),
				Line:  line,
				Col:   col,
			})
		case '[':
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
				Line:  line,
				Col:   col,
			})
		case ']':
			tokens = append(tokens, Token{
				Kind:  T_RBRACK,
				Value: string(char),
				Line:  line,
				Col:   col,
			})
		case '|':
			if l.peek() == '|' {
				l.readChar()
				tokens = append(tokens, Token{
					Kind:  T_OR,
					Value: "||",
					Line:  line,
					Col:   col,
				})
			}
		case '&':
			if l.peek() == '&' {
				l.readChar()
				tokens = append(tokens, Token{
					Kind:  T_AND,
					Value: "&&",
					Line:  line,
					Col:   col,
				})
			}
		case ' ', '\t':
			l.col++
		case '\n':
			// check if we should add semicolon
			if lastTokensLen < len(tokens) && shouldAddSemiColon {
				tokens = append(tokens, Token{
					Kind:  T_SEMICOLON,
					Value: ";",
					Line:  line,
					Col:   col,
				})
			}

			shouldAddSemiColon = true
			lastTokensLen = len(tokens)

			l.line++
			l.col = 1
		default:
			if unicode.IsDigit(rune(char)) {
				tokens = append(tokens, Token{
					Kind:  T_NUMBER,
					Value: l.parseNumber(),
					Line:  line,
					Col:   col,
				})
			} else if isOp, opValue := l.parseOp(char); isOp {
				tokens = append(tokens, Token{
					Kind:  opValue,
					Value: string(char),
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

				case peekNonWSValue != '(':
					kind = T_VAR
				}

				tokens = append(tokens, Token{
					Kind:  kind,
					Value: value,
					Line:  line,
					Col:   col,
				})
			} else {
				return nil, fmt.Errorf(`unexpected char %s at col=%d line=%d`, string(char), l.col, l.line)
			}

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
		if char == ' ' || char == '\t' || char == '\r' {
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

func (l *Lexer) parseLineComment() string {
	var comment string
	l.readChar() // read /
	for l.cursor < len(l.source)-1 {
		char := l.getChar()
		if char == '\n' {
			break
		}
		comment += string(char)
	}
	return comment
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

func (l *Lexer) parseOp(char byte) (bool, string) {
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
