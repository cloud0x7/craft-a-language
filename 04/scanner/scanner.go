package scanner

import (
	"log"
	"strings"
)

type Scanner struct {
	stream   *CharStream
	tokens   []Token
	KeyWords map[string]struct{}
}

func NewScanner(stream *CharStream) *Scanner {
	t := &Scanner{
		stream:   stream,
		KeyWords: map[string]struct{}{},
	}

	kw := []string{"function", "class", "break", "delete", "return",
		"case", "do", "if", "switch", "var",
		"catch", "else", "in", "this", "void",
		"continue", "false", "instanceof", "throw", "while",
		"debugger", "finally", "new", "true", "with",
		"default", "for", "null", "try", "typeof",
		//下面这些用于严格模式
		"implements", "let", "private", "public", "yield",
		"interface", "package", "protected", "static"}

	for _, v := range kw {
		t.KeyWords[v] = struct{}{}
	}

	return t
}
func (t *Scanner) Next() Token {
	if len(t.tokens) == 0 {
		return t.getAToken()
	} else {
		tk := t.tokens[0]
		t.tokens = t.tokens[1:]
		return tk
	}
}

func (t *Scanner) Peek() Token {
	var tk Token
	if len(t.tokens) == 0 {
		tk = t.getAToken()
		t.tokens = append(t.tokens, tk)
	} else {
		tk = t.tokens[0]
	}

	return tk
}

func (t *Scanner) Peek2() Token {
	for len(t.tokens) < 2 {
		t.tokens = append(t.tokens, t.getAToken())
	}

	return t.tokens[1]
}

func (t *Scanner) getAToken() Token {
	t.skipWhiteSpaces()

	if t.stream.eof() {
		return Token{Kind: EOF, Text: ""}
	} else {
		ch := t.stream.peek()
		if t.isLetter(ch) || ch == "_" {
			return t.parseIdentifer()
		} else if ch == "\"" {
			return t.parseStringLIteral()
		} else if ch == "(" || ch == ")" || ch == "{" || ch == "}" || ch == "[" || ch == "]" ||
			ch == "," || ch == ";" || ch == ":" || ch == "?" || ch == "@" {
			t.stream.next()
			return Token{Kind: Seperator, Text: ch}
		} else if t.isDigit(ch) {
			t.stream.next()
			ch1 := t.stream.peek()
			literal := strings.Builder{}
			if ch == "0" {
				if !(ch >= "1" && ch1 <= "9") {
					literal.WriteString("0")
				} else {
					log.Printf("0 cannot be followed by other digit now, at line: %d, col: %d ", t.stream.line, t.stream.col)
					t.stream.next()
					return t.getAToken()
				}
			} else if ch >= "1" && ch <= "9" {
				literal.WriteString(ch)
				for t.isDigit(ch1) {
					ch = t.stream.next()
					literal.WriteString(ch)
					ch1 = t.stream.peek()
				}
			}
			if ch1 == "." {
				literal.WriteString(".")
				t.stream.next()
				ch1 = t.stream.peek()
				for t.isDigit(ch1) {
					ch = t.stream.next()
					literal.WriteString(ch)
					ch1 = t.stream.peek()
				}
				return Token{Kind: DecimalLiteral, Text: literal.String()}
			} else {
				return Token{Kind: IntegerLiteral, Text: literal.String()}
			}
		} else if ch == "." {
			t.stream.next()
			ch1 := t.stream.peek()
			if t.isDigit(ch1) {
				literal := "."
				for t.isDigit(ch1) {
					ch = t.stream.next()
					literal += ch
					ch1 = t.stream.peek()
				}
				return Token{Kind: DecimalLiteral, Text: literal}
			} else if ch1 == "." {
				t.stream.next()
				ch1 = t.stream.peek()
				if ch1 == "." {
					return Token{Kind: Seperator, Text: "..."}
				} else {
					log.Print("Unrecognized pattern : .., missed a . ?")
					return t.getAToken()
				}
			} else {
				return Token{Kind: Seperator, Text: "."}
			}
		} else if ch == "/" {
			t.stream.next()
			ch1 := t.stream.peek()
			if ch1 == "*" {
				t.skipMultipleLineComments()
				return t.getAToken()
			} else if ch1 == "/" {
				t.skipSingleLineComment()
				return t.getAToken()
			} else if ch1 == "=" {
				t.stream.next()
				return Token{Kind: Operator, Text: "/="}
			} else {
				return Token{Kind: Operator, Text: "/"}
			}
		} else if ch == "+" {
			t.stream.next()
			ch1 := t.stream.peek()
			if ch1 == "+" {
				t.stream.next()
				return Token{Kind: Operator, Text: "++"}
			} else if ch1 == "=" {
				t.stream.next()
				return Token{Kind: Operator, Text: "+="}
			} else {
				return Token{Kind: Operator, Text: "+"}
			}
		} else if ch == "-" {
			t.stream.next()
			ch1 := t.stream.peek()
			if ch1 == "-" {
				t.stream.next()
				return Token{Kind: Operator, Text: "--"}
			} else if ch1 == "=" {
				t.stream.next()
				return Token{Kind: Operator, Text: "-="}
			} else {
				return Token{Kind: Operator, Text: "-"}
			}
		} else if ch == "*" {
			t.stream.next()
			ch1 := t.stream.peek()
			if ch1 == "=" {
				t.stream.next()
				return Token{Kind: Operator, Text: "*="}
			} else {
				return Token{Kind: Operator, Text: "*"}
			}
		} else if ch == "%" {
			t.stream.next()
			ch1 := t.stream.peek()
			if ch1 == "=" {
				t.stream.next()
				return Token{Kind: Operator, Text: "%="}
			} else {
				return Token{Kind: Operator, Text: "%"}
			}
		} else if ch == ">" {
			t.stream.next()
			ch1 := t.stream.peek()
			if ch1 == "=" {
				t.stream.next()
				return Token{Kind: Operator, Text: ">="}
			} else if ch1 == ">" {
				t.stream.next()
				ch1 = t.stream.peek()
				if ch1 == ">" {
					t.stream.next()
					ch1 = t.stream.peek()
					if ch1 == "=" {
						t.stream.next()
						return Token{Kind: Operator, Text: ">>>="}
					} else {
						return Token{Kind: Operator, Text: ">>>"}
					}
				} else if ch1 == "=" {
					t.stream.next()
					return Token{Kind: Operator, Text: ">>="}
				} else {
					t.stream.next()
					return Token{Kind: Operator, Text: ">>"}
				}
			} else {
				return Token{Kind: Operator, Text: ">"}
			}
		} else if ch == "<" {
			t.stream.next()
			ch1 := t.stream.peek()
			if ch1 == "=" {
				t.stream.next()
				return Token{Kind: Operator, Text: "<="}
			} else if ch1 == "<" {
				t.stream.next()
				ch1 = t.stream.peek()
				if ch1 == "=" {
					t.stream.next()
					return Token{Kind: Operator, Text: "<<="}
				} else {
					return Token{Kind: Operator, Text: "<<"}
				}
			} else {
				return Token{Kind: Operator, Text: "<"}
			}
		} else if ch == "=" {
			t.stream.next()
			ch1 := t.stream.peek()
			if ch1 == "=" {
				t.stream.next()
				ch1 = t.stream.next()
				if ch1 == "=" {
					t.stream.next()
					return Token{Kind: Operator, Text: "==="}
				} else {
					return Token{Kind: Operator, Text: "=="}
				}
			} else if ch1 == ">" {
				t.stream.next()
				return Token{Kind: Operator, Text: "=>"}
			} else {
				return Token{Kind: Operator, Text: "="}
			}
		} else if ch == "!" {
			t.stream.next()
			ch1 := t.stream.peek()
			if ch1 == "=" {
				t.stream.next()
				ch1 = t.stream.peek()
				if ch1 == "=" {
					t.stream.next()
					return Token{Kind: Operator, Text: "!=="}
				} else {
					return Token{Kind: Operator, Text: "!="}
				}
			} else {
				return Token{Kind: Operator, Text: "!"}
			}
		} else if ch == "|" {
			t.stream.next()
			ch1 := t.stream.peek()
			if ch1 == "|" {
				t.stream.next()
				return Token{Kind: Operator, Text: "||"}
			} else if ch1 == "=" {
				t.stream.next()
				return Token{Kind: Operator, Text: "!="}
			} else {
				return Token{Kind: Operator, Text: "|"}
			}
		} else if ch == "&" {
			t.stream.next()
			ch1 := t.stream.peek()
			if ch1 == "&" {
				t.stream.next()
				return Token{Kind: Operator, Text: "&&"}
			} else if ch1 == "=" {
				t.stream.next()
				return Token{Kind: Operator, Text: "&="}
			} else {
				return Token{Kind: Operator, Text: "&"}
			}
		} else if ch == "^" {
			t.stream.next()
			ch1 := t.stream.peek()
			if ch1 == "=" {
				t.stream.next()
				return Token{Kind: Operator, Text: "^="}
			} else {
				return Token{Kind: Operator, Text: "^"}
			}
		} else if ch == "~" {
			t.stream.next()
			return Token{Kind: Operator, Text: ch}
		} else {
			log.Printf("Unrecognized pattern meeting ': %s, at %d col: %d\n", ch, t.stream.line, t.stream.col)
			t.stream.next()
			return t.getAToken()
		}
	}
}
func (t *Scanner) skipSingleLineComment() {
	t.stream.next()

	for t.stream.peek() != "\n" && !t.stream.eof() {
		t.stream.next()
	}
}
func (t *Scanner) skipMultipleLineComments() {
	t.stream.next()

	if !t.stream.eof() {
		ch1 := t.stream.next()
		for !t.stream.eof() {
			ch2 := t.stream.next()
			if ch1 == "*" && ch2 == "/" {
				return
			}
			ch1 = ch2
		}
	}

	log.Printf("Failed to find matching */ for multiple line comments at ': line: %d, col: %d\n", t.stream.line, t.stream.col)
}
func (t *Scanner) skipWhiteSpaces() {
	for t.isWhiteSpace(t.stream.peek()) {
		t.stream.next()
	}
}
func (t *Scanner) parseStringLIteral() Token {
	token := Token{Kind: StringLiteral, Text: ""}
	t.stream.next()

	for !t.stream.eof() && t.stream.peek() != "\"" {
		token.Text += t.stream.next()
	}

	if t.stream.peek() == "\"" {
		t.stream.next()
	} else {
		log.Printf("Expecting an \" at line: %d col: %d\n", t.stream.line, t.stream.col)
	}

	return token
}
func (t *Scanner) parseIdentifer() Token {
	token := Token{Kind: Identifier, Text: ""}
	token.Text += t.stream.next()

	for !t.stream.eof() && t.isLetterDigitOrUnderScore(t.stream.peek()) {
		token.Text += t.stream.next()
	}

	if _, ok := t.KeyWords[token.Text]; ok {
		token.Kind = Keyword
	}

	if token.Text == "function" {
		token.Kind = Keyword
	}

	return token
}
func (t *Scanner) isLetterDigitOrUnderScore(ch string) bool {
	return ch >= "A" && ch <= "Z" ||
		ch >= "a" && ch <= "z" ||
		ch >= "0" && ch <= "9" ||
		ch == "_"
}
func (t *Scanner) isLetter(ch string) bool {
	return ch >= "A" && ch <= "Z" || ch >= "a" && ch <= "z"
}
func (t *Scanner) isDigit(ch string) bool {
	return ch >= "0" && ch <= "9"
}
func (t *Scanner) isWhiteSpace(ch string) bool {
	return (ch == " " || ch == "\n" || ch == "\t")
}
