package scanner

import (
	"log"
)

// 词法分析器
type Scanner struct {
	stream   *CharStream
	tokens   []Token             // 存储token的列表
	KeyWords map[string]struct{} // 关键字
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
		t.KeyWords[v] = struct{}{} // 添加空结构体，模拟集合set
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

// 预读前面第二个token
func (t *Scanner) Peek2() Token {
	for len(t.tokens) < 2 {
		t.tokens = append(t.tokens, t.getAToken())
	}

	return t.tokens[1]
}

// 从字符串流中获取一个新Token
func (t *Scanner) getAToken() Token {
	t.skipWhiteSpaces() // 跳过空白字符

	if t.stream.eof() { // 到达字符流结尾
		return Token{Kind: EOF, Text: ""}
	} else {
		ch := t.stream.peek()
		if t.isLetter(ch) || ch == "_" { // 是字母或数字，则解析标识符
			return t.parseIdentifer()
		} else if ch == "\"" { // 是双引号，则解析字符串字面量
			return t.parseStringLIteral()
		} else if ch == "(" || ch == ")" || ch == "{" || ch == "}" || ch == "[" || ch == "]" ||
			ch == "," || ch == ";" || ch == ":" || ch == "?" || ch == "@" {
			t.stream.next()
			return Token{Kind: Seperator, Text: ch}
		} else if t.isDigit(ch) {
			t.stream.next()
			ch1 := t.stream.peek()
			literal := ""
			if ch == "0" { //暂不支持八进制、二进制、十六进制
				if !(ch >= "1" && ch1 <= "9") {
					literal += "0"
				} else {
					log.Printf("0 cannot be followed by other digit now, at line: %d, col: %d ", t.stream.line, t.stream.col)
					t.stream.next()
					return t.getAToken()
				}
			} else if ch >= "1" && ch <= "9" {
				literal += ch
				for t.isDigit(ch1) {
					ch = t.stream.next()
					literal += ch
					ch1 = t.stream.peek()
				}
			}
			if ch1 == "." { // 小数点
				literal += "."
				t.stream.next()
				ch1 = t.stream.peek()
				for t.isDigit(ch1) {
					ch = t.stream.next()
					literal += ch
					ch1 = t.stream.peek()
				}
				return Token{Kind: DecimalLiteral, Text: literal}
			} else {
				return Token{Kind: IntegerLiteral, Text: literal}
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

// 解析标识符。从标识符中还要挑出关键字
func (t *Scanner) parseIdentifer() Token {
	token := Token{Kind: Identifier, Text: ""}
	// 第一个字符不用判断，因为在调用者那里已经判断过了
	token.Text += t.stream.next()

	for !t.stream.eof() && t.isLetterDigitOrUnderScore(t.stream.peek()) {
		token.Text += t.stream.next()
	}
	// 识别关键字
	if _, ok := t.KeyWords[token.Text]; ok {
		token.Kind = Keyword
	} else if token.Text == "null" {
		token.Kind = NullLiteral
	} else if token.Text == "true" || token.Text == "false" {
		token.Kind = BooleanLiteral
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
