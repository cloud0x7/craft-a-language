package scanner

import (
	"log"
	"strings"
)

type Scanner struct {
	stream     *CharStream
	tokens     []*Token
	KeyWordMap map[string]KEYWORD
	lastPos    *Position
}

func NewScanner(stream *CharStream) *Scanner {
	t := &Scanner{
		stream:  stream,
		lastPos: NewPosition(0, 0, 0, 0),
	}
	// 从切片改为了map
	t.KeyWordMap = map[string]KEYWORD{
		"function":   Function,
		"class":      Class,
		"break":      Break,
		"delete":     Delete,
		"return":     Return,
		"case":       Case,
		"do":         Do,
		"if":         If,
		"switch":     Switch,
		"var":        Var,
		"catch":      Catch,
		"else":       Else,
		"in":         In,
		"this":       This,
		"void":       Void,
		"continue":   Continue,
		"false":      False,
		"instanceof": Instanceof,
		"throw":      Throw,
		"while":      While,
		"debugger":   Debugger,
		"finally":    Finally,
		"new":        New,
		"true":       True,
		"with":       With,
		"default":    Default,
		"for":        For,
		"null":       Null,
		"try":        Try,
		"typeof":     Typeof,
		//下面这些用于严格模式
		"implements": Implements,
		"let":        Let,
		"private":    Private,
		"public":     Public,
		"yield":      Yield,
		"interface":  Interface,
		"package":    Package,
		"protected":  Protected,
		"static":     Static,
		// //类型
		// // "number":  Number,
		// "string":  String,
		// "boolean": Boolean,
		// "any":     Any,
		// "symbol":  Symbol,
		// //值
		// "undefined": Undefined,
	}

	return t
}
func (t *Scanner) Next() *Token {
	var val *Token
	if len(t.tokens) == 0 {
		val = t.getAToken()
	} else {
		val = t.tokens[0]
		t.tokens = t.tokens[1:]
	}
	t.lastPos = val.Pos
	return val
}

func (t *Scanner) Peek() *Token {
	var tk *Token
	if len(t.tokens) == 0 {
		tk = t.getAToken()
		t.tokens = append(t.tokens, tk)
	} else {
		tk = t.tokens[0]
	}

	return tk
}

func (t *Scanner) Peek2() *Token {
	for len(t.tokens) < 2 {
		t.tokens = append(t.tokens, t.getAToken())
	}

	return t.tokens[1]
}

func (t *Scanner) GetNextPos() *Position {
	p := t.Peek().Pos
	return p
}
func (t *Scanner) GetLastPos() *Position {
	return t.lastPos
}

func (t *Scanner) getAToken() *Token {
	t.skipWhiteSpaces()
	pos := t.stream.getPosition()

	if t.stream.eof() {
		return NewToken(EOF, "EOF", pos, nil)
	} else {
		ch := t.stream.peek()
		if t.isLetter(ch) || ch == "_" {
			return t.parseIdentifer()
		} else if ch == "\"" {
			return t.parseStringLIteral()
		} else if ch == "(" {
			t.stream.next()
			return NewToken(Seperator, ch, pos, OpenParen)
		} else if ch == ")" {
			t.stream.next()
			return NewToken(Seperator, ch, pos, CloseParen)
		} else if ch == "{" {
			t.stream.next()
			return NewToken(Seperator, ch, pos, OpenBrace)
		} else if ch == "}" {
			t.stream.next()
			return NewToken(Seperator, ch, pos, CloseBrace)
		} else if ch == "[" {
			t.stream.next()
			return NewToken(Seperator, ch, pos, OpenBracket)
		} else if ch == "]" {
			t.stream.next()
			return NewToken(Seperator, ch, pos, CloseBracket)
		} else if ch == ":" {
			t.stream.next()
			return NewToken(Seperator, ch, pos, Colon)
		} else if ch == ";" {
			t.stream.next()
			return NewToken(Seperator, ch, pos, SemiColon)
		} else if ch == "," {
			t.stream.next()
			return NewToken(Seperator, ch, pos, Comma)
		} else if ch == "?" {
			t.stream.next()
			return NewToken(Seperator, ch, pos, QuestionMark)
		} else if ch == "@" {
			t.stream.next()
			return NewToken(Seperator, ch, pos, At)
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
				return NewToken(DecimalLiteral, literal.String(), pos, nil)
			} else {
				return NewToken(IntegerLiteral, literal.String(), pos, nil)
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
				pos.End = t.stream.pos + 1
				return NewToken(DecimalLiteral, literal, pos, nil)
			} else if ch1 == "." {
				t.stream.next()
				ch1 = t.stream.peek()
				if ch1 == "." {
					pos.End = t.stream.pos + 1
					return NewToken(Seperator, "...", pos, Ellipsis)
				} else {
					log.Print("Unrecognized pattern : .., missed a . ?")
					return t.getAToken()
				}
			} else {
				return NewToken(Operator, ".", pos, Dot)
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
				pos.End = t.stream.pos + 1
				return NewToken(Operator, "/=", pos, DivideAssign)
			} else {
				return NewToken(Operator, "/", pos, Divide)
			}
		} else if ch == "+" {
			t.stream.next()
			ch1 := t.stream.peek()
			if ch1 == "+" {
				t.stream.next()
				return NewToken(Operator, "++", pos, Inc)
			} else if ch1 == "=" {
				t.stream.next()
				return NewToken(Operator, "+=", pos, PlusAssign)
			} else {
				return NewToken(Operator, "+", pos, Plus)
			}
		} else if ch == "-" {
			t.stream.next()
			ch1 := t.stream.peek()
			if ch1 == "-" {
				t.stream.next()
				pos.End = t.stream.pos + 1
				return NewToken(Operator, "--", pos, Dec)
			} else if ch1 == "=" {
				t.stream.next()
				return NewToken(Operator, "-=", pos, MinusAssign)
			} else {
				return NewToken(Operator, "-", pos, Minus)
			}
		} else if ch == "*" {
			t.stream.next()
			ch1 := t.stream.peek()
			if ch1 == "=" {
				t.stream.next()
				pos.End = t.stream.pos + 1
				return NewToken(Operator, "*=", pos, MultiplyAssign)
			} else {
				return NewToken(Operator, "*", pos, Multiply)
			}
		} else if ch == "%" {
			t.stream.next()
			ch1 := t.stream.peek()
			if ch1 == "=" {
				t.stream.next()
				pos.End = t.stream.pos + 1
				return NewToken(Operator, "%=", pos, ModulusAssign)
			} else {
				return NewToken(Operator, "%", pos, Modulus)
			}
		} else if ch == ">" {
			t.stream.next()
			ch1 := t.stream.peek()
			if ch1 == "=" {
				t.stream.next()
				pos.End = t.stream.pos + 1
				return NewToken(Operator, ">=", pos, GE)
			} else if ch1 == ">" {
				t.stream.next()
				ch1 = t.stream.peek()
				if ch1 == ">" {
					t.stream.next()
					ch1 = t.stream.peek()
					if ch1 == "=" {
						t.stream.next()
						pos.End = t.stream.pos + 1
						return NewToken(Operator, ">>>=", pos, RightShiftLogicalAssign)
					} else {
						return NewToken(Operator, ">>>", pos, RightShiftLogical)
					}
				} else if ch1 == "=" {
					t.stream.next()
					pos.End = t.stream.pos + 1
					return NewToken(Operator, ">>=", pos, RightShiftArithmeticAssign)
				} else {
					pos.End = t.stream.pos + 1
					return NewToken(Operator, ">>", pos, RightShiftArithmetic)
				}
			} else {
				return NewToken(Operator, ">", pos, G)
			}
		} else if ch == "<" {
			t.stream.next()
			ch1 := t.stream.peek()
			if ch1 == "=" {
				t.stream.next()
				pos.End = t.stream.pos + 1
				return NewToken(Operator, "<=", pos, LE)
			} else if ch1 == "<" {
				t.stream.next()
				ch1 = t.stream.peek()
				if ch1 == "=" {
					t.stream.next()
					pos.End = t.stream.pos + 1
					return NewToken(Operator, "<<=", pos, LeftShiftArithmeticAssign)
				} else {
					pos.End = t.stream.pos + 1
					return NewToken(Operator, "<<", pos, LeftShiftArithmetic)
				}
			} else {
				return NewToken(Operator, "<", pos, L)
			}
		} else if ch == "=" {
			t.stream.next()
			ch1 := t.stream.peek()
			if ch1 == "=" {
				t.stream.next()
				ch1 = t.stream.next()
				if ch1 == "=" {
					t.stream.next()
					pos.End = t.stream.pos + 1
					return NewToken(Operator, "===", pos, IdentityEquals)
				} else {
					pos.End = t.stream.pos + 1
					return NewToken(Operator, "==", pos, EQ)
				}
			} else if ch1 == ">" {
				t.stream.next()
				pos.End = t.stream.pos + 1
				return NewToken(Operator, "=>", pos, ARROW)
			} else {
				return NewToken(Operator, "=", pos, Assign)
			}
		} else if ch == "!" {
			t.stream.next()
			ch1 := t.stream.peek()
			if ch1 == "=" {
				t.stream.next()
				ch1 = t.stream.peek()
				if ch1 == "=" {
					t.stream.next()
					pos.End = t.stream.pos + 1
					return NewToken(Operator, "!==", pos, IdentityNotEquals)
				} else {
					pos.End = t.stream.pos + 1
					return NewToken(Operator, "!=", pos, NE)
				}
			} else {
				return NewToken(Operator, "!", pos, Not)
			}
		} else if ch == "|" {
			t.stream.next()
			ch1 := t.stream.peek()
			if ch1 == "|" {
				pos.End = t.stream.pos + 1
				return NewToken(Operator, "||", pos, Or)
			} else if ch1 == "=" {
				t.stream.next()
				pos.End = t.stream.pos + 1
				return NewToken(Operator, "|=", pos, BitOrAssign)
			} else {
				return NewToken(Operator, "|", pos, BitOr)
			}
		} else if ch == "&" {
			t.stream.next()
			ch1 := t.stream.peek()
			if ch1 == "&" {
				t.stream.next()
				pos.End = t.stream.pos + 1
				return NewToken(Operator, "&&", pos, And)
			} else if ch1 == "=" {
				t.stream.next()
				pos.End = t.stream.pos + 1
				return NewToken(Operator, "&=", pos, BitAndAssign)
			} else {
				return NewToken(Operator, "&", pos, BitAnd)
			}
		} else if ch == "^" {
			t.stream.next()
			ch1 := t.stream.peek()
			if ch1 == "=" {
				t.stream.next()
				pos.End = t.stream.pos + 1
				return NewToken(Operator, "^=", pos, BitXorAssign)
			} else {
				return NewToken(Operator, "^", pos, BitXOr)
			}
		} else if ch == "~" {
			t.stream.next()
			return NewToken(Operator, "~", pos, BitNot)
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
func (t *Scanner) parseStringLIteral() *Token {
	pos := t.stream.getPosition()
	token := NewToken(StringLiteral, "", pos, nil)
	t.stream.next()

	for !t.stream.eof() && t.stream.peek() != "\"" {
		token.Text += t.stream.next()
	}

	if t.stream.peek() == "\"" {
		t.stream.next()
	} else {
		log.Printf("Expecting an \" at line: %d col: %d\n", t.stream.line, t.stream.col)
	}

	pos.End = t.stream.pos + 1
	return token
}
func (t *Scanner) parseIdentifer() *Token {
	pos := t.stream.getPosition()
	token := NewToken(Identifier, "", pos, nil)
	token.Text += t.stream.next()

	for !t.stream.eof() && t.isLetterDigitOrUnderScore(t.stream.peek()) {
		token.Text += t.stream.next()
	}

	pos.End = t.stream.pos + 1
	if _, ok := t.KeyWordMap[token.Text]; ok {
		token.Kind = Keyword
		token.Code = t.KeyWordMap[token.Text]
	}

	return token
}
func (t Scanner) isLetterDigitOrUnderScore(ch string) bool {
	return ch >= "A" && ch <= "Z" ||
		ch >= "a" && ch <= "z" ||
		ch >= "0" && ch <= "9" ||
		ch == "_"
}
func (t Scanner) isLetter(ch string) bool {
	return ch >= "A" && ch <= "Z" || ch >= "a" && ch <= "z"
}
func (t Scanner) isDigit(ch string) bool {
	return ch >= "0" && ch <= "9"
}
func (t Scanner) isWhiteSpace(ch string) bool {
	return (ch == " " || ch == "\n" || ch == "\t")
}

func IsAssignOp(op OP) bool {
	return op >= Assign && op <= BitOrAssign
}
func IsRelationOp(op OP) bool {
	return op >= EQ && op <= LE
}
func IsArithmeticOp(op OP) bool {
	return op >= Plus && op <= Modulus
}
func IsLogicalOp(op OP) bool {
	return op >= Not && op <= Or
}

type SEPERATOR int

const (
	OpenBracket  SEPERATOR = iota
	CloseBracket           //]
	OpenParen              //(
	CloseParen             //)
	OpenBrace              //{
	CloseBrace             //}
	Colon                  //:
	SemiColon
)

type OP int

const (
	QuestionMark OP = iota + 100
	Ellipsis        //...
	Dot             //.
	Comma           //,
	At              //@

	RightShiftArithmetic //>>
	LeftShiftArithmetic  //<<
	RightShiftLogical    //>>>
	IdentityEquals       //===
	IdentityNotEquals    //!==

	BitNot //~
	BitAnd //&
	BitXOr //^
	BitOr  //|

	Not //!
	And //&&
	Or  //||

	Assign                     //=
	MultiplyAssign             //*=
	DivideAssign               ///=
	ModulusAssign              //%=
	PlusAssign                 //+=
	MinusAssign                //-=
	LeftShiftArithmeticAssign  //<<=
	RightShiftArithmeticAssign //>>=
	RightShiftLogicalAssign    //>>>=
	BitAndAssign               //&=
	BitXorAssign               //^=
	BitOrAssign                //|=

	ARROW //=>

	Inc //++
	Dec //--

	Plus     //+
	Minus    //-
	Multiply //*
	Divide   ///
	Modulus  //%

	EQ //==
	NE //!=
	G  //>
	GE //>=
	L  //<
	LE //<=
)

func (o OP) ToString() string {
	arr := []string{
		"QuestionMark",
		"Ellipsis",
		"Dot",
		"Comma",
		"At",
		"RightShiftArithmetic",
		"LeftShiftArithmetic",
		"RightShiftLogical",
		"IdentityEquals",
		"IdentityNotEquals",
		"BitNot",
		"BitAnd",
		"BitXOr",
		"BitOr",
		"Not",
		"And",
		"Or",
		"Assign",
		"MultiplyAssign",
		"DivideAssign",
		"ModulusAssign",
		"PlusAssign",
		"MinusAssign",
		"LeftShiftArithmeticAssign",
		"RightShiftArithmeticAssign",
		"RightShiftLogicalAssign",
		"BitAndAssign",
		"BitXorAssign",
		"BitOrAssign",
		"ARROW",
		"Inc",
		"Dec",
		"Plus",
		"Minus",
		"Multiply",
		"Divide",
		"Modulus",
		"EQ",
		"NE",
		"G",
		"GE",
		"L",
		"LE",
	}
	return arr[o-100]
}

type KEYWORD int

const (
	Function KEYWORD = iota + 200
	Class
	Break
	Delete
	Return
	Case
	Do
	If
	Switch
	Var
	Catch
	Else
	In
	This
	Void
	Continue
	False
	Instanceof
	Throw
	While
	Debugger
	Finally
	New
	True
	With
	Default
	For
	Null
	Try
	Typeof
	//下面这些用于严格模式
	Implements
	Let
	Private
	Public
	Yield
	Interface
	Package
	Protected
	Static
	//more
	Any
	String
	Number
	Boolean
	Symbol
	//值
	Undefined
)
