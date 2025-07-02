package scanner

import "fmt"

type TokenKind int

const (
	Keyword TokenKind = iota
	Identifier
	StringLiteral
	IntegerLiteral
	DecimalLiteral
	NullLiteral
	BooleanLiteral
	Seperator
	Operator
	EOF
)

var TokenKindToString = map[TokenKind]string{
	Keyword:        "Keyword",
	Identifier:     "Identifier",
	StringLiteral:  "StringLiteral",
	IntegerLiteral: "IntegerLiteral",
	DecimalLiteral: "DecimalLiteral",
	NullLiteral:    "NullLiteral",
	BooleanLiteral: "BooleanLiteral",
	Seperator:      "Seperator",
	Operator:       "Operator",
	EOF:            "EOF",
}

type Position struct {
	Begin uint
	End   uint
	Line  uint
	Col   uint
}

func NewPosition(begin, end, line, col uint) *Position {
	return &Position{Begin: begin, End: end, Line: line, Col: col}
}
func (p Position) ToString() string {
	return fmt.Sprintf("(ln: %d, col: %d, pos: %d)", p.Line, p.Col, p.Begin)
}

type Token struct {
	Kind TokenKind
	Text string
	Pos  *Position
	Code any
}

func NewToken(kind TokenKind, text string, pos *Position, code any) *Token {
	return &Token{Kind: kind, Text: text, Pos: pos, Code: code}
}
func (tk Token) ToString() string {
	return fmt.Sprintf("Token@%-33s %-15s '%s'", tk.Pos.ToString(), TokenKindToString[tk.Kind], tk.Text)
}
