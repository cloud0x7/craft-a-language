package scanner

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

type Token struct {
	Kind TokenKind
	Text string
}
