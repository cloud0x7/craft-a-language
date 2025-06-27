package scanner

type CharStream struct {
	data string
	pos  uint
	line uint
	col  uint
}

func NewCharStream(data string) *CharStream {
	return &CharStream{data: data, line: 1, col: 1}
}
func (cs CharStream) peek() string {
	if int(cs.pos) < len(cs.data) {
		return string(cs.data[cs.pos])
	}
	return ""
}
func (cs *CharStream) next() string {
	ch := cs.data[cs.pos]
	cs.pos++
	if ch == '\n' {
		cs.line++
		cs.col = 1
	} else {
		cs.col++
	}
	return string(ch)
}
func (cs CharStream) eof() bool {
	return cs.peek() == ""
}
func (cs CharStream) getPosition() *Position {
	return NewPosition(cs.pos+1, cs.pos+1, cs.line, cs.col)
}
