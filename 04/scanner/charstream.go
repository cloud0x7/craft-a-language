package scanner

// 字符串流
type CharStream struct {
	data string // 字符串
	pos  uint   // 指针位置
	line uint   // 行号
	col  uint   // 列号
}

func NewCharStream(data string) *CharStream {
	return &CharStream{data: data}
}

// 预读下一个字符，但不移动指针
func (cs *CharStream) peek() string {
	if int(cs.pos) < len(cs.data) {
		return string(cs.data[cs.pos])
	}
	return ""
}

// 读取下一个字符，并移动指针
func (cs *CharStream) next() string {
	ch := cs.data[cs.pos]
	cs.pos++
	if ch == '\n' {
		cs.line++
		cs.col = 0
	} else {
		cs.col++
	}
	return string(ch)
}

// 判断是否到达结尾
func (cs *CharStream) eof() bool {
	return cs.peek() == ""
}
