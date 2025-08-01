package parser

import (
	"craft-language/scanner"
)

// 标记编译错误的类
type CompilerError struct {
	Msg       string
	IsWarning bool              // 是告警还是错误
	BeginPos  *scanner.Position // token位置
}

func NewCompilerError(msg string, beginPos *scanner.Position, isWarning bool) *CompilerError {
	return &CompilerError{
		Msg:       msg,
		BeginPos:  beginPos,
		IsWarning: isWarning,
	}
}
