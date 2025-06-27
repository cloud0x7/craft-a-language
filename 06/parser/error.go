package parser

import (
	"craft-language/scanner"
)

type CompilerError struct {
	Msg       string
	IsWarning bool
	BeginPos  *scanner.Position
}

func NewCompilerError(msg string, beginPos *scanner.Position, isWarning bool) *CompilerError {
	return &CompilerError{
		Msg:       msg,
		BeginPos:  beginPos,
		IsWarning: isWarning,
	}
}
