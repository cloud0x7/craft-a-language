package parser

import "fmt"

type Scope struct {
	Name2sym       map[string]ISymbol
	enclosingScope *Scope
}

func NewScope(enclosingScope *Scope) *Scope {
	r := &Scope{
		Name2sym:       make(map[string]ISymbol),
		enclosingScope: enclosingScope,
	}
	return r
}

func (s *Scope) Enter(name string, sym ISymbol) {
	s.Name2sym[name] = sym
}

func (s *Scope) HasSymbol(name string) bool {
	_, ok := s.Name2sym[name]
	return ok
}
func (s *Scope) GetSymbol(name string) ISymbol {
	if s.HasSymbol(name) {
		return s.Name2sym[name]
	}
	return nil
}
func (s *Scope) GetSymbolCascade(name string) ISymbol {
	sym := s.GetSymbol(name)
	if sym != nil {
		return sym
	} else if s.enclosingScope != nil {
		return s.enclosingScope.GetSymbolCascade(name)
	} else {
		return nil
	}
}

type ScopeDumper struct {
	AstVisitor
}

func NewScopeDumper() *ScopeDumper {
	r := &ScopeDumper{AstVisitor{}}
	r.AstVisitor.Curr = r
	return r
}

// func (s *ScopeDumper) Visit(node IAstNode, additional any) any {
// 	return node.Accept(s, additional)
// }

func (s *ScopeDumper) VisitFunctionDecl(functionDecl *FunctionDecl, additional any) any {
	fmt.Printf("%sScope of function: %s\n", additional, functionDecl.Name)
	if functionDecl.scope != nil {
		s.dumpScope(functionDecl.scope, additional)
	} else {
		fmt.Printf("%s{null}\n", additional)
	}

	return s.AstVisitor.VisitFunctionDecl(functionDecl, additional.(string)+"   ")
}

func (s *ScopeDumper) VisitBlock(block *Block, additional any) any {
	fmt.Printf("%sScope of block:\n", additional)

	if block.scope != nil {
		s.dumpScope(block.scope, additional)
	} else {
		fmt.Printf("%s{null}", additional)
	}

	return s.AstVisitor.VisitBlock(block, additional.(string)+"   ")
}
func (s *ScopeDumper) VisitForStatement(stmt *ForStatement, additional any) any {
	fmt.Printf("%sScope of for statement:\n", additional)
	if stmt.Scope != nil {
		s.dumpScope(stmt.Scope, additional)
	} else {
		fmt.Printf("%s{null}", additional)
	}
	return nil
}
func (s *ScopeDumper) dumpScope(scope *Scope, additional any) {
	if len(scope.Name2sym) > 0 {
		symbolDumper := SymbolDumper{}
		for _, sym := range scope.Name2sym {
			symbolDumper.Visit(sym, fmt.Sprintf("%s  ", additional))
		}
	} else {
		fmt.Printf("%s    {empty}\n", additional)
	}
}
