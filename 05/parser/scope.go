package parser

import "fmt"

// 作用域，用来限定标识符的可见性
type Scope struct {
	Name2sym       map[string]ISymbol // 存储符号，以名称为key
	enclosingScope *Scope             // 父作用域
}

func NewScope(enclosingScope *Scope) *Scope {
	r := &Scope{
		Name2sym:       make(map[string]ISymbol),
		enclosingScope: enclosingScope,
	}
	return r
}

// 把符号记入符号表（作用域）
func (s *Scope) Enter(name string, sym ISymbol) {
	s.Name2sym[name] = sym
}

// 查询是否有某名称的符号
func (s *Scope) HasSymbol(name string) bool {
	_, ok := s.Name2sym[name]
	return ok
}

// 根据名称查找符号
func (s *Scope) GetSymbol(name string) ISymbol {
	if s.HasSymbol(name) {
		return s.Name2sym[name]
	}
	return nil
}

// 级联查找某个符号，本作用域查询不到，则去上一级作用域查
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

// 打印Scope信息
type ScopeDumper struct {
	AstVisitor
}

func NewScopeDumper() *ScopeDumper {
	r := &ScopeDumper{AstVisitor{}}
	r.AstVisitor.Curr = r
	return r
}

// 访问函数声明节点
func (s *ScopeDumper) VisitFunctionDecl(functionDecl *FunctionDecl, additional any) any {
	fmt.Printf("%sScope of function: %s\n", additional, functionDecl.Name)
	if functionDecl.scope != nil { // 显示本级作用域
		s.dumpScope(functionDecl.scope, additional)
	} else {
		fmt.Printf("%s{null}\n", additional)
	}
	// 继续遍历
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
