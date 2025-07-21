package semantic

import (
	"craft-language/ast"
	"log"
)

// 语义分析功能

// 符号类型
type SymKind int

const (
	Variable SymKind = iota
	Function
	Class
	Interface
)

// 符号表，保存变量、函数、类等的名称和它的类型、声明的位置（AST节点）
type SymTable struct {
	table map[string]*Symbol
}

func NewSymTable() *SymTable {
	return &SymTable{table: make(map[string]*Symbol)}
}

// 加入符号
func (s *SymTable) Enter(name string, decl ast.IDecl, symType SymKind) {
	s.table[name] = NewSymbol(name, decl, symType)
}

// 查询符号表中是否存在
func (s SymTable) HasSymbol(name string) bool {
	_, ok := s.table[name]
	return ok
}

// 获取符号
func (s SymTable) GetSymbol(name string) (rt *Symbol, bl bool) {
	item, ok := s.table[name]
	if ok {
		return item, true
	}
	return
}

// 符号表条目
type Symbol struct {
	name string
	decl ast.IDecl
	kind SymKind // 类型
}

func NewSymbol(name string, decl ast.IDecl, kind SymKind) *Symbol {
	return &Symbol{
		name: name,
		decl: decl,
		kind: kind,
	}
}

// ///////////////////////////////////////////////////////////////////////
// 建立符号表
type Enter struct {
	ast.AstVisitor // 组合访问者的所有方法
	symTable       *SymTable
}

func NewEnter(symTable *SymTable) *Enter {
	return &Enter{symTable: symTable}
}

func (e *Enter) Visit(node ast.AstNode) any {
	return node.Accept(e)
}
func (e *Enter) VisitProg(prog *ast.Prog) any {
	var retVal any
	for _, x := range prog.Stmts {
		retVal = e.Visit(x)
	}
	return retVal
}

// 把变量声明加入符号表
func (e *Enter) VisitVariableDecl(variableDecl *ast.VariableDecl) any {
	if e.symTable.HasSymbol(variableDecl.Name) {
		log.Print("Dumplicate symbol: " + variableDecl.Name)
	}
	e.symTable.Enter(variableDecl.Name, variableDecl, Variable)
	return nil
}

// 把函数声明加入符号表
func (e *Enter) VisitFunctionDecl(functionDecl *ast.FunctionDecl) any {
	if e.symTable.HasSymbol(functionDecl.Name) {
		log.Print("Dumplicate symbol: " + functionDecl.Name)
	}
	e.symTable.Enter(functionDecl.Name, functionDecl, Function)
	return nil
}

// ///////////////////////////////////////////////////////////////////////
// 引用消解, 遍历AST，如果发现函数调用和变量引用，就去找它的定义
type RefResolver struct {
	ast.AstVisitor
	symTable *SymTable // 符号表
}

func NewRefResolver(symTable *SymTable) *RefResolver {
	return &RefResolver{symTable: symTable}
}
func (r *RefResolver) Visit(node ast.AstNode) any {
	return node.Accept(r)
}
func (r *RefResolver) VisitProg(prog *ast.Prog) any {
	var retVal any
	for _, x := range prog.Stmts {
		retVal = r.Visit(x)
	}
	return retVal
}

// 消解函数引用
func (r *RefResolver) VisitFunctionCall(functionCall *ast.FunctionCall) any {
	symbol, ok := r.symTable.GetSymbol(functionCall.Name)
	if ok && symbol.kind == Function {
		functionCall.Decl = symbol.decl.(*ast.FunctionDecl)
	} else {
		if functionCall.Name != "println" { //系统内置函数不用报错
			log.Print("Error: cannot find declaration of function " + functionCall.Name)
		}
	}
	return nil
}

// 消解变量引用
func (r *RefResolver) VisitVariable(variable *ast.Variable) any {
	symbol, ok := r.symTable.GetSymbol(variable.Name)
	if ok && symbol.kind == Variable {
		variable.Decl = symbol.decl.(*ast.VariableDecl)
	} else {
		log.Print("Error: cannot find declaration of variable " + variable.Name)
	}
	return nil
}
func (r *RefResolver) VisitBinary(exp *ast.Binary) any {
	r.Visit(exp.Exp1)
	r.Visit(exp.Exp2)
	return 0
}
func (r *RefResolver) VisitExpressionStatement(stmt *ast.ExpressionStatement) any {
	return r.Visit(stmt.Exp)
}
