package semantic

import (
	"craft-language/ast"
	"log"
)

// 符号类型
type SymKind int

const (
	Variable SymKind = iota
	Function
	Class
	Interface
)

type Symbol struct {
	name string
	decl ast.IDecl
	kind SymKind
}

func NewSymbol(name string, decl ast.IDecl, kind SymKind) *Symbol {
	return &Symbol{
		name: name,
		decl: decl,
		kind: kind,
	}
}

type SymTable struct {
	table map[string]*Symbol
}

func NewSymTable() *SymTable {
	return &SymTable{table: make(map[string]*Symbol)}
}
func (s *SymTable) Enter(name string, decl ast.IDecl, symType SymKind) {
	s.table[name] = NewSymbol(name, decl, symType)
}
func (s SymTable) HasSymbol(name string) bool {
	_, ok := s.table[name]
	return ok
}
func (s SymTable) GetSymbol(name string) (rt *Symbol, bl bool) {
	item, ok := s.table[name]
	if ok {
		return item, true
	}
	return
}

type Enter struct {
	ast.AstVisitor
	symTable *SymTable
}

func NewEnter(symTable *SymTable) *Enter {
	return &Enter{symTable: symTable}
}

func (e *Enter) Visit(node ast.AstNode) any {
	return node.Accept(e)
}
func (e *Enter) VisitProg(prog *ast.Prog) any {
	var retVal interface{}
	for _, x := range prog.Stmts {
		retVal = e.Visit(x)
	}
	return retVal
}

func (e *Enter) VisitVariableDecl(variableDecl *ast.VariableDecl) any {
	if e.symTable.HasSymbol(variableDecl.Name) {
		log.Print("Dumplicate symbol: " + variableDecl.Name)
	}
	e.symTable.Enter(variableDecl.Name, variableDecl, Variable)
	return nil
}
func (e *Enter) VisitFunctionDecl(functionDecl *ast.FunctionDecl) any {
	if e.symTable.HasSymbol(functionDecl.Name) {
		log.Print("Dumplicate symbol: " + functionDecl.Name)
	}
	e.symTable.Enter(functionDecl.Name, functionDecl, Function)
	return nil
}

type RefResolver struct {
	ast.AstVisitor
	symTable *SymTable
}

func NewRefResolver(symTable *SymTable) *RefResolver {
	return &RefResolver{symTable: symTable}
}
func (r *RefResolver) Visit(node ast.AstNode) any {
	// fmt.Printf("-node: %T, %v\n", node, node)
	return node.Accept(r)
}
func (r *RefResolver) VisitProg(prog *ast.Prog) any {
	var retVal interface{}
	for _, x := range prog.Stmts {
		// fmt.Printf("--stmt: %T, %+v\n", x, x)
		retVal = r.Visit(x)
	}
	return retVal
}
func (r *RefResolver) VisitFunctionCall(functionCall *ast.FunctionCall) any {
	symbol, ok := r.symTable.GetSymbol(functionCall.Name)
	if ok && symbol.kind == Function {
		functionCall.Decl = symbol.decl.(*ast.FunctionDecl)
	} else {
		if functionCall.Name != "println" {
			log.Print("Error: cannot find declaration of function " + functionCall.Name)
		}
	}
	return nil
}

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
	// fmt.Printf("--- exp: %T, %+v\n", stmt.Exp, stmt.Exp)
	return r.Visit(stmt.Exp)
}
