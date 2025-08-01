package parser

import "fmt"

type Symkind int

const (
	VariableKind Symkind = iota // 这里换个名字，避免冲突
	Function
	Class
	Interface
	Parameter
	ProgKind // 这里换个名字，避免冲突
)

var SymkindMap = []string{"Variable", "Function", "Class", "Interface", "Parameter", "Prog"}

func (s Symkind) ToString() string {
	return SymkindMap[s]
}

// ///////////////////////////////////////////////////////////////////////
// 符号表
type ISymbolAttr interface {
	GetKind() Symkind
}
type ISymbol interface {
	Accept(visit ISymbolVisitor, additional any) any
	ISymbolAttr
}

type ISymbolVisitor interface {
	VisitVarSymbol(sym *VarSymbol, additional any) any
	VisitFunctionSymbol(sym *FunctionSymbol, additional any) any
}

type Symbol struct {
	Name    string
	TheType IType
	kind    Symkind
}

func (s *Symbol) Accept(visit ISymbolVisitor, additional any) any {
	return nil
}
func (s *Symbol) GetKind() Symkind {
	return s.kind
}

func NewSymbol(name string, theType IType, kind Symkind) *Symbol {
	return &Symbol{
		Name:    name,
		TheType: theType,
		kind:    kind,
	}
}

// 函数符号
type FunctionSymbol struct {
	Symbol
	Vars        []VarSymbol   // 本地变量和参数列表
	opStackSize int           // 操作数栈大小
	byteCode    []byte        // 存放字节码，当前版本没用到
	Decl        *FunctionDecl // 存放AST
}

func NewFunctionSymbol(name string, theType IType, vars []VarSymbol) *FunctionSymbol {
	return &FunctionSymbol{
		Symbol:      Symbol{Name: name, TheType: theType, kind: Function},
		Vars:        vars,
		opStackSize: 0,
	}
}
func (f *FunctionSymbol) Accept(visitor ISymbolVisitor, additional any) any {
	return visitor.VisitFunctionSymbol(f, additional)
}

// 获取参数数量
func (f *FunctionSymbol) GetNumParams() int {
	return len(f.TheType.(*FunctionType).ParamTypes)
}

// 变量符号
type VarSymbol struct {
	Symbol
}

func NewVarSymbol(name string, theType IType) *VarSymbol {
	return &VarSymbol{
		Symbol: Symbol{Name: name, TheType: theType},
	}
}

func (v *VarSymbol) Accept(visitor ISymbolVisitor, additional any) any {
	return visitor.VisitVarSymbol(v, additional)
}

type SymbolVisitor struct{}

func (s *SymbolVisitor) VisitVarSymbol(symbol *VarSymbol, additional any) any {
	return nil
}
func (s *SymbolVisitor) VisitFunctionSymbol(symbol *FunctionSymbol, additional any) any {
	return nil
}

type SymbolDumper struct {
	SymbolVisitor
}

func (s *SymbolDumper) Visit(sym ISymbol, additional any) any {
	return sym.Accept(s, additional)
}

func (s *SymbolDumper) VisitVarSymbol(sym *VarSymbol, additional any) any {
	fmt.Printf("%s%s{%s}\n", additional, sym.Name, sym.kind.ToString())
	return nil
}

func (s *SymbolDumper) VisitFunctionSymbol(sym *FunctionSymbol, additional any) any {
	fmt.Printf("%s%s{%s, local var count:%d}\n", additional, sym.Name, sym.kind.ToString(), len(sym.Vars))
	return nil
}

// /////////////////////////////////////////////////////////////////////
// 系统内置的符号
var FUN_println = NewFunctionSymbol("println", NewFunctionType(Void, []IType{String}, ""), []VarSymbol{*NewVarSymbol("a", String)})
var FUN_tick = NewFunctionSymbol("tick", NewFunctionType(Integer, []IType{}, ""), []VarSymbol{})
var FUN_integer_to_string = NewFunctionSymbol("integer_to_string", NewFunctionType(String, []IType{Integer}, ""), []VarSymbol{*NewVarSymbol("a", Integer)})

var Built_ins = map[string]*FunctionSymbol{
	"println":           FUN_println,
	"tick":              FUN_tick,
	"integer_to_string": FUN_integer_to_string,
}

var FUN_string_create_by_str = NewFunctionSymbol("string_create_by_str", NewFunctionType(String, []IType{String}, ""), []VarSymbol{*NewVarSymbol("a", String)})
var FUN_string_concat = NewFunctionSymbol("string_concat", NewFunctionType(String, []IType{String, String}, ""), []VarSymbol{*NewVarSymbol("str1", String), *NewVarSymbol("str2", String)})
