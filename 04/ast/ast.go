package ast

import (
	"fmt"
	"strconv"
)

// 语法分析器
type AstNode interface {
	Dump(prefix string)
	Accept(visitor IAstVisitor) any
}

type Statement interface {
	AstNode
}
type Expression interface {
	AstNode
}

type IDecl interface {
	AstNode
}
type Decl struct {
	Name string
}

func NewDecl(name string) *Decl {
	return &Decl{Name: name}
}

type FunctionDecl struct {
	Decl
	Body *Block
}

func NewFunctionDecl(name string, body *Block) *FunctionDecl {
	return &FunctionDecl{Decl: Decl{Name: name}, Body: body}
}
func (f *FunctionDecl) Accept(visitor IAstVisitor) any {
	return visitor.VisitFunctionDecl(f)
}
func (f *FunctionDecl) Dump(prefix string) {
	fmt.Println(prefix + "FunctionDecl " + f.Name)
	f.Body.Dump(prefix + "\t")
}

type Block struct {
	Stmts []Statement
}

func NewBlock(stmts []Statement) *Block {
	return &Block{Stmts: stmts}
}
func (b *Block) Accept(visitor IAstVisitor) any {
	return visitor.VisitBlock(b)
}
func (b Block) Dump(prefix string) {
	fmt.Println(prefix + "Block")
	for _, x := range b.Stmts {
		x.Dump(prefix + "    ")
	}
}

type Prog struct {
	Block
}

func NewProg(stmts []Statement) *Prog {
	return &Prog{Block: Block{Stmts: stmts}}
}
func (p *Prog) Accept(visitor IAstVisitor) any {
	return visitor.VisitProg(p)
}
func (p *Prog) Dump(prefix string) {
	fmt.Println(prefix + "Prog")
	for _, stmt := range p.Stmts {
		stmt.Dump(prefix + "\t")
	}
}

type VariableDecl struct {
	Decl
	varType string
	Init    Expression
}

func NewVariableDecl(name, varType string, init Expression) *VariableDecl {
	return &VariableDecl{
		Decl:    Decl{Name: name},
		varType: varType,
		Init:    init,
	}
}
func (v *VariableDecl) Accept(visitor IAstVisitor) any {
	return visitor.VisitVariableDecl(v)
}
func (v *VariableDecl) Dump(prefix string) {
	fmt.Println(prefix + "VariableDecl " + v.Name + ", type: " + v.varType)
	if v.Init == nil {
		fmt.Println(prefix + "no Iialization.")
	} else {
		v.Init.Dump(prefix + "    ")
	}
}

type Binary struct {
	Op   string
	Exp1 Expression
	Exp2 Expression
}

func NewBinary(op string, exp1, exp2 Expression) *Binary {
	return &Binary{
		Op:   op,
		Exp1: exp1,
		Exp2: exp2,
	}
}
func (b *Binary) Accept(visitor IAstVisitor) any {
	return visitor.VisitBinary(b)
}
func (b *Binary) Dump(prefix string) {
	fmt.Println(prefix + "Binary:" + b.Op)
	b.Exp1.Dump(prefix + "    ")
	b.Exp2.Dump(prefix + "    ")
}

type ExpressionStatement struct {
	Exp Expression
}

func NewExpressionStatement(exp Expression) *ExpressionStatement {
	return &ExpressionStatement{Exp: exp}
}
func (e *ExpressionStatement) Accept(visitor IAstVisitor) any {
	return visitor.VisitExpressionStatement(e)
}
func (e *ExpressionStatement) Dump(prefix string) {
	fmt.Println(prefix + "ExpressionStatement")
	e.Exp.Dump(prefix + "    ")
}

type FunctionCall struct {
	Name       string
	Parameters []Expression
	Decl       *FunctionDecl
}

func NewFunctionCall(name string, parameters []Expression) *FunctionCall {
	return &FunctionCall{
		Name:       name,
		Parameters: parameters,
	}
}
func (f *FunctionCall) Accept(visitor IAstVisitor) any {
	return visitor.VisitFunctionCall(f)
}
func (f *FunctionCall) Dump(prefix string) {
	fmt.Print(prefix + "FunctionCall " + f.Name)
	if f.Decl != nil {
		fmt.Print(", resolved")
	} else {
		fmt.Print(", not resolved")
	}
	fmt.Println()
	for _, x := range f.Parameters {
		x.Dump(prefix + "    ")
	}
}

// 变量引用
type Variable struct {
	Expression
	Name string
	Decl *VariableDecl
}

func NewVariable(name string) *Variable {
	return &Variable{
		Name: name,
	}
}
func (v *Variable) Accept(visitor IAstVisitor) any {
	return visitor.VisitVariable(v)
}
func (v *Variable) Dump(prefix string) {
	fmt.Print(prefix + "Variable: " + v.Name)
	if v.Decl == nil {
		fmt.Print(", not resolved")
	} else {
		fmt.Print(", resolved")
	}
	fmt.Println()
}

type StringLiteral struct {
	Expression
	value string
}

func NewStringLiteral(value string) *StringLiteral {
	return &StringLiteral{value: value}
}
func (s *StringLiteral) Accept(visitor IAstVisitor) any {
	return visitor.VisitStringLiteral(s)
}
func (s *StringLiteral) Dump(prefix string) {
	fmt.Println(prefix + s.value)
}

type IntegerLiteral struct {
	Expression
	value int
}

func NewIntegerLiteral(value int) *IntegerLiteral {
	return &IntegerLiteral{value: value}
}
func (i *IntegerLiteral) Accept(visitor IAstVisitor) any {
	return visitor.VisitIntegerLiteral(i)
}
func (i *IntegerLiteral) Dump(prefix string) {
	fmt.Println(prefix + strconv.Itoa(i.value))
}

type DecimalLiteral struct {
	Expression
	value float64
}

func NewDecimalLiteral(value float64) *DecimalLiteral {
	return &DecimalLiteral{value: value}
}
func (d *DecimalLiteral) Accept(visitor IAstVisitor) any {
	return visitor.VisitDecimalLiteral(d)
}
func (d *DecimalLiteral) Dump(prefix string) {
	fmt.Printf("%s %.f\n", prefix, d.value)
}

type NullLiteral struct {
	Expression
	value interface{}
}

func NewNullLiteral() *NullLiteral {
	return &NullLiteral{value: nil}
}
func (n *NullLiteral) Accept(visitor IAstVisitor) any {
	return visitor.VisitNullLiteral(n)
}
func (n *NullLiteral) Dump(prefix string) {
	fmt.Println(prefix + " null")
}

type BooleanLiteral struct {
	Expression
	value bool
}

func NewBooleanLiteral(value bool) *BooleanLiteral {
	return &BooleanLiteral{value: value}
}
func (b *BooleanLiteral) Accept(visitor IAstVisitor) any {
	return visitor.VisitBooleanLiteral(b)
}
func (b *BooleanLiteral) Dump(prefix string) {
	fmt.Println(prefix + strconv.FormatBool(b.value))
}

// 访问
type IAstVisitor interface {
	Visit(node AstNode) any
	VisitProg(prog *Prog) any
	VisitVariableDecl(variableDecl *VariableDecl) any
	VisitFunctionDecl(functionDecl *FunctionDecl) any
	VisitBlock(block *Block) any
	VisitExpressionStatement(stmt *ExpressionStatement) any
	VisitBinary(exp *Binary) any
	VisitIntegerLiteral(exp *IntegerLiteral) any
	VisitDecimalLiteral(exp *DecimalLiteral) any
	VisitStringLiteral(exp *StringLiteral) any
	VisitNullLiteral(exp *NullLiteral) any
	VisitBooleanLiteral(exp *BooleanLiteral) any
	VisitVariable(variable *Variable) any
	VisitFunctionCall(functionCall *FunctionCall) any
}
type AstVisitor struct {
}

func (a *AstVisitor) Visit(node AstNode) any {
	return node.Accept(a)
}
func (a *AstVisitor) VisitProg(prog *Prog) any {
	var retVal interface{}
	for _, x := range prog.Stmts {
		retVal = a.Visit(x)
	}
	return retVal
}
func (a *AstVisitor) VisitVariableDecl(variableDecl *VariableDecl) any {
	if variableDecl.Init != nil {
		return a.Visit(variableDecl.Init)
	}
	return nil
}

func (a *AstVisitor) VisitFunctionDecl(functionDecl *FunctionDecl) any {
	return a.VisitBlock(functionDecl.Body)
}
func (a *AstVisitor) VisitBlock(block *Block) any {
	var retVal any
	for _, x := range block.Stmts {
		retVal = a.Visit(x)
	}
	return retVal
}
func (a *AstVisitor) VisitExpressionStatement(stmt *ExpressionStatement) any {
	return a.Visit(stmt.Exp)
}
func (a *AstVisitor) VisitBinary(exp *Binary) any {
	a.Visit(exp.Exp1)
	a.Visit(exp.Exp2)
	return 0
}
func (a *AstVisitor) VisitIntegerLiteral(exp *IntegerLiteral) any {
	return exp.value
}
func (a *AstVisitor) VisitDecimalLiteral(exp *DecimalLiteral) any {
	return exp.value
}
func (a *AstVisitor) VisitStringLiteral(exp *StringLiteral) any {
	return exp.value
}
func (a *AstVisitor) VisitNullLiteral(exp *NullLiteral) any {
	return exp.value
}
func (a *AstVisitor) VisitBooleanLiteral(exp *BooleanLiteral) any {
	return exp.value
}
func (a *AstVisitor) VisitVariable(variable *Variable) any {
	return nil
}
func (a *AstVisitor) VisitFunctionCall(functionCall *FunctionCall) any {
	return nil
}
