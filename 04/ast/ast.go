package ast

import (
	"fmt"
	"strconv"
)

// 语法分析器
type AstNode interface {
	//打印对象信息，prefix是前面填充的字符串，通常用于缩进显示
	Dump(prefix string)
	//访问者模式
	Accept(visitor IAstVisitor) any
}

// 语句
type Statement interface {
	AstNode
}

// 表达式
type Expression interface {
	AstNode
}

// 声明
type IDecl interface {
	AstNode
}
type Decl struct {
	Name string // 声明都有一个符号
}

func NewDecl(name string) *Decl {
	return &Decl{Name: name}
}

// 函数声明节点
type FunctionDecl struct {
	Decl
	Body *Block // 函数体
}

func NewFunctionDecl(name string, body *Block) *FunctionDecl {
	return &FunctionDecl{Decl: Decl{Name: name}, Body: body}
}
func (f *FunctionDecl) Accept(visitor IAstVisitor) any {
	return visitor.VisitFunctionDecl(f)
}
func (f *FunctionDecl) Dump(prefix string) {
	fmt.Println(prefix + "FunctionDecl " + f.Name)
	f.Body.Dump(prefix + "    ")
}

// 函数体
type Block struct {
	Stmts []Statement // 语句列表
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

// 程序节点，也是AST的根节点
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

// 变量声明节点
type VariableDecl struct {
	Decl
	varType string     // 变量类型
	Init    Expression // 变量初始化所使用的表达式
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

// 二元表达式
type Binary struct {
	Op   string     // 运算符
	Exp1 Expression // 左边表达式
	Exp2 Expression // 右边表达式
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

// 表达式语句
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

// 函数调用
type FunctionCall struct {
	Name       string        // 函数名称
	Parameters []Expression  // 参数列表
	Decl       *FunctionDecl // 指向声明
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
	Decl *VariableDecl // 指向声明
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

// 字符串字面量
type StringLiteral struct {
	Expression
	value string // 保存值
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

// 整型字面量
type IntegerLiteral struct {
	Expression
	value int // 保存值
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

// 实数字面量
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

// null字面量
type NullLiteral struct {
	Expression
	value any
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

// Boolean字面量
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

// //////////////////////////////////////////////////////////////////////////////
// 访问者，定义了缺省的遍历方式
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

// 使用一个结构体模拟基类
type AstVisitor struct {
}

// 相应的具体类，会调用visitor合适的具体方法
func (a *AstVisitor) Visit(node AstNode) any {
	return node.Accept(a)
}

// 访问根节点
func (a *AstVisitor) VisitProg(prog *Prog) any {
	var retVal any
	// 遍历并处理所有语句
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
	// 访问并处理函数体
	return a.VisitBlock(functionDecl.Body)
}
func (a *AstVisitor) VisitBlock(block *Block) any {
	var retVal any
	// 遍历并处理所有语句
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
	return nil
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
