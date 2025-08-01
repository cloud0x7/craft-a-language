package parser

import (
	// "craft-language/mytype"

	"craft-language/scanner"
	"fmt"
)

// 移除了Dump()方法，打印对象内容的任务交给独立的访问者
type IAstNode interface {
	Accept(visitor IAstVisitor, additional any) any
	GetPosition() *scanner.Position // 临时添加的，方便获取属性
}

type IAstVisitor interface {
	Visit(node IAstNode, additional any) any
	VisitProg(prog *Prog, additional any) any
	VisitFunctionDecl(functionDecl *FunctionDecl, additional any) any
	VisitFunctionCall(functionCall *FunctionCall, additional any) any

	VisitBlock(block *Block, additional any) any
	VisitVariableStatement(variableStmt *VariableStatement, additional any) any
	VisitVariableDecl(variableDecl *VariableDecl, additional any) any
	VisitExpressionStatement(stmt *ExpressionStatement, additional any) any
	VisitVariable(variable *Variable, additional any) any
	VisitStringLiteral(exp *StringLiteral, additional any) any
	VisitIntegerLiteral(exp *IntegerLiteral, additional any) any
	VisitDecimalLiteral(exp *DecimalLiteral, additional any) any
	VisitNullLiteral(exp *NullLiteral, additional any) any
	VisitBooleanLiteral(exp *BooleanLiteral, additional any) any
	VisitErrorExp(exp *ErrorExp, additional any) any
	VisitErrorStmt(exp *ErrorStmt, additional any) any

	VisitCallSignature(callSinature *CallSignature, additional any) any
	VisitParameterList(paramList *ParameterList, additional any) any
	VisitReturnStatement(stmt *ReturnStatement, additional any) any

	VisitBinary(exp *Binary, additional any) any
	VisitUnary(exp *Unary, additional any) any

	VisitIfStatement(stmt *IfStatement, additional any) any
	VisitForStatement(stmt *ForStatement, additional any) any
}

// AST基类
type AstNode struct {
	beginPos    *scanner.Position
	endPos      *scanner.Position
	isErrorNode bool
}

func NewAstNode(beginPos, endPos *scanner.Position, isErrorNode bool) *AstNode {
	return &AstNode{beginPos: beginPos, endPos: endPos, isErrorNode: isErrorNode}
}
func (a AstNode) Accept(visitor IAstVisitor, additional any) any {
	return visitor.Visit(a, additional)
}

func (a AstNode) GetPosition() *scanner.Position {
	return a.beginPos
}

// 语句
// 其子类包括函数声明、表达式语句
type IStatement interface {
	IAstNode
}

// 声明
type IDecl interface {
	IAstNode
}

// 声明基类
type Decl struct {
	*AstNode
	Name string
}

func NewDecl(beginPos, endPos *scanner.Position, isErrorNode bool, name string) *Decl {
	return &Decl{AstNode: NewAstNode(beginPos, endPos, isErrorNode), Name: name}
}

// ///////////////////////////////////////////////////////////
// 语句
// 函数声明节点
type FunctionDecl struct {
	Decl
	CallSignature *CallSignature // 函数签名
	Body          *Block         // 函数体
	scope         *Scope         // 函数对应的作用域
	Sym           *FunctionSymbol
}

func NewFunctionDecl(beginPos *scanner.Position, name string, callSignature *CallSignature, body *Block, isErrorNode bool) *FunctionDecl {
	return &FunctionDecl{Body: body, CallSignature: callSignature, Decl: *NewDecl(beginPos, body.endPos, isErrorNode, name)}
}
func (f *FunctionDecl) Accept(visitor IAstVisitor, additional any) any {
	return visitor.VisitFunctionDecl(f, additional)
}

// 函数签名
type CallSignature struct {
	AstNode
	ParamList *ParameterList // 参数列表
	TheType   IType          // 返回值类型
}

func NewCallSignature(beginPos, endPos *scanner.Position, paramList *ParameterList, theType IType, isErrorNode bool) *CallSignature {
	return &CallSignature{AstNode: AstNode{beginPos: beginPos, endPos: endPos, isErrorNode: isErrorNode}, ParamList: paramList, TheType: theType}
}
func (c *CallSignature) Accept(visitor IAstVisitor, additional any) any {
	return visitor.VisitCallSignature(c, additional)
}

// 参数列表
type ParameterList struct {
	AstNode
	Params []*VariableDecl // 把参数当作变量声明
}

func NewParameterList(beginPos, endPos *scanner.Position, params []*VariableDecl, isErrorNode bool) *ParameterList {
	return &ParameterList{AstNode: AstNode{beginPos: beginPos, endPos: endPos}, Params: params}
}
func (p *ParameterList) Accept(visitor IAstVisitor, additional any) any {
	return visitor.VisitParameterList(p, additional)
}

// 函数体
type Block struct {
	*AstNode
	Stmts []IStatement
	scope *Scope
}

func NewBlock(beginPos, endPos *scanner.Position, stmt []IStatement, isErrorNode bool) *Block {
	return &Block{Stmts: stmt, AstNode: NewAstNode(beginPos, endPos, isErrorNode)}
}
func (b *Block) Accept(visitor IAstVisitor, additional any) any {
	return visitor.VisitBlock(b, additional)
}

// 程序，AST的根节点，也可以当作一个函数
type Prog struct {
	*Block
	Sym *FunctionSymbol
}

func NewProg(beginPos, endPos *scanner.Position, stmt []IStatement) *Prog {
	return &Prog{Block: NewBlock(beginPos, endPos, stmt, false)}
}
func (p *Prog) Accept(visitor IAstVisitor, additional any) any {
	return visitor.VisitProg(p, additional)
}

// 临时添加的接口，方便获取属性
type IExpressionAttr interface {
	GetTheType() IType
	IsLeftValue() bool
}

// ///////////////////////////////////////////////////////////
// 表达式
// 表达式语句
type IExpression interface {
	IAstNode
	IExpressionAttr
}

// 表达式语句
type Expression struct {
	*AstNode
	TheType           IType
	shouldBeLeftValue bool // 需要一个左值
	isLeftValue       bool // 是否是一个左值
	constValue        any  // 表达式的常量值，用在常量折叠、流程分析时

	inferredType IType // 推断类型，一般是TheType的子类型
}

func NewExpression(beginPos, endPos *scanner.Position, isErrorNode bool) *Expression {
	return &Expression{
		AstNode: NewAstNode(beginPos, endPos, isErrorNode),
	}
}

func (e *Expression) Accept(visitor IAstVisitor, additional any) any {
	return visitor.Visit(e, additional)
}
func (e *Expression) GetTheType() IType {
	return e.TheType
}
func (e *Expression) IsLeftValue() bool {
	return e.isLeftValue
}

func getBeginPosition(exp IExpression) *scanner.Position {
	switch exp.(type) {
	case *Expression:
		return exp.(*Expression).beginPos
	case *Variable:
		return exp.(*Variable).beginPos
	case *Unary:
		return exp.(*Unary).beginPos
	case *IntegerLiteral:
		return exp.(*IntegerLiteral).beginPos
	default:
		return scanner.NewPosition(0, 0, 0, 0)
	}
}

func getEndPosition(exp IExpression) *scanner.Position {
	switch exp.(type) {
	case *Expression:
		return exp.(*Expression).endPos
	case *Variable:
		return exp.(*Variable).endPos
	case *Unary:
		return exp.(*Unary).endPos
	case *IntegerLiteral:
		return exp.(*IntegerLiteral).endPos
	default:
		return scanner.NewPosition(0, 0, 0, 0)
	}
}

// 二元表达式
type Binary struct {
	Expression
	Op   scanner.OP  // 运算符
	Exp1 IExpression // 左边表达式
	Exp2 IExpression // 右边表达式
}

func NewBinary(op scanner.OP, exp1, exp2 IExpression, isErrorNode bool) *Binary {
	return &Binary{
		Expression: *NewExpression(getBeginPosition(exp1), getEndPosition(exp2), isErrorNode),
		Op:         op,
		Exp1:       exp1,
		Exp2:       exp2,
	}
}
func (b *Binary) Accept(visitor IAstVisitor, additional any) any {
	return visitor.VisitBinary(b, additional)
}

// 一元表达式
type Unary struct {
	Expression
	Op       scanner.OP
	Exp      IExpression
	isPrefix bool // 前缀还是后缀
}

func NewUnary(beginPos, endPos *scanner.Position, op scanner.OP, exp IExpression, isPrefix, isErrorNode bool) *Unary {
	return &Unary{Op: op, Exp: exp, isPrefix: isPrefix,
		Expression: *NewExpression(exp.(*Unary).beginPos, exp.(*Unary).endPos, isErrorNode)}
}
func (u *Unary) Accept(visitor IAstVisitor, additional any) any {
	return visitor.VisitUnary(u, additional)
}

// 表达式语句，表达式后面加分号
type ExpressionStatement struct {
	*AstNode
	exp IExpression
}

func NewExpressionStatement(beginPos, endPos *scanner.Position, exp IExpression, isErrorNode bool) *ExpressionStatement {
	return &ExpressionStatement{exp: exp, AstNode: NewAstNode(beginPos, endPos, isErrorNode)}
}

func (e *ExpressionStatement) Accept(visitor IAstVisitor, additional any) any {
	return visitor.VisitExpressionStatement(e, additional)
}

// 返回语句
type ReturnStatement struct {
	*AstNode
	Exp IExpression
}

func NewReturnStatement(beginPos, endPos *scanner.Position, exp IExpression, isErrorNode bool) *ReturnStatement {
	return &ReturnStatement{Exp: exp, AstNode: NewAstNode(beginPos, endPos, isErrorNode)}
}
func (e *ReturnStatement) Accept(visitor IAstVisitor, additional any) any {
	return visitor.VisitReturnStatement(e, additional)
}

// if语句
type IfStatement struct {
	*AstNode
	condition IExpression // 条件
	stmt      IStatement  // if语句块
	elseStmt  IStatement  // else语句块
}

func NewIfStatement(beginPos, endPos *scanner.Position, condition IExpression, stmt IStatement, elseStmt IStatement, isErrorNode bool) *IfStatement {
	return &IfStatement{condition: condition, stmt: stmt, elseStmt: elseStmt, AstNode: NewAstNode(beginPos, endPos, isErrorNode)}
}
func (i *IfStatement) Accept(visitor AstVisitor, additional any) any {
	return visitor.VisitIfStatement(i, additional)
}

type ForStatement struct {
	*AstNode
	init      IExpression // 初始条件
	condition IExpression // 循环条件
	increment IExpression // 自增
	stmt      IStatement
}

func NewForStatement(beginPos, endPos *scanner.Position, init IExpression, condition IExpression, increment IExpression, stmt IStatement, isErrorNode bool) *ForStatement {
	return &ForStatement{init: init, condition: condition, increment: increment, stmt: stmt, AstNode: NewAstNode(beginPos, endPos, isErrorNode)}
}

func (f *ForStatement) Accept(visitor AstVisitor, additional any) any {
	return visitor.VisitForStatement(f, additional)
}

// 函数调用
type FunctionCall struct {
	Expression
	Name      string
	Arguments []IExpression
	Sym       *FunctionSymbol
}

func NewFunctionCall(beginPos, endPos *scanner.Position, name string, paramValues []IExpression, isErrorNode bool) *FunctionCall {
	return &FunctionCall{Name: name, Arguments: paramValues, Expression: *NewExpression(beginPos, endPos, isErrorNode)}
}
func (f *FunctionCall) Accept(visitor IAstVisitor, additional any) any {
	return visitor.VisitFunctionCall(f, additional)
}

// ******
// ******
// ******
// ******
// ******
// ******
// ******
// ******
// ******
// ******
// ******
// ******
// ******
// ******
type VariableStatement struct {
	*AstNode
	variableDecl *VariableDecl
}

func NewVariableStatement(beginPos, endPos *scanner.Position, variableDecl *VariableDecl, isErrorNode bool) *VariableStatement {
	return &VariableStatement{variableDecl: variableDecl, AstNode: NewAstNode(beginPos, endPos, isErrorNode)}
}
func (v *VariableStatement) Accept(visitor IAstVisitor, additional any) any {
	return visitor.VisitVariableStatement(v, additional)
}

// 变量声明节点
type VariableDecl struct {
	Decl
	TheType      IType       // 变量类型
	Init         IExpression // 初始化表达式
	Sym          *VarSymbol
	inferredType IType // 推断类型
}

func NewVariableDecl(beginPos, endPos *scanner.Position, name string, theType IType, init IExpression, isErrorNode bool) *VariableDecl {
	return &VariableDecl{Decl: *NewDecl(beginPos, endPos, isErrorNode, name), Init: init, TheType: theType}
}
func (v *VariableDecl) Accept(visitor IAstVisitor, additional any) any {
	return visitor.VisitVariableDecl(v, additional)
}

type Variable struct {
	Expression
	Name string
	Sym  *VarSymbol
}

func NewVariable(beginPos, endPos *scanner.Position, name string, isErrorNode bool) *Variable {
	return &Variable{Name: name, Expression: *NewExpression(beginPos, endPos, isErrorNode)}
}

func (v *Variable) Accept(visitor IAstVisitor, additional any) any {
	return visitor.VisitVariable(v, additional)
}

// 字符串字面量
type StringLiteral struct {
	*AstNode
	Expression
	value string
}

func NewStringLiteral(pos *scanner.Position, value string, isErrorNode bool) *StringLiteral {
	return &StringLiteral{value: value, AstNode: NewAstNode(pos, pos, isErrorNode), Expression: Expression{TheType: String}}
}
func (s *StringLiteral) Accept(visitor IAstVisitor, additional any) any {
	return visitor.VisitStringLiteral(s, additional)
}

// 整型字面量
type IntegerLiteral struct {
	Expression
	value int
}

func NewIntegerLiteral(pos *scanner.Position, value int, isErrorNode bool) *IntegerLiteral {
	exp := NewExpression(pos, pos, isErrorNode)
	exp.TheType = Integer
	exp.constValue = value
	exp.AstNode = NewAstNode(pos, pos, isErrorNode)
	return &IntegerLiteral{value: value,
		Expression: *exp,
	}
}

func (i *IntegerLiteral) Accept(visitor IAstVisitor, additional any) any {
	return visitor.VisitIntegerLiteral(i, additional)
}

// 实数字面量
type DecimalLiteral struct {
	*AstNode
	Expression
	value float64
}

func NewDecimalLiteral(pos *scanner.Position, value float64, isErrorNode bool) *DecimalLiteral {
	return &DecimalLiteral{value: value, Expression: Expression{TheType: Decimal}, AstNode: NewAstNode(pos, pos, isErrorNode)}
}
func (d *DecimalLiteral) Accept(visitor IAstVisitor, additional any) any {
	return visitor.VisitDecimalLiteral(d, additional)
}

// null字面量
type NullLiteral struct {
	*AstNode
	Expression
	value any
}

func NewNullLiteral(pos *scanner.Position, isErrorNode bool) *NullLiteral {
	return &NullLiteral{value: nil, AstNode: NewAstNode(pos, pos, isErrorNode), Expression: Expression{TheType: Null}}
}

func (n *NullLiteral) Accept(visitor IAstVisitor, additional any) any {
	return visitor.VisitNullLiteral(n, additional)
}

// Boolean字面量
type BooleanLiteral struct {
	*AstNode
	Expression
	value bool
}

func NewBooleanLiteral(pos *scanner.Position, value bool, isErrorNode bool) *BooleanLiteral {
	return &BooleanLiteral{value: value, AstNode: NewAstNode(pos, pos, isErrorNode), Expression: Expression{TheType: Boolean, constValue: value}}
}
func (b *BooleanLiteral) Accept(visitor IAstVisitor, additional any) any {
	return visitor.VisitBooleanLiteral(b, additional)
}

// 错误表达式
type ErrorExp struct {
	Expression
}

func NewErrorExp(beginPos, endPos *scanner.Position) *ErrorExp {
	return &ErrorExp{Expression: *NewExpression(beginPos, endPos, true)}
}
func (e *ErrorExp) Accept(visitor IAstVisitor, additional any) any {
	return visitor.VisitErrorExp(e, additional)
}

// 错误语句
type ErrorStmt struct {
	*AstNode
}

func NewErrorStmt(beginPos, endPos *scanner.Position) *ErrorStmt {
	return &ErrorStmt{AstNode: NewAstNode(beginPos, endPos, true)}
}
func (e *ErrorStmt) Accept(visitor IAstVisitor, additional any) any {
	return visitor.VisitErrorStmt(e, additional)
}

//////////////////////////
// Visitor

// 这是一个基类，定义了缺省的遍历方式。子类可以覆盖某些方法，修改遍历方式
type AstVisitor struct {
	Curr IAstVisitor // 这里保存一下当前子类，方便选择调用哪个类的方法
}

func (a *AstVisitor) Visit(node IAstNode, additional any) any {
	return node.Accept(a.Curr, additional)
}
func (a *AstVisitor) VisitProg(prog *Prog, additional any) any {
	return a.Curr.VisitBlock(prog.Block, additional)
}
func (a *AstVisitor) VisitVariableStatement(variableStmt *VariableStatement, additional any) any {
	return a.Curr.Visit(variableStmt.variableDecl, additional)
}
func (a *AstVisitor) VisitVariableDecl(variableDecl *VariableDecl, additional any) any {
	if variableDecl.Init != nil {
		return a.Curr.Visit(variableDecl.Init, additional)
	}
	return nil
}
func (a *AstVisitor) VisitFunctionDecl(functionDecl *FunctionDecl, additional any) any {
	a.Curr.Visit(functionDecl.CallSignature, additional)
	return a.Curr.Visit(functionDecl.Body, additional)
}
func (a *AstVisitor) VisitCallSignature(callSignature *CallSignature, additional any) any {
	if callSignature.ParamList != nil {
		return a.Curr.Visit(callSignature.ParamList, additional)
	}
	return nil
}
func (a *AstVisitor) VisitParameterList(parameterList *ParameterList, additional any) any {
	var retVal any
	for _, x := range parameterList.Params {
		retVal = a.Curr.Visit(x, additional)
	}
	return retVal
}
func (a *AstVisitor) VisitBlock(block *Block, additional any) any {
	var retVal any
	for _, x := range block.Stmts {
		retVal = a.Curr.Visit(x, additional)
	}
	return retVal
}

func (a *AstVisitor) VisitExpressionStatement(stmt *ExpressionStatement, additional any) any {
	return a.Curr.Visit(stmt.exp, additional)
}

func (a *AstVisitor) VisitReturnStatement(stmt *ReturnStatement, additional any) any {
	if stmt.Exp != nil {
		return a.Curr.Visit(stmt.Exp, additional)
	}
	return nil
}
func (a *AstVisitor) VisitIfStatement(stmt *IfStatement, additional any) any {
	a.Curr.Visit(stmt.condition, additional)
	a.Curr.Visit(stmt.stmt, additional)
	if stmt.elseStmt != nil {
		a.Curr.Visit(stmt.elseStmt, additional)
	}
	return nil
}

func (a *AstVisitor) VisitForStatement(stmt *ForStatement, additional any) any {
	if stmt.init != nil {
		a.Curr.Visit(stmt.init, additional)
	}
	if stmt.condition != nil {
		a.Curr.Visit(stmt.condition, additional)
	}
	if stmt.increment != nil {
		a.Curr.Visit(stmt.increment, additional)
	}
	return a.Curr.Visit(stmt.stmt, additional)
}
func (a *AstVisitor) VisitBinary(exp *Binary, additional any) any {
	a.Curr.Visit(exp.Exp1, additional)
	return a.Curr.Visit(exp.Exp2, additional)
}
func (a *AstVisitor) VisitUnary(exp *Unary, additional any) any {
	return a.Curr.Visit(exp.Exp, additional)
}

func (a *AstVisitor) VisitIntegerLiteral(exp *IntegerLiteral, additional any) any {
	return exp.value
}
func (a *AstVisitor) VisitDecimalLiteral(exp *DecimalLiteral, additional any) any {
	return exp.value
}
func (a *AstVisitor) VisitStringLiteral(exp *StringLiteral, additional any) any {
	return exp.value
}
func (a *AstVisitor) VisitNullLiteral(exp *NullLiteral, additional any) any {
	return exp.value
}
func (a *AstVisitor) VisitBooleanLiteral(exp *BooleanLiteral, additional any) any {
	return exp.value
}
func (a *AstVisitor) VisitVariable(variable *Variable, additional any) any {
	return nil
}
func (a *AstVisitor) VisitFunctionCall(functionCall *FunctionCall, additional any) any {
	for _, param := range functionCall.Arguments {
		a.Curr.Visit(param, additional)
	}
	return nil
}

func (a *AstVisitor) VisitErrorExp(exp *ErrorExp, additional any) any {
	return nil
}
func (a *AstVisitor) VisitErrorStmt(exp *ErrorStmt, additional any) any {
	return nil
}

// 打印AST的调试信息，主要打印一些属性、类型信息、是否有错误记录、是否引用消解等
type AstDumper struct {
	AstVisitor
}

func NewAstDumper() *AstDumper {
	r := &AstDumper{}
	r.AstVisitor.Curr = r
	return r
}

func (a *AstDumper) VisitProg(prog *Prog, prefix any) any {
	fmt.Printf("%sProg", prefix)
	if prog.isErrorNode {
		fmt.Print(" **EL** ")
	}
	fmt.Println()
	for _, x := range prog.Stmts {
		a.Visit(x, fmt.Sprintf("%s    ", prefix))
	}
	return nil
}
func (a *AstDumper) VisitVariableStatement(variableStmt *VariableStatement, prefix any) any {
	fmt.Printf("%sVariableStatement", prefix)
	if variableStmt.isErrorNode {
		fmt.Print(" **EL** ")
	}
	fmt.Println()
	a.AstVisitor.Visit(variableStmt.variableDecl, fmt.Sprintf("%s    ", prefix))
	return nil
}
func (a *AstDumper) VisitVariableDecl(variableDecl *VariableDecl, prefix any) any {
	fmt.Printf("%sVariableDecl %s", prefix, variableDecl.Name)
	if variableDecl.TheType != nil {
		fmt.Printf("(%s)", variableDecl.TheType.(*SimpleType).Name)
	}
	if variableDecl.isErrorNode {
		fmt.Print(" **EL** ")
	}
	fmt.Println()

	if variableDecl.Init == nil {
		fmt.Printf("%sno initialization.\n", prefix)
	} else {
		a.AstVisitor.Visit(variableDecl.Init, fmt.Sprintf("%s    ", prefix))
	}
	return nil
}
func (a *AstDumper) VisitFunctionDecl(functionDecl *FunctionDecl, prefix any) any {
	fmt.Printf("%sFunctionDecl %s", prefix, functionDecl.Name)
	if functionDecl.isErrorNode {
		fmt.Print(" **EL** ")
	}
	fmt.Println()
	a.AstVisitor.Visit(functionDecl.CallSignature, fmt.Sprintf("%s    ", prefix))
	return a.AstVisitor.Visit(functionDecl.Body, fmt.Sprintf("%s    ", prefix))
}

func (a *AstDumper) VisitCallSignature(callSignature *CallSignature, prefix any) any {
	fmt.Printf("%s", prefix)
	if callSignature.isErrorNode {
		fmt.Print(" **EL** ")
	}
	fmt.Printf("Return type: %s", callSignature.TheType.(*SimpleType).Name)
	fmt.Println()

	if callSignature.ParamList != nil {
		a.Visit(callSignature.ParamList, fmt.Sprintf("%s    ", prefix))
	}
	return nil
}

func (a *AstDumper) VisitParameterList(paramList *ParameterList, prefix any) any {
	fmt.Printf("%sParamList: ", prefix)
	if paramList.isErrorNode {
		fmt.Print(" **EL** ")
	}
	if len(paramList.Params) == 0 {
		fmt.Print(" none ")
	}
	fmt.Println()
	for _, x := range paramList.Params {
		a.Visit(x, fmt.Sprintf("%s    ", prefix))
	}
	return nil
}
func (a *AstDumper) VisitBlock(block *Block, prefix any) any {
	if block.isErrorNode {
		fmt.Printf("%sBlock", prefix)
		fmt.Print(" **EL** ")
		fmt.Println()
	}
	for _, x := range block.Stmts {
		a.Visit(x, fmt.Sprintf("%s    ", prefix))
	}
	return nil
}
func (a *AstDumper) VisitExpressionStatement(stmt *ExpressionStatement, prefix any) any {
	fmt.Printf("%sExpressionStatement", prefix)
	if stmt.isErrorNode {
		fmt.Print(" **E** ")
	}
	fmt.Println()
	a.Visit(stmt.exp, fmt.Sprintf("%s    ", prefix))
	return nil
}

func (a *AstDumper) VisitReturnStatement(stmt *ReturnStatement, prefix any) any {
	fmt.Printf("%sReturnStatement", prefix)
	if stmt.isErrorNode {
		fmt.Print(" **E** ")
	}
	fmt.Println()
	if stmt.Exp != nil {
		return a.Visit(stmt.Exp, fmt.Sprintf("%s    ", prefix))
	}
	return nil
}

func (a *AstDumper) VisitBinary(exp *Binary, prefix any) any {
	fmt.Printf("%sBinary:%s", prefix, exp.Op.ToString())
	if exp.TheType != nil {
		fmt.Printf("(%s)", exp.TheType.(*SimpleType).Name)
	}
	if exp.isErrorNode {
		fmt.Printf(" **E** ")
	}
	fmt.Println()
	a.Visit(exp.Exp1, fmt.Sprintf("%s    ", prefix))
	return a.Visit(exp.Exp2, fmt.Sprintf("%s    ", prefix))
}
func (a *AstDumper) VisitUnary(exp *Unary, prefix any) any {
	fmt.Print(prefix)
	if exp.isPrefix {
		fmt.Print("Prefix")
	} else {
		fmt.Print("Postfix")
	}
	fmt.Printf("Unary:%d", exp.Op)
	if exp.TheType != nil {
		fmt.Printf(":%s", exp.TheType.(*SimpleType).Name)
	}
	if exp.isErrorNode {
		fmt.Printf(" **E** ")
	}
	fmt.Println()
	return a.Visit(exp.Exp, fmt.Sprintf("%s    ", prefix))
}

func (a *AstDumper) VisitIntegerLiteral(exp *IntegerLiteral, additional any) any {
	fmt.Printf("%s%d", additional, exp.value)
	if exp.TheType != nil {
		fmt.Printf("(%s)", exp.TheType.(*SimpleType).Name)
	}
	if exp.isErrorNode {
		fmt.Printf(" **E** ")
	}
	fmt.Println()
	return nil
}

func (a *AstDumper) VisitFunctionCall(functionCall *FunctionCall, additional any) any {
	fmt.Printf("%sFunctionCall ", additional)
	if functionCall.TheType != nil {
		fmt.Printf("(%s)", functionCall.TheType.(*SimpleType).Name)
	}
	if functionCall.isErrorNode {
		fmt.Printf(" **E** ")
	}
	fmt.Printf("%s", functionCall.Name)
	if _, ok := Built_ins[functionCall.Name]; ok {
		fmt.Print(", built-in")
	} else {
		if functionCall.Sym != nil {
			fmt.Print(", resolved")
		} else {
			fmt.Print(", not resolved")
		}
	}

	fmt.Println()
	for _, param := range functionCall.Arguments {
		a.Curr.Visit(param, additional.(string)+"    ")
	}

	return nil
}

func (a *AstDumper) VisitVariable(variable *Variable, additional any) any {
	fmt.Printf("%sVariable:  ", additional)
	if variable.isErrorNode {
		fmt.Printf(" **E** ")
	}
	fmt.Printf("%s", variable.Name)
	if variable.TheType != nil {
		fmt.Printf("(%s)", variable.TheType.(*SimpleType).Name)
	}
	if variable.IsLeftValue() {
		fmt.Print(", LeftValue")
	}
	if variable.Sym != nil {
		fmt.Print(", resolved")
	} else {
		fmt.Print(", not resolved")
	}
	fmt.Println()
	return nil
}

func (a *AstDumper) VisitDecimalLiteral(exp *DecimalLiteral, additional any) any {
	fmt.Printf("%s%.2f", additional, exp.value)
	if exp.TheType != nil {
		fmt.Printf("(%s)", exp.TheType.(*SimpleType).Name)
	}
	if exp.isErrorNode {
		fmt.Printf(" **E** ")
	}
	fmt.Println()
	return nil
}

func (a *AstDumper) VisitStringLiteral(exp *StringLiteral, additional any) any {
	fmt.Printf("%s%s", additional, exp.value)
	if exp.TheType != nil {
		fmt.Printf("(%s)", exp.TheType.(*SimpleType).Name)
	}
	if exp.isErrorNode {
		fmt.Printf(" **E** ")
	}
	fmt.Println()
	return nil
}

func (a *AstDumper) VisitForStatement(stmt *ForStatement, additional any) any {
	fmt.Printf("%sForStatement", additional)
	if stmt.isErrorNode {
		fmt.Printf(" **E** ")
	}
	if stmt.init != nil {
		fmt.Printf("%s    Init:", additional)
		a.Visit(stmt.init, fmt.Sprintf("%s    ", additional))
	}
	if stmt.condition != nil {
		fmt.Printf("%s    Condition:", additional)
		a.Visit(stmt.condition, fmt.Sprintf("%s    ", additional))
	}
	if stmt.increment != nil {
		fmt.Printf("%s    Increment:", additional)
		a.Visit(stmt.increment, fmt.Sprintf("%s    ", additional))
	}

	fmt.Printf("%s    Body:", additional)
	return a.Visit(stmt.stmt, fmt.Sprintf("%s    ", additional))
}

func init() {
	// 检查一下作为基类，是否实现了所有需要的接口
	var _ IAstVisitor = (*AstVisitor)(nil)
}
