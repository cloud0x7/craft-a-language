package parser

import (
	"craft-language/scanner"
	"fmt"
)

type IAstNode interface {
	Accept(visitor IAstVisitor, additional any) any
	GetPosition() *scanner.Position
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

func (a AstNode) GetPosition() *scanner.Position {
	return a.beginPos
}
func NewAstNode(beginPos, endPos *scanner.Position, isErrorNode bool) *AstNode {
	return &AstNode{beginPos: beginPos, endPos: endPos, isErrorNode: isErrorNode}
}
func (a AstNode) Accept(visitor IAstVisitor, additional any) any {
	return visitor.Visit(a, additional)
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

// 函数声明节点
type FunctionDecl struct {
	Decl
	CallSignature *CallSignature
	Body          *Block
	scope         *Scope
	Sym           *FunctionSymbol
}

func NewFunctionDecl(beginPos *scanner.Position, name string, callSignature *CallSignature, body *Block, isErrorNode bool) *FunctionDecl {
	return &FunctionDecl{Body: body, CallSignature: callSignature, Decl: *NewDecl(beginPos, body.endPos, isErrorNode, name)}
}
func (f *FunctionDecl) Accept(visitor IAstVisitor, additional any) any {
	return visitor.VisitFunctionDecl(f, additional)
}

type CallSignature struct {
	AstNode
	ParamList *ParameterList
	TheType   IType
}

func NewCallSignature(beginPos, endPos *scanner.Position, paramList *ParameterList, theType IType, isErrorNode bool) *CallSignature {
	return &CallSignature{AstNode: AstNode{beginPos: beginPos, endPos: endPos, isErrorNode: isErrorNode}, ParamList: paramList, TheType: theType}
}
func (c *CallSignature) Accept(visitor IAstVisitor, additional any) any {
	return visitor.VisitCallSignature(c, additional)
}

type ParameterList struct {
	AstNode
	Params []*VariableDecl
}

func NewParameterList(beginPos, endPos *scanner.Position, params []*VariableDecl, isErrorNode bool) *ParameterList {
	return &ParameterList{AstNode: AstNode{beginPos: beginPos, endPos: endPos}, Params: params}
}
func (p *ParameterList) Accept(visitor IAstVisitor, additional any) any {
	return visitor.VisitParameterList(p, additional)
}

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

// 程序，AST的根节点
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

type IExpressionAttr interface {
	GetTheType() IType
	IsLeftValue() bool
}

// 表达式
type IExpression interface {
	IAstNode
	IExpressionAttr
}
type Expression struct {
	*AstNode
	TheType           IType
	shouldBeLeftValue bool
	isLeftValue       bool
	constValue        any

	inferredType IType
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

type Binary struct {
	Expression
	Op   scanner.OP
	Exp1 IExpression
	Exp2 IExpression
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

type Unary struct {
	Expression
	Op       scanner.OP
	Exp      IExpression
	IsPrefix bool
}

func NewUnary(beginPos, endPos *scanner.Position, op scanner.OP, exp IExpression, isPrefix, isErrorNode bool) *Unary {
	return &Unary{Op: op, Exp: exp, IsPrefix: isPrefix,
		Expression: *NewExpression(beginPos, endPos, isErrorNode)}
}
func (u *Unary) Accept(visitor IAstVisitor, additional any) any {
	return visitor.VisitUnary(u, additional)
}
func (u *Unary) GetTheType() IType {
	return nil
}
func (u *Unary) IsLeftValue() bool {
	return false
}

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

type IfStatement struct {
	*AstNode
	Condition IExpression
	Stmt      IStatement
	ElseStmt  IStatement
}

func NewIfStatement(beginPos, endPos *scanner.Position, condition IExpression, stmt IStatement, elseStmt IStatement, isErrorNode bool) *IfStatement {
	return &IfStatement{Condition: condition, Stmt: stmt, ElseStmt: elseStmt, AstNode: NewAstNode(beginPos, endPos, isErrorNode)}
}
func (i *IfStatement) Accept(visitor IAstVisitor, additional any) any {
	return visitor.VisitIfStatement(i, additional)
}

type ForStatement struct {
	*AstNode
	Init      IExpression
	Condition IExpression
	Increment IExpression
	Stmt      IStatement

	Scope *Scope
}

func NewForStatement(beginPos, endPos *scanner.Position, init IExpression, condition IExpression, increment IExpression, stmt IStatement, isErrorNode bool) *ForStatement {
	return &ForStatement{Init: init, Condition: condition, Increment: increment, Stmt: stmt, AstNode: NewAstNode(beginPos, endPos, isErrorNode)}
}

func (f *ForStatement) Accept(visitor IAstVisitor, additional any) any {
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

type VariableDecl struct {
	Decl
	TheType      IType
	Init         IExpression
	Sym          *VarSymbol
	inferredType IType
}

func NewVariableDecl(beginPos, endPos *scanner.Position, name string, theType IType, init IExpression, isErrorNode bool) *VariableDecl {
	return &VariableDecl{Decl: *NewDecl(beginPos, endPos, isErrorNode, name), Init: init, TheType: theType}
}
func (v *VariableDecl) Accept(visitor IAstVisitor, additional any) any {
	return visitor.VisitVariableDecl(v, additional)
}
func (v *VariableDecl) GetTheType() IType {
	return v.TheType
}
func (v *VariableDecl) IsLeftValue() bool {
	// TODO: check
	return true
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

type StringLiteral struct {
	*AstNode
	Expression
	Value string
}

func NewStringLiteral(pos *scanner.Position, value string, isErrorNode bool) *StringLiteral {
	return &StringLiteral{Value: value, AstNode: NewAstNode(pos, pos, isErrorNode), Expression: Expression{TheType: String}}
}
func (s *StringLiteral) Accept(visitor IAstVisitor, additional any) any {
	return visitor.VisitStringLiteral(s, additional)
}

type IntegerLiteral struct {
	Expression
	Value int
}

func NewIntegerLiteral(pos *scanner.Position, value int, isErrorNode bool) *IntegerLiteral {
	exp := NewExpression(pos, pos, isErrorNode)
	exp.TheType = Integer
	exp.constValue = value
	exp.AstNode = NewAstNode(pos, pos, isErrorNode)
	return &IntegerLiteral{Value: value,
		Expression: *exp,
	}
}

func (i *IntegerLiteral) Accept(visitor IAstVisitor, additional any) any {
	return visitor.VisitIntegerLiteral(i, additional)
}

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

type ErrorExp struct {
	Expression
}

func NewErrorExp(beginPos, endPos *scanner.Position) *ErrorExp {
	return &ErrorExp{Expression: *NewExpression(beginPos, endPos, true)}
}
func (e *ErrorExp) Accept(visitor IAstVisitor, additional any) any {
	return visitor.VisitErrorExp(e, additional)
}

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

type AstVisitor struct {
	Curr IAstVisitor
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
	a.Curr.Visit(stmt.Condition, additional)
	a.Curr.Visit(stmt.Stmt, additional)
	if stmt.ElseStmt != nil {
		a.Curr.Visit(stmt.ElseStmt, additional)
	}
	return nil
}

func (a *AstVisitor) VisitForStatement(stmt *ForStatement, additional any) any {
	if stmt.Init != nil {
		a.Curr.Visit(stmt.Init, additional)
	}
	if stmt.Condition != nil {
		a.Curr.Visit(stmt.Condition, additional)
	}
	if stmt.Increment != nil {
		a.Curr.Visit(stmt.Increment, additional)
	}
	return a.Curr.Visit(stmt.Stmt, additional)
}
func (a *AstVisitor) VisitBinary(exp *Binary, additional any) any {
	a.Curr.Visit(exp.Exp1, additional)
	return a.Curr.Visit(exp.Exp2, additional)
}
func (a *AstVisitor) VisitUnary(exp *Unary, additional any) any {
	return a.Curr.Visit(exp.Exp, additional)
}

func (a *AstVisitor) VisitIntegerLiteral(exp *IntegerLiteral, additional any) any {
	return exp.Value
}
func (a *AstVisitor) VisitDecimalLiteral(exp *DecimalLiteral, additional any) any {
	return exp.value
}
func (a *AstVisitor) VisitStringLiteral(exp *StringLiteral, additional any) any {
	return exp.Value
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
		a.AstVisitor.Visit(x, fmt.Sprintf("%s    ", prefix))
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
func (a *AstDumper) VisitIfStatement(stmt *IfStatement, additional any) any {
	fmt.Printf("%sIfStatement", additional)
	if stmt.isErrorNode {
		fmt.Print(" **E** ")
	}
	fmt.Println()
	fmt.Printf("%s    Condition:\n", additional)
	a.Visit(stmt.Condition, fmt.Sprintf("%s    ", additional))
	fmt.Printf("%s    Then:\n", additional)
	a.Visit(stmt.Stmt, fmt.Sprintf("%s    ", additional))
	if stmt.ElseStmt != nil {
		fmt.Printf("%s    Else:\n", additional)
		a.Visit(stmt.ElseStmt, fmt.Sprintf("%s    ", additional))
	}
	return nil
}

func (a *AstDumper) VisitForStatement(stmt *ForStatement, additional any) any {
	fmt.Printf("%sForStatement:", additional)
	if stmt.isErrorNode {
		fmt.Print(" **E** ")
	}
	fmt.Println()
	if stmt.Init != nil {
		fmt.Printf("%s    Init:\n", additional)
		a.Visit(stmt.Init, additional)
	}
	if stmt.Condition != nil {
		fmt.Printf("%sCondition:\n", additional)
		a.Visit(stmt.Condition, additional)
	}
	if stmt.Increment != nil {
		fmt.Printf("%sIncrement:\n", additional)
		a.Visit(stmt.Increment, additional)
	}
	fmt.Printf("%sBody:\n", additional)
	return a.Visit(stmt.Stmt, additional)
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
	if exp.IsPrefix {
		fmt.Print("Prefix")
	} else {
		fmt.Print("Postfix ")
	}
	fmt.Printf("Unary:%s", exp.Op.ToString())
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
	fmt.Printf("%s%d", additional, exp.Value)
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
	fmt.Printf("%s%s", additional, exp.Value)
	if exp.TheType != nil {
		fmt.Printf("(%s)", exp.TheType.(*SimpleType).Name)
	}
	if exp.isErrorNode {
		fmt.Printf(" **E** ")
	}
	fmt.Println()
	return nil
}

func init() {
	var _ IAstVisitor = (*AstVisitor)(nil)
}
