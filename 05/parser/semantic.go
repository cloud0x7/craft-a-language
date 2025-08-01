package parser

import (
	"craft-language/scanner"
	"fmt"
)

// 语义分析功能
type ISemanticAstVisitor interface {
	IAstVisitor
	addError(string, IAstNode)
	addWarning(string, IAstNode)
	getErrors() []*SemanticError
	getWarnings() []*SemanticError
}
type SemanticAnalyer struct {
	passes   []ISemanticAstVisitor // 分析器的内部组件列表
	errors   []*SemanticError      // 语义错误
	warnings []*SemanticError      // 语义警告信息
}

func NewSemanticAnalyer() *SemanticAnalyer {
	ret := &SemanticAnalyer{
		passes: make([]ISemanticAstVisitor, 0),
	}
	// 添加内部组件，包括符号表、引用消解、类型检查、类型转换、左值处理等
	ret.passes = append(ret.passes, NewEnter())
	ret.passes = append(ret.passes, NewRefResolver())
	ret.passes = append(ret.passes, NewTypeChecker())
	ret.passes = append(ret.passes, NewTypeConverter())
	ret.passes = append(ret.passes, NewLeftValueAttributor())

	return ret
}

// 执行语义分析，由每个组件从Prog节点访问开始
func (s *SemanticAnalyer) Execute(prog *Prog) {
	for _, pass := range s.passes {
		pass.VisitProg(prog, nil)
		s.errors = append(s.errors, pass.getErrors()...)
		s.warnings = append(s.warnings, pass.getWarnings()...)
	}
}

// 语义错误
type SemanticError struct {
	*CompilerError
	node IAstNode
}

func NewSemanticError(msg string, node IAstNode, isWarning bool) *SemanticError {
	return &SemanticError{
		CompilerError: NewCompilerError(msg, node.GetPosition(), isWarning),
		node:          node,
	}
}

type SemanticAstVisitor struct {
	AstVisitor
	errors   []*SemanticError
	warnings []*SemanticError
}

// 添加错误信息
func (s *SemanticAstVisitor) addError(msg string, node IAstNode) {
	s.errors = append(s.errors, NewSemanticError(msg, node, false))
	fmt.Printf("@%s : %s\n", node.GetPosition().ToString(), msg)
}

// 添加警告信息
func (s *SemanticAstVisitor) addWarning(msg string, node IAstNode) {
	s.warnings = append(s.warnings, NewSemanticError(msg, node.(AstNode), true))
	fmt.Printf("@%s : %s\n", node.(AstNode).beginPos.ToString(), msg)
}
func (s *SemanticAstVisitor) getErrors() []*SemanticError {
	return s.errors
}
func (s *SemanticAstVisitor) getWarnings() []*SemanticError {
	return s.warnings
}

// 把符号加入符号表
type Enter struct {
	SemanticAstVisitor
	scope       *Scope // 当前所属scope
	functionSym *FunctionSymbol
}

func NewEnter() *Enter {
	r := &Enter{
		SemanticAstVisitor: SemanticAstVisitor{AstVisitor: AstVisitor{}},
	}
	r.SemanticAstVisitor.AstVisitor.Curr = r
	return r
}

// 返回最顶级的Scope对象
func (e *Enter) VisitProg(prog *Prog, additional any) any {
	sym := NewFunctionSymbol("main", NewFunctionType(Integer, []IType{}, ""), []VarSymbol{})
	prog.Sym = sym // 补充一个函数符号
	e.functionSym = sym
	// 继续遍历
	return e.AstVisitor.VisitProg(prog, additional)
}

// 把函数声明加入符号表
func (e *Enter) VisitFunctionDecl(functionDecl *FunctionDecl, additional any) any {
	currentScope := e.scope

	// 创建函数的symbol
	var paramTypes []IType
	if functionDecl.CallSignature.ParamList != nil { // 函数声明有参数
		for _, p := range functionDecl.CallSignature.ParamList.Params {
			paramTypes = append(paramTypes, p.TheType)
		}
	}
	// 函数符号，名称就是函数名称
	sym := NewFunctionSymbol(functionDecl.Name, NewFunctionType(functionDecl.CallSignature.TheType, paramTypes, ""), []VarSymbol{})
	sym.Decl = functionDecl
	functionDecl.Sym = sym

	if currentScope.HasSymbol(functionDecl.Name) { // 如果有这个函数名，添加错误信息
		e.addError("Dumplicate symbol: "+functionDecl.Name, functionDecl)
	} else {
		currentScope.Enter(functionDecl.Name, sym) // 加入符号表
	}
	// 保存当前符号
	lastFunctionSym := e.functionSym
	// 替换为新的
	e.functionSym = sym

	// 保存当前作用域
	oldScope := currentScope
	// 新的作用域
	e.scope = NewScope(oldScope)
	functionDecl.scope = e.scope

	// 遍历子节点
	e.AstVisitor.VisitFunctionDecl(functionDecl, additional)

	// 恢复原符号
	e.functionSym = lastFunctionSym
	// 恢复原作用域
	e.scope = oldScope
	return nil
}

// 遇到块，建立新的作用域
func (e *Enter) VisitBlock(block *Block, additional any) any {
	oldScope := e.scope         // 保存当前作用域
	e.scope = NewScope(e.scope) // 新的作用域
	block.scope = e.scope       // 使用新作用域
	// 遍历子节点
	e.AstVisitor.VisitBlock(block, additional)
	// 恢复原作用域
	e.scope = oldScope

	return nil
}

// 把变量声明加入符号表
func (e *Enter) VisitVariableDecl(variableDecl *VariableDecl, additional any) any {
	currentScope := e.scope
	if currentScope.HasSymbol(variableDecl.Name) {
		e.addError("Dumplicate symbol: "+variableDecl.Name, variableDecl)
	}
	sym := NewVarSymbol(variableDecl.Name, variableDecl.TheType)
	variableDecl.Sym = sym
	currentScope.Enter(variableDecl.Name, sym)

	if e.functionSym != nil {
		e.functionSym.Vars = append(e.functionSym.Vars, *sym)
	}

	return nil
}

// ///////////////////////////////////////////////////////////////////////
// 引用消解
// 遍历AST。如果发现函数调用和变量引用，就去找它的定义
type RefResolver struct {
	SemanticAstVisitor
	scope           *Scope                           // 当前作用域
	declaredVarsMap map[*Scope]map[string]*VarSymbol // 每个作用域对应的变量列表
}

func NewRefResolver() *RefResolver {
	r := &RefResolver{
		SemanticAstVisitor: SemanticAstVisitor{AstVisitor: AstVisitor{}},
		declaredVarsMap:    make(map[*Scope]map[string]*VarSymbol),
		scope:              NewScope(nil),
	}
	r.SemanticAstVisitor.AstVisitor.Curr = r
	return r
}

func (e *RefResolver) VisitFunctionDecl(functionDecl *FunctionDecl, additional any) any {
	oldScope := e.scope          // 保存当前作用域
	e.scope = functionDecl.scope // 修改作用域
	if e.scope == nil {
		e.addError("scope 不可为null", functionDecl)
	}
	// 存储当前作用域
	e.declaredVarsMap[e.scope] = map[string]*VarSymbol{}
	// 遍历下级节点
	e.AstVisitor.VisitFunctionDecl(functionDecl, additional)
	// 恢复作用域
	e.scope = oldScope
	return nil
}

func (e *RefResolver) VisitBlock(block *Block, additional any) any {
	// 修改作用域
	oldScope := e.scope
	e.scope = block.scope
	// 存储当前作用域
	e.declaredVarsMap[e.scope] = map[string]*VarSymbol{}
	e.AstVisitor.VisitBlock(block, additional)
	// 恢复作用域
	e.scope = oldScope
	return nil
}

func (e *RefResolver) VisitFunctionCall(functionCall *FunctionCall, additional any) any {
	currentScope := e.scope

	if _, ok := Built_ins[functionCall.Name]; ok { // 检查是否为内置函数
		functionCall.Sym = Built_ins[functionCall.Name]
	} else { // 不是内置函数，则级联查找
		functionCall.Sym = currentScope.GetSymbolCascade(functionCall.Name).(*FunctionSymbol)
	}

	e.AstVisitor.VisitFunctionCall(functionCall, additional)
	return nil
}

func (e *RefResolver) VisitVariableDecl(variableDecl *VariableDecl, additional any) any {
	currentScope := e.scope
	declaredSyms := e.declaredVarsMap[currentScope]
	sym := currentScope.GetSymbol(variableDecl.Name)
	if sym != nil {
		declaredSyms[variableDecl.Name] = sym.(*VarSymbol)
	}

	e.AstVisitor.VisitVariableDecl(variableDecl, additional)
	return nil
}

// 变量引用消解
func (e *RefResolver) VisitVariable(variable *Variable, additional any) any {
	currentScope := e.scope
	variable.Sym = e.findVariableCascade(currentScope, variable)

	return nil
}

// 逐级查找某个符号是不是在声明前就使用了
func (e *RefResolver) findVariableCascade(scope *Scope, variable *Variable) *VarSymbol {
	declaredSyms := e.declaredVarsMap[scope]
	symInScope := scope.GetSymbol(variable.Name)
	if symInScope != nil {
		if v, ok := declaredSyms[variable.Name]; ok {
			return v
		} else {
			if symInScope.GetKind() == VariableKind {
				e.addError("Variable: '"+variable.Name+"' is used before declaration.", variable)
			} else {
				e.addError("We expect a variable of name: '"+variable.Name+"' but find a "+symInScope.(*Symbol).kind.ToString()+".", variable)
			}
		}
	} else {
		if scope.enclosingScope != nil {
			return e.findVariableCascade(scope.enclosingScope, variable)
		} else {
			e.addError("Cannot find a variable of name: '"+variable.Name+"'", variable)
		}
	}

	return nil
}

// ///////////////////////////////////////////////////////////////////////
// 属性分析
// 左值
type LeftValueAttributor struct {
	SemanticAstVisitor
	ParentOperator scanner.OP // 上层级的操作符
}

func NewLeftValueAttributor() *LeftValueAttributor {
	r := &LeftValueAttributor{
		SemanticAstVisitor: SemanticAstVisitor{AstVisitor: AstVisitor{}},
	}
	r.SemanticAstVisitor.AstVisitor.Curr = r
	return r
}
func (lv *LeftValueAttributor) VisitBinary(exp *Binary, additional any) any {
	if scanner.IsAssignOp(exp.Op) || exp.Op == scanner.Dot { // 操作符是'='或者'.'
		lastParentOperator := lv.ParentOperator
		lv.ParentOperator = exp.Op
		// 检查左子节点
		lv.Visit(exp.Exp1, additional)
		if !exp.Exp1.IsLeftValue() { // 不是左值，添加error
			lv.addError(fmt.Sprintf("Left child of operator %s need a left value", exp.Op.ToString()), exp.Exp1)
		}
		// 恢复原状态
		lv.ParentOperator = lastParentOperator
		// 继续遍历右节点
		lv.AstVisitor.Visit(exp.Exp2, additional)
	} else {
		lv.AstVisitor.VisitBinary(exp, additional)
	}

	return nil
}

func (lv *LeftValueAttributor) VisitVariable(variable *Variable, additional any) any {
	if lv.ParentOperator != 0 { // 没有0值，所以这里代表存在
		t := variable.TheType
		if !t.hasVoid() { // 不是void，就可以作为左值
			variable.isLeftValue = true
		}
	}
	return nil
}

// 函数调用是在.符号左边，并且返回值不为void的时候，可以作为左值
func (lv *LeftValueAttributor) VisitFunctionCall(functionCall *FunctionCall, additional any) any {
	if lv.ParentOperator == scanner.Dot {
		functionType := functionCall.TheType.(*FunctionType)
		if !functionType.ReturnType.hasVoid() {
			functionCall.isLeftValue = true
		}
	}
	return nil
}

// 类型检查
type TypeChecker struct {
	SemanticAstVisitor
}

func NewTypeChecker() *TypeChecker {
	r := &TypeChecker{
		SemanticAstVisitor: SemanticAstVisitor{AstVisitor: AstVisitor{}},
	}
	r.SemanticAstVisitor.AstVisitor.Curr = r
	return r
}
func (t *TypeChecker) VisitVariableDecl(variableDecl *VariableDecl, additional any) any {
	t.AstVisitor.VisitVariableDecl(variableDecl, additional)

	if variableDecl.Init != nil {
		t1 := variableDecl.TheType
		t2 := variableDecl.Init.(IExpression).GetTheType()
		if !t2.LE(t1) { // 值的类型范围大于声明的类型范围
			t.addError("Operator '=' can not be applied to '"+t1.(Type).Name+"' and '"+t2.(Type).Name+"'.", variableDecl)
		}

		if t1 == Any { // 对于any类型，统一改为右边的具体类型
			variableDecl.TheType = t2
			variableDecl.Sym.TheType = t2
		}
	}

	return nil
}

func (t *TypeChecker) VisitBinary(exp *Binary, additional any) any {
	t.AstVisitor.VisitBinary(exp, additional)

	t1 := exp.Exp1.(IExpression).GetTheType()
	t2 := exp.Exp2.(IExpression).GetTheType()

	if scanner.IsAssignOp(exp.Op) {
		exp.TheType = t1
		if !t2.LE(t1) { // 类型不匹配
			t.addError(fmt.Sprintf("operator '%s' can not be applied to %s and %s.\n", exp.Op.ToString(), t1.(Type).Name, t2.(Type).Name), exp)
		}
	} else if exp.Op == scanner.Plus { // 如果操作符是'+'
		if t1 == String || t2 == String { // 如果有一个是字符串，则结果是字符串
			exp.TheType = String
		} else if t1.LE(Number) && t2.LE(Number) {
			exp.TheType = GetUpperBound(t1, t2)
		} else {
			t.addError(fmt.Sprintf("operator '%s' can not be applied to %s and %s.\n", exp.Op.ToString(), t1.(Type).Name, t2.(Type).Name), exp)
		}
	} else if scanner.IsArithmeticOp(exp.Op) {
		if t1.LE(Number) && t2.LE(Number) {
			exp.TheType = GetUpperBound(t1, t2)
		} else {
			t.addError(fmt.Sprintf("operator '%s' can not be applied to %s and %s.\n", exp.Op.ToString(), t1.(Type).Name, t2.(Type).Name), exp)
		}
	} else if scanner.IsLogicalOp(exp.Op) {
		if t1.LE(Boolean) && t2.LE(Boolean) { // 两个都是布尔值
			exp.TheType = Boolean
		} else {
			t.addError(fmt.Sprintf("operator '%s' can not be applied to %s and %s.\n", exp.Op.ToString(), t1.(Type).Name, t2.(Type).Name), exp)
		}
	} else {
		t.addError(fmt.Sprintf("Unsupported binary operator:  %s.\n", exp.Op.ToString()), exp)
	}

	return nil
}
func (tc *TypeChecker) VisitUnary(exp *Unary, additional any) any {
	tc.AstVisitor.VisitUnary(exp, additional)

	t := exp.Exp.(*Expression).TheType
	if exp.Op == scanner.Inc || exp.Op == scanner.Dec {
		if t.LE(Number) {
			exp.TheType = t
		} else {
			tc.addError(fmt.Sprintf("unary operator %s can not be applied to '%s'.", exp.Op.ToString(), t.(Type).Name), exp)
		}
	} else if exp.Op == scanner.Minus || exp.Op == scanner.Plus {
		if t.LE(Number) {
			exp.TheType = t
		} else {
			tc.addError(fmt.Sprintf("unary operator %s can not be applied to '%s'.", exp.Op.ToString(), t.(Type).Name), exp)
		}
	} else if exp.Op == scanner.Not {
		if t.LE(Boolean) {
			exp.TheType = t
		} else {
			tc.addError(fmt.Sprintf("unary operator %s can not be applied to '%s'.", exp.Op.ToString(), t.(Type).Name), exp)
		}
	} else {
		tc.addError(fmt.Sprintf("unsupported unary operator %s applied to '%s'.", exp.Op.ToString(), t.(Type).Name), exp)
	}

	return nil
}
func (t *TypeChecker) VisitVariable(variable *Variable, additional any) any {
	if variable.Sym != nil {
		// 用符号的类型标注本节点
		variable.TheType = variable.Sym.TheType
	}
	return nil
}
func (tc *TypeChecker) VisitFunctionCall(functionCall *FunctionCall, additional any) any {
	if functionCall.Sym != nil {
		functionType := functionCall.Sym.TheType.(*FunctionType)
		// 使用返回值的类型
		functionCall.TheType = functionType.ReturnType
		// 检查参数数量
		if len(functionCall.Arguments) != len(functionType.ParamTypes) {
			tc.addError(fmt.Sprintf("Function Call of %s has %d arguments, while expecting %d.", functionCall.Name, len(functionCall.Arguments), len(functionType.ParamTypes)), functionCall)
		}
		// 依次检查参数类型
		for i := 0; i < len(functionCall.Arguments); i++ {
			tc.Visit(functionCall.Arguments[i], additional)
			if i < len(functionType.ParamTypes) {
				t1 := functionCall.Arguments[i].GetTheType()
				t2 := functionType.ParamTypes[i]
				if !t1.LE(t2) && t2 != String {
					tc.addError(fmt.Sprintf("Argument %d of FunctionCall %s is of Type %s, while expecting %s", i, functionCall.Name, t1.(Type).Name, t2.(Type).Name), functionCall)
				}
			}
		}
	}
	return nil
}

// 类型转换，添加必要的AST节点，来完成转换，当前只把其它类型转换成字符串
type TypeConverter struct {
	SemanticAstVisitor
}

func NewTypeConverter() *TypeConverter {
	r := &TypeConverter{
		SemanticAstVisitor: SemanticAstVisitor{AstVisitor: AstVisitor{}},
	}
	r.SemanticAstVisitor.AstVisitor.Curr = r
	return r
}

func (tc *TypeConverter) VisitBinary(exp *Binary, additional any) any {
	tc.AstVisitor.VisitBinary(exp, additional)

	t1 := exp.Exp1.(IExpression).GetTheType()
	t2 := exp.Exp2.(IExpression).GetTheType()

	if scanner.IsAssignOp(exp.Op) {
		// 一个是字符串，另一个是数值，则使用内置函数转为字符串
		if t1 == String && t2 != String {
			if t2 == Integer {
				tmp := NewFunctionCall(exp.Exp2.(*Expression).beginPos, exp.Exp2.(*Expression).endPos, "integer_to_string", []IExpression{exp.Exp2}, false)
				tmp.Sym = Built_ins["integer_to_string"]
				exp.Exp2 = tmp
			}
		}
	} else if exp.Op == scanner.Plus { // 操作符是'+'
		if t1 == String || t2 == String { // 一个是字符串，另一个是数值，则使用内置函数转为字符串
			if t1 == Integer || t1 == Number || t1 == Decimal {
				tmp := NewFunctionCall(exp.Exp1.(*Expression).beginPos, exp.Exp2.(*Expression).endPos, "integer_to_string", []IExpression{exp.Exp1}, false)
				tmp.Sym = Built_ins["integer_to_string"]
				exp.Exp1 = tmp
			}
			if t2 == Integer || t2 == Number || t2 == Decimal {
				tmp := NewFunctionCall(exp.Exp2.GetPosition(), exp.Exp2.GetPosition(), "integer_to_string", []IExpression{exp.Exp2}, false)
				tmp.Sym = Built_ins["interger_to_string"]
				exp.Exp2 = tmp
			}
		}
	}

	return nil
}

func (tc *TypeConverter) VisitFunctionCall(functionCall *FunctionCall, additional any) any {
	if functionCall.Sym != nil {
		functionType := functionCall.Sym.TheType.(*FunctionType)
		// 检查参数要不要转换
		for i, x := range functionCall.Arguments {
			tc.Visit(x, additional)
			if i < len(functionType.ParamTypes) {
				t1 := x.GetTheType()
				t2 := functionType.ParamTypes[i]

				if (t1 == Integer || t1 == Number) && t2 == String {
					tmp := NewFunctionCall(x.GetPosition(), x.GetPosition(), "integer_to_string", []IExpression{x}, false)
					tmp.Sym = Built_ins["integer_to_string"]
					functionCall.Arguments[i] = tmp
				}
			}
		}
	}
	return nil
}
