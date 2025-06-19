package parser

import (
	"craft-language/scanner"
	"fmt"
)

type ISemanticAstVisitor interface {
	IAstVisitor
	addError(string, IAstNode)
	addWarning(string, IAstNode)
	getErrors() []*SemanticError
	getWarnings() []*SemanticError
}
type SemanticAnalyer struct {
	passes   []ISemanticAstVisitor
	errors   []*SemanticError
	warnings []*SemanticError
}

func NewSemanticAnalyer() *SemanticAnalyer {
	ret := &SemanticAnalyer{
		passes: make([]ISemanticAstVisitor, 0),
	}
	ret.passes = append(ret.passes, NewEnter())
	ret.passes = append(ret.passes, NewRefResolver())
	ret.passes = append(ret.passes, NewTypeChecker())
	ret.passes = append(ret.passes, NewTypeConverter())
	ret.passes = append(ret.passes, NewLeftValueAttributor())

	return ret
}

func (s *SemanticAnalyer) Execute(prog *Prog) {
	for _, pass := range s.passes {
		pass.VisitProg(prog, nil)
		s.errors = append(s.errors, pass.getErrors()...)
		s.warnings = append(s.warnings, pass.getWarnings()...)
	}
}

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

func (s *SemanticAstVisitor) addError(msg string, node IAstNode) {
	s.errors = append(s.errors, NewSemanticError(msg, node, false))
	fmt.Printf("@%s : %s\n", node.GetPosition().ToString(), msg)
}
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

func (e *Enter) VisitProg(prog *Prog, additional any) any {
	sym := NewFunctionSymbol("main", NewFunctionType(Integer, []IType{}, ""), []VarSymbol{})
	prog.Sym = sym
	e.functionSym = sym

	return e.AstVisitor.VisitProg(prog, additional)
}

func (e *Enter) VisitFunctionDecl(functionDecl *FunctionDecl, additional any) any {
	currentScope := e.scope
	var paramTypes []IType
	if functionDecl.CallSignature.ParamList != nil {
		for _, p := range functionDecl.CallSignature.ParamList.Params {
			paramTypes = append(paramTypes, p.TheType)
		}
	}
	sym := NewFunctionSymbol(functionDecl.Name, NewFunctionType(functionDecl.CallSignature.TheType, paramTypes, ""), []VarSymbol{})
	sym.Decl = functionDecl
	functionDecl.Sym = sym

	if currentScope.HasSymbol(functionDecl.Name) {
		e.addError("Dumplicate symbol: "+functionDecl.Name, functionDecl)
	} else {
		currentScope.Enter(functionDecl.Name, sym)
	}

	lastFunctionSym := e.functionSym
	e.functionSym = sym

	oldScope := currentScope
	e.scope = NewScope(oldScope)
	functionDecl.scope = e.scope

	e.AstVisitor.VisitFunctionDecl(functionDecl, additional)

	e.functionSym = lastFunctionSym
	e.scope = oldScope
	return nil
}

func (e *Enter) VisitBlock(block *Block, additional any) any {
	oldScope := e.scope
	e.scope = NewScope(e.scope)
	block.scope = e.scope

	e.AstVisitor.VisitBlock(block, additional)

	e.scope = oldScope

	return nil
}

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

// ---------------

type RefResolver struct {
	SemanticAstVisitor
	scope           *Scope
	declaredVarsMap map[*Scope]map[string]*VarSymbol
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
	oldScope := e.scope
	e.scope = functionDecl.scope
	if e.scope == nil {
		e.addError("scope 不可为null", functionDecl)
	}
	e.declaredVarsMap[e.scope] = map[string]*VarSymbol{}
	e.AstVisitor.VisitFunctionDecl(functionDecl, additional)
	e.scope = oldScope
	return nil
}

func (e *RefResolver) VisitBlock(block *Block, additional any) any {
	oldScope := e.scope
	e.scope = block.scope

	e.declaredVarsMap[e.scope] = map[string]*VarSymbol{}
	e.AstVisitor.VisitBlock(block, additional)

	e.scope = oldScope
	return nil
}

func (e *RefResolver) VisitFunctionCall(functionCall *FunctionCall, additional any) any {
	currentScope := e.scope

	if _, ok := Built_ins[functionCall.Name]; ok {
		functionCall.Sym = Built_ins[functionCall.Name]
	} else {
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
func (e *RefResolver) VisitVariable(variable *Variable, additional any) any {
	currentScope := e.scope
	variable.Sym = e.findVariableCascade(currentScope, variable)

	return nil
}
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

type LeftValueAttributor struct {
	SemanticAstVisitor
	ParentOperator scanner.OP
}

func NewLeftValueAttributor() *LeftValueAttributor {
	r := &LeftValueAttributor{
		SemanticAstVisitor: SemanticAstVisitor{AstVisitor: AstVisitor{}},
	}
	r.SemanticAstVisitor.AstVisitor.Curr = r
	return r
}
func (lv *LeftValueAttributor) VisitBinary(exp *Binary, additional any) any {
	if scanner.IsAssignOp(exp.Op) || exp.Op == scanner.Dot {
		lastParentOperator := lv.ParentOperator
		lv.ParentOperator = exp.Op
		lv.Visit(exp.Exp1, additional)
		if !exp.Exp1.IsLeftValue() {
			lv.addError(fmt.Sprintf("Left child of operator %s need a left value", exp.Op.ToString()), exp.Exp1)
		}
		lv.ParentOperator = lastParentOperator

		lv.AstVisitor.Visit(exp.Exp2, additional)
	} else {
		lv.AstVisitor.VisitBinary(exp, additional)
	}

	return nil
}

func (lv *LeftValueAttributor) VisitVariable(variable *Variable, additional any) any {
	if lv.ParentOperator != 0 {
		t := variable.TheType
		if !t.hasVoid() {

			variable.isLeftValue = true
		}
	}
	return nil
}

func (lv *LeftValueAttributor) VisitFunctionCall(functionCall *FunctionCall, additional any) any {
	if lv.ParentOperator == scanner.Dot {
		functionType := functionCall.TheType.(*FunctionType)
		if !functionType.ReturnType.hasVoid() {
			functionCall.isLeftValue = true
		}
	}
	return nil
}

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
		if !t2.LE(t1) {
			t.addError("Operator '=' can not be applied to '"+t1.(Type).Name+"' and '"+t2.(Type).Name+"'.", variableDecl)
		}

		if t1 == Any {
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
		if !t2.LE(t1) {
			t.addError(fmt.Sprintf("operator '%s' can not be applied to %s and %s.\n", exp.Op.ToString(), t1.(Type).Name, t2.(Type).Name), exp)
		}
	} else if exp.Op == scanner.Plus {
		if t1 == String || t2 == String {
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
		if t1.LE(Boolean) && t2.LE(Boolean) {
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
		variable.TheType = variable.Sym.TheType
	}
	return nil
}
func (tc *TypeChecker) VisitFunctionCall(functionCall *FunctionCall, additional any) any {
	if functionCall.Sym != nil {
		functionType := functionCall.Sym.TheType.(*FunctionType)
		functionCall.TheType = functionType.ReturnType

		if len(functionCall.Arguments) != len(functionType.ParamTypes) {
			tc.addError(fmt.Sprintf("Function Call of %s has %d arguments, while expecting %d.", functionCall.Name, len(functionCall.Arguments), len(functionType.ParamTypes)), functionCall)
		}

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
		if t1 == String && t2 != String {
			if t2 == Integer {
				tmp := NewFunctionCall(exp.Exp2.(*Expression).beginPos, exp.Exp2.(*Expression).endPos, "integer_to_string", []IExpression{exp.Exp2}, false)
				tmp.Sym = Built_ins["integer_to_string"]
				exp.Exp2 = tmp
			}
		}
	} else if exp.Op == scanner.Plus {
		if t1 == String || t2 == String {
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
