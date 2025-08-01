package main

import (
	"craft-language/parser"
	"craft-language/scanner"
	"fmt"
	"os"
	"reflect"
	"strconv"
)

func main() {
	params := os.Args[1:]
	if len(params) <= 0 {
		fmt.Println("Usage: go run main.go FILENAME")
		return
	}

	program, err := os.ReadFile(params[0])
	if err != nil {
		fmt.Println(err)
		return
	}

	fmt.Println("源代码:")
	fmt.Println(string(program))

	// 词法分析
	fmt.Println("词法分析结果:")
	tokenizer := scanner.NewScanner(scanner.NewCharStream(string(program)))

	for tokenizer.Peek().Kind != scanner.EOF {
		fmt.Printf("%+v\n", tokenizer.Next().ToString())
	}

	sysScanner := scanner.NewScanner(scanner.NewCharStream(string(program)))

	// 语法分析
	fmt.Println("语法分析后的AST:")
	sysParse := parser.NewParser(sysScanner)
	prog := sysParse.ParseProg()

	astDumper := parser.NewAstDumper()
	astDumper.Visit(prog, "")

	semanticAnalyer := parser.NewSemanticAnalyer()
	semanticAnalyer.Execute(prog)

	fmt.Println("符号表：")
	parser.NewScopeDumper().Visit(prog, "")

	fmt.Println("")
	fmt.Println("语义分析后的AST，注意变量和函数已被消解:")
	astDumper.Visit(prog, "")

	fmt.Println("运行当前的程序:")
	intp := NewIntepretor()
	retVal := intp.Visit(prog, "")

	// fmt.Printf("intp %T, %+v\n", intp, intp)
	fmt.Printf("程序返回值:%v\n", retVal)
}

type Interpretor struct {
	parser.AstVisitor
	callStack    []*StackFrame
	currentFrame *StackFrame
}

func NewIntepretor() *Interpretor {
	currFrame := &StackFrame{
		values: make(map[any]any, 0),
	}
	r := &Interpretor{
		callStack:    make([]*StackFrame, 0),
		currentFrame: currFrame,
		AstVisitor:   parser.AstVisitor{},
	}
	r.callStack = append(r.callStack, currFrame)
	r.AstVisitor.Curr = r

	return r
}
func (i *Interpretor) pushFrame(frame *StackFrame) {
	i.callStack = append(i.callStack, frame)
	i.currentFrame = frame
}
func (i *Interpretor) popFrame() {
	if len(i.callStack) > 1 {
		frame := i.callStack[len(i.callStack)-2]
		i.callStack = i.callStack[:len(i.callStack)-1]
		i.currentFrame = frame
	}
}

func (i *Interpretor) VisitFunctionDecl(functionDecl *parser.FunctionDecl, additional any) any {
	return nil
}
func (i *Interpretor) VisitBlock(block *parser.Block, additional any) any {
	var retVal any
	for _, x := range block.Stmts {
		retVal = i.Visit(x, additional)
		if isReturnValue(retVal) {
			return retVal
		}
	}
	return retVal
}

func (i *Interpretor) VisitReturnStatement(stmt *parser.ReturnStatement, additional any) any {
	var retVal any
	if stmt.Exp != nil {
		retVal = i.Visit(stmt.Exp, additional)
		i.setReturnValue(retVal)
	}
	return NewReturnValue(retVal)
}

func (i *Interpretor) setReturnValue(retVal any) {
	frame := i.callStack[len(i.callStack)-2]
	frame.retVal = retVal
}

func (i *Interpretor) VisitIfStatement(ifStmt *parser.IfStatement, additional any) any {
	conditionValue := i.Visit(ifStmt.Condition, additional)
	if conditionValue.(bool) {
		return i.Visit(ifStmt.Stmt, additional)
	} else if ifStmt.ElseStmt != nil {
		return i.Visit(ifStmt.ElseStmt, additional)
	}
	return nil
}

func (i *Interpretor) VisitForStatement(forStmt *parser.ForStatement, additional any) any {
	if forStmt.Init != nil {
		i.Visit(forStmt.Init, additional)
	}

	notTerminate := true
	if forStmt.Condition != nil {
		notTerminate = i.Visit(forStmt.Condition, additional).(bool)
	}
	for notTerminate {
		retVal := i.Visit(forStmt.Stmt, additional)
		if isReturnValue(retVal) {
			return retVal
		}

		if forStmt.Increment != nil {
			i.Visit(forStmt.Increment, additional)
		}

		notTerminate = true
		if forStmt.Condition != nil {
			notTerminate = i.Visit(forStmt.Condition, additional).(bool)
		}
	}
	return nil
}
func (i *Interpretor) VisitFunctionCall(functionCall *parser.FunctionCall, additional any) any {
	if functionCall.Name == "println" {
		return i.println(functionCall.Arguments)
	} else if functionCall.Name == "tick" {
		return i.tick()
	} else if functionCall.Name == "integer_to_string" {
		return i.integer_to_string(functionCall.Arguments)
	}

	if functionCall.Sym != nil {
		i.currentFrame.retVal = nil

		frame := NewStackFrame()
		functionDecl := functionCall.Sym.Decl
		if functionDecl.CallSignature.ParamList.Params != nil {
			params := functionDecl.CallSignature.ParamList.Params
			for j := 0; j < len(params); j++ {
				variableDecl := params[j]
				val := i.Visit(functionCall.Arguments[j], additional)
				frame.values[variableDecl.Sym] = val
			}
		}

		i.pushFrame(frame)
		i.Visit(functionDecl.Body, additional)
		i.popFrame()

		return i.currentFrame.retVal
	} else {
		panic("Runtime error, cannot find declaration of " + functionCall.Name + ".")
	}
}

func (i *Interpretor) println(args []parser.IExpression) any {

	if len(args) > 0 {
		retVal := i.Visit(args[0], nil)
		fmt.Println(retVal)
	} else {
		fmt.Println()
	}
	return 0
}

func (i *Interpretor) tick() any {
	return nil
}
func (i *Interpretor) integer_to_string(args []parser.IExpression) string {
	if len(args) > 0 {
		arg := i.Visit(args[0], nil)
		if v, ok := arg.(string); ok {
			return v
		} else if v, ok := arg.(int); ok {
			return strconv.Itoa(v)
		} else if v, ok := arg.(float64); ok {
			return fmt.Sprintf("%.2f", v)
		}
	}
	return ""
}

func (i *Interpretor) VisitVariableDecl(variableDecl *parser.VariableDecl, additional any) any {
	if variableDecl.Init != nil {

		v := i.Visit(variableDecl.Init, additional)
		i.setVariableValue(variableDecl.Sym, v)
		return v
	}
	return nil
}

func (i *Interpretor) VisitVariable(variable *parser.Variable, additional any) any {
	if variable.IsLeftValue() {
		return variable.Sym
	} else {
		return i.getVariableValue(variable.Sym)
	}
}

func (i *Interpretor) getVariableValue(sym any) any {
	return i.currentFrame.values[sym]
}
func (i *Interpretor) setVariableValue(sym any, value any) {
	i.currentFrame.values[sym] = value
}

func (i *Interpretor) VisitBinary(exp *parser.Binary, additional any) any {
	var ret any
	v1 := i.Visit(exp.Exp1, additional)
	v2 := i.Visit(exp.Exp2, additional)

	vo1 := reflect.ValueOf(v1)
	vo2 := reflect.ValueOf(v2)

	var fa, fb float64
	if isNumber(vo1) && isNumber(vo2) {
		fa = toFloat(vo1)
		fb = toFloat(vo2)
	}

	switch exp.Op {
	case scanner.Plus:
		if vo1.Kind() == reflect.String || vo2.Kind() == reflect.String {
			ret = fmt.Sprintf("%v%v", v1, v2)
		} else if vo1.Kind() == reflect.Int && vo2.Kind() == reflect.Int {
			ret = v1.(int) + v2.(int)
		} else {
			ret = fa + fb
		}
	case scanner.Minus:
		ret = fa - fb
	case scanner.Multiply:
		ret = fa * fb
	case scanner.Divide:
		ret = v1.(int) / v2.(int)
	case scanner.Modulus:
		ret = v1.(int) % v2.(int)
	case scanner.G:
		ret = v1.(int) > v2.(int)
	case scanner.GE:
		ret = v1.(int) >= v2.(int)
	case scanner.L:
		ret = v1.(int) < v2.(int)
	case scanner.LE:
		ret = v1.(int) <= v2.(int)
	case scanner.EQ:
		ret = v1.(int) == v2.(int)
	case scanner.NE:
		ret = v1.(int) != v2.(int)
	case scanner.And:
		// ret = v1.(int) && v2.(int)
	case scanner.Or:
		// ret = v1.(int) || v2.(int)
	case scanner.Assign:
		varSymbol := v1
		i.setVariableValue(varSymbol, v2)
	default:
		fmt.Printf("Unsupported binary operation: %s\n" + exp.Op.ToString())
	}

	return ret
}

func (i *Interpretor) VisitUnary(exp *parser.Unary, additional any) any {
	v := i.Visit(exp.Exp, additional)
	var value any

	switch exp.Op {
	case scanner.Inc:
		value = i.getVariableValue(v)
		i.setVariableValue(v, value.(int)+1)
		if exp.IsPrefix {
			return value.(int) + 1
		} else {
			return value
		}
	case scanner.Dec:
		value = i.getVariableValue(v)
		i.setVariableValue(v, value.(int)-1)
		if exp.IsPrefix {
			return value.(int) - 1
		} else {
			return value
		}
	case scanner.Plus:
		return v.(int)
	case scanner.Minus:
		return -v.(int)
	default:
		fmt.Printf("unsupported unary op: %s\n", exp.Op.ToString())
	}

	return nil
}

// 栈帧
type StackFrame struct {
	values map[any]any
	retVal any
}

func NewStackFrame() *StackFrame {
	return &StackFrame{
		values: make(map[any]any, 0),
	}
}

type ReturnValue struct {
	tag_ReturnValue int
	value           any
}

func NewReturnValue(value any) *ReturnValue {
	return &ReturnValue{
		tag_ReturnValue: 0,
		value:           value,
	}
}
func isReturnValue(v any) bool {
	_, ok := v.(ReturnValue)
	return ok
}

func toFloat(v reflect.Value) float64 {
	switch v.Kind() {
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		return float64(v.Int())
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
		return float64(v.Uint())
	case reflect.Float32, reflect.Float64:
		return v.Float()
	default:
		return 0
	}
}
func isNumber(v reflect.Value) bool {
	switch v.Kind() {
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64,
		reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64,
		reflect.Float32, reflect.Float64:
		return true
	default:
		return false
	}
}
