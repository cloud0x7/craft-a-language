package main

import (
	"craft-language/ast"
	"craft-language/parser"
	"craft-language/scanner"
	"craft-language/semantic"
	"fmt"
	"log"
	"os"
)

func main() {
	// 传递文件名作参数
	params := os.Args[1:]
	if len(params) <= 0 {
		fmt.Println("Usage: go run main.go FILENAME")
		return
	}
	// 读取文件内容
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
	// 打印所有token
	for tokenizer.Peek().Kind != scanner.EOF {
		fmt.Printf("%+v\n", tokenizer.Next())
	}
	// 重新定位到第一个token
	tokenizer = scanner.NewScanner(scanner.NewCharStream(string(program)))

	// 语法分析，prog是抽象语法树AST
	fmt.Println("语法分析后的AST:")
	prog := parser.NewParser(tokenizer).ParseProg()
	prog.Dump("")

	// 语义分析，新增了符号表
	symTable := semantic.NewSymTable()
	enter := semantic.NewEnter(symTable)
	enter.Visit(prog)
	// 打印符号表
	fmt.Printf("symTable: %+v\n", symTable)
	// 引用消解
	refResolver := semantic.NewRefResolver(symTable)
	refResolver.Visit(prog)

	fmt.Println("语义分析后的AST，注意变量和函数已被消解:")
	prog.Dump("")
	// 运行程序，通过访问者模式
	fmt.Println("运行当前的程序:")
	intp := NewIntepretor()
	retVal := intp.Visit(prog)

	fmt.Printf("程序返回值:%d\n", retVal)
}

// ///////////////////////////////////////////////////////////////////////
// 解释器
type Interpretor struct {
	ast.AstVisitor
	values map[string]any // 存储变量值
}

func NewIntepretor() Interpretor {
	return Interpretor{values: map[string]any{}}
}
func (intp *Interpretor) Visit(node ast.AstNode) any {
	return node.Accept(intp)
}
func (intp *Interpretor) VisitProg(prog *ast.Prog) any {
	var retVal any
	for _, x := range prog.Stmts {
		retVal = intp.Visit(x)
	}
	return retVal
}

// 函数声明不做任何事情
func (intp *Interpretor) VisitFunctionDecl(functionDecl *ast.FunctionDecl) any {
	return nil
}

// 运行函数调用
func (intp *Interpretor) VisitFunctionCall(functionCall *ast.FunctionCall) any {
	if functionCall.Name == "println" {
		if len(functionCall.Parameters) > 0 { // 内置函数
			retVal := intp.Visit(functionCall.Parameters[0])
			if v, ok := retVal.(*LeftValue); ok {
				retVal = intp.getVariableValue(v.variable.Name)
			}
			fmt.Println(retVal)
			return 0
		}
	} else {
		if functionCall.Decl != nil { // 找到函数定义，继续遍历函数体
			intp.VisitBlock(functionCall.Decl.Body)
		}
	}
	return 0
}
func (intp *Interpretor) VisitBlock(block *ast.Block) any {
	var retVal any
	for _, x := range block.Stmts {
		retVal = intp.Visit(x)
	}
	return retVal
}

// 如果存在变量初始化部分，要存下变量值
func (intp *Interpretor) VisitVariableDecl(variableDecl *ast.VariableDecl) any {
	if variableDecl.Init != nil {
		v := intp.Visit(variableDecl.Init)
		if intp.isLeftValue(v) { // 是左值，再查一下变量值
			v = intp.getVariableValue(v.(LeftValue).variable.Name)
		}
		intp.setVariableValue(variableDecl.Name, v)
		return v
	}
	return nil
}

// 访问变量
func (intp *Interpretor) VisitVariable(v *ast.Variable) any {
	return NewLeftValue(v)
}
func (intp *Interpretor) VisitExpressionStatement(stmt *ast.ExpressionStatement) any {
	return intp.Visit(stmt.Exp)
}

// 获取变量值
func (intp *Interpretor) getVariableValue(varName string) any {
	ret := intp.values[varName]
	return ret
}

// 设置变量值
func (intp *Interpretor) setVariableValue(varName string, value any) any {
	intp.values[varName] = value
	return nil
}

// 判断是否为左值
func (intp *Interpretor) isLeftValue(v any) bool {
	_, ok := v.(*LeftValue)
	return ok
}
func (intp *Interpretor) VisitBinary(bi *ast.Binary) (ret any) {
	var v1left any

	v1 := intp.Visit(bi.Exp1)
	v2 := intp.Visit(bi.Exp2)

	var v1ok bool
	if intp.isLeftValue(v1) {
		v1left, v1ok = v1.(*LeftValue)
		v1 = intp.getVariableValue(v1left.(*LeftValue).variable.Name)
	}
	if intp.isLeftValue(v2) {
		v2left := v2.(*LeftValue)
		v2 = intp.getVariableValue(v2left.variable.Name)
	}

	switch bi.Op {
	case "+":
		ret = v1.(int) + v2.(int)
	case "-":
		ret = v1.(int) - v2.(int)
	case "*":
		ret = v1.(int) * v2.(int)
	case "/":
		ret = v1.(int) / v2.(int)
	case "%":
		ret = v1.(int) % v2.(int)
	case ">":
		ret = v1.(int) > v2.(int)
	case ">=":
		ret = v1.(int) >= v2.(int)
	case "=":
		if v1ok {
			intp.setVariableValue(v1left.(*LeftValue).variable.Name, v2)
		}
	default:
		log.Print("Unsupported binary operation: " + bi.Op)
	}

	return
}

type LeftValue struct {
	variable *ast.Variable
}

func NewLeftValue(variable *ast.Variable) *LeftValue {
	return &LeftValue{variable: variable}
}
