package main

import (
	"fmt"
	"log"
)

// 第一节 精简语言版本
// 内容：递归下降，引用消解，遍历AST执行

func main() {
	tokenizer := NewTokenizer(tokenArray)
	fmt.Println("程序使用的Token:")
	for _, token := range tokenArray {
		fmt.Printf("{kind: %d, text: '%s'}\n", token.kind, token.text)
	}

	prog := NewParser(tokenizer).parseProg()
	fmt.Println("语法分析后的AST")
	prog.dump("")

	NewRefResolver().visitProg(prog)
	fmt.Println("语义分析后的AST，注意自定义函数的调用已被消解:")
	prog.dump("")

	fmt.Println("运行当前的程序:")
	retVal := NewIntepretor().visitProg(prog)
	fmt.Printf("程序返回值：%d", retVal)

}

// Token的类型
type TokenKind int

const (
	Keyword TokenKind = iota
	Identifier
	StringLiteral
	Separator
	Operator
	EOF
)

// Token 词法单元
type Token struct {
	kind TokenKind
	text string
}

// 一个Token数组，代表词法分析的结果
var tokenArray = []Token{
	{kind: Keyword, text: "function"},
	{kind: Identifier, text: "sayHello"},
	{kind: Separator, text: "("},
	{kind: Separator, text: ")"},
	{kind: Separator, text: "{"},
	{kind: Identifier, text: "println"},
	{kind: Separator, text: "("},
	{kind: StringLiteral, text: "Hello, World!"},
	{kind: Separator, text: ")"},
	{kind: Separator, text: ";"},
	{kind: Separator, text: "}"},
	{kind: Identifier, text: "sayHello"},
	{kind: Separator, text: "("},
	{kind: Separator, text: ")"},
	{kind: Separator, text: ";"},
	{kind: EOF, text: ""},
}

// 简化的词法分析器
type Tokenizer struct {
	tokens []Token
	pos    int // 当前token的位置
}

func NewTokenizer(tokens []Token) *Tokenizer {
	return &Tokenizer{tokens: tokens}
}

func (t *Tokenizer) Next() Token {
	if t.pos < len(t.tokens)-1 {
		curpos := t.pos
		t.pos++
		return t.tokens[curpos]
	} else {
		// 到了末尾，总是返回EOF
		return t.tokens[t.pos]
	}
}
func (t *Tokenizer) Position() int {
	return t.pos
}

// 把指针回溯到上一个位置
func (t *Tokenizer) TraceBack(newPos int) {
	t.pos = newPos
}

// 语法分析器，ts中这里是一个抽象类，我们用接口代替
type AstNode interface {
	dump(prefix string)
}

// 语句，包括函数声明和函数调用
// 在ts中，这里有一个静态方法isStatementNode()，因为这里有断言，可以省略
type Statement interface {
	AstNode
}

type Prog struct {
	stmts []Statement
}

func NewProg(stmts []Statement) *Prog {
	return &Prog{stmts: stmts}
}
func (p *Prog) dump(prefix string) {
	fmt.Println(prefix + "Prog")
	for _, stmt := range p.stmts {
		stmt.dump(prefix + "\t")
	}
}

type FunctionDecl struct {
	name string
	body *FunctionBody
}

func NewFunctionDecl(name string, body *FunctionBody) *FunctionDecl {
	return &FunctionDecl{name: name, body: body}
}
func (f *FunctionDecl) dump(prefix string) {
	fmt.Println(prefix + "FunctionDecl " + f.name)
	f.body.dump(prefix + "\t")
}

type FunctionBody struct {
	stmts []*FunctionCall
}

func NewFunctionBody(stmts []*FunctionCall) *FunctionBody {
	return &FunctionBody{stmts: stmts}
}
func (f *FunctionBody) dump(prefix string) {
	fmt.Println(prefix + "FunctionBody")
	for _, stmt := range f.stmts {
		stmt.dump(prefix + "\t")
	}
}

type FunctionCall struct {
	name       string
	parameters []string
	definition *FunctionDecl
}

func NewFunctionCall(name string, parameters []string) *FunctionCall {
	return &FunctionCall{name: name, parameters: parameters, definition: nil}
}

func (f *FunctionCall) dump(prefix string) {
	fmt.Print(prefix + "FunctionCall " + f.name)
	if f.definition != nil {
		fmt.Print(", resolved")
	} else {
		fmt.Print(", not resolved")
	}
	fmt.Println()
	for _, param := range f.parameters {
		fmt.Println(prefix + "\t" + "Parameter: " + param)
	}
}

type Parser struct {
	tokenizer *Tokenizer
}

func NewParser(tokenizer *Tokenizer) *Parser {
	return &Parser{tokenizer: tokenizer}
}
func (p *Parser) parseProg() *Prog {
	stmts := []Statement{}
	for {
		stmt, ok := p.parseFunctionDecl()
		if ok {
			stmts = append(stmts, stmt)
			continue
		}

		stmt, ok = p.parseFunctionCall()
		if ok {
			stmts = append(stmts, stmt)
			continue
		}

		break
	}
	return NewProg(stmts)
}

func (p *Parser) parseFunctionDecl() (Statement, bool) {
	oldPos := p.tokenizer.Position()
	t := p.tokenizer.Next()
	if t.kind == Keyword && t.text == "function" {
		t = p.tokenizer.Next()
		if t.kind == Identifier {
			t1 := p.tokenizer.Next()
			if t1.text == "(" {
				t2 := p.tokenizer.Next()
				if t2.text == ")" {
					functionBody, ok := p.parseFunctionBody()
					if ok {
						return NewFunctionDecl(t.text, functionBody), true
					}
				} else {
					log.Print("Expecting ')' in FunctionDecl, while we got a " + t2.text)
					return nil, false
				}
			} else {
				log.Print("Expecting '(' in FunctionDecl, while we got a " + t.text)
				return nil, false
			}
		}
	}

	p.tokenizer.TraceBack(oldPos)
	return nil, false
}
func (p *Parser) parseFunctionCall() (Statement, bool) {
	oldPos := p.tokenizer.Position()
	params := []string{}
	t := p.tokenizer.Next()
	if t.kind == Identifier {
		t1 := p.tokenizer.Next()
		if t1.text == "(" {
			t2 := p.tokenizer.Next()
			for t2.text != ")" {
				if t2.kind == StringLiteral {
					params = append(params, t2.text)
				} else {
					log.Print("Expecting parameter in FunctionCall, while we got a " + t2.text)
					return nil, false
				}
				t2 = p.tokenizer.Next()
				if t2.text != ")" {
					if t2.text == "," {
						t2 = p.tokenizer.Next()
					} else {
						log.Print("Expecting a comma in FunctionCall, while we got a " + t2.text)
						return nil, false
					}
				}
			}
		}
		t2 := p.tokenizer.Next()
		if t2.text == ";" {
			return NewFunctionCall(t.text, params), true
		} else {
			log.Print("Expecting a comma in FunctionCall, while we got a ", t2.text)
			return nil, false
		}
	}

	p.tokenizer.TraceBack(oldPos)
	return nil, false
}

func (p *Parser) parseFunctionBody() (*FunctionBody, bool) {
	oldPos := p.tokenizer.Position()
	stmts := []*FunctionCall{}
	t := p.tokenizer.Next()
	if t.text == "{" {
		functionCall, ok := p.parseFunctionCall()
		for ok {
			stmts = append(stmts, functionCall.(*FunctionCall))
			functionCall, ok = p.parseFunctionCall()
		}
		t = p.tokenizer.Next()
		if t.text == "}" {
			return NewFunctionBody(stmts), true
		} else {
			log.Print("expecting '}' in FunctionBody, while we got a " + t.text)
		}
	} else {
		log.Print("Expecting '{' in FunctionBody, while we got a " + t.text)
	}

	p.tokenizer.TraceBack(oldPos)
	return &FunctionBody{}, false
}

// 语义分析
type AstVisitor struct{}

func (v *AstVisitor) visitProg(prog Prog) interface{} {
	var retVal interface{}
	for _, x := range prog.stmts {
		if d, ok := x.(*FunctionDecl); ok {
			retVal = v.visitFunctionDecl(d)
		} else {
			d := x.(*FunctionCall)
			retVal = v.visitFunctionCall(d)
		}
	}
	return retVal
}

func (v *AstVisitor) visitFunctionDecl(functionDecl *FunctionDecl) any {
	return v.visitFunctionBody(functionDecl.body)
}

func (v *AstVisitor) visitFunctionBody(functionBody *FunctionBody) any {
	var retVal any
	for _, x := range functionBody.stmts {
		retVal = v.visitFunctionCall(x)
	}
	return retVal
}
func (v *AstVisitor) visitFunctionCall(*FunctionCall) any {
	return nil
}

type RefResolver struct {
	AstVisitor
	prog *Prog
}

func NewRefResolver() *RefResolver {
	return &RefResolver{}
}

func (ref *RefResolver) visitProg(prog *Prog) {
	ref.prog = prog
	for _, x := range prog.stmts {
		funtionCall, ok := x.(*FunctionCall)
		if ok {
			ref.resolveFunctionCall(prog, funtionCall)
		} else {
			if d, ok := x.(*FunctionDecl); ok {
				ref.visitFunctionDecl(d)
			}
		}
	}
}

func (ref *RefResolver) visitFunctionDecl(functionDecl *FunctionDecl) {
	ref.visitFunctionBody(functionDecl.body)
}

func (ref *RefResolver) visitFunctionBody(functionBody *FunctionBody) {
	for _, x := range functionBody.stmts {
		ref.resolveFunctionCall(ref.prog, x)
	}
}
func (ref *RefResolver) resolveFunctionCall(prog *Prog, functionCall *FunctionCall) {
	functionDecl, ok := ref.findFunctionDecl(prog, functionCall.name)
	if ok {
		functionCall.definition = functionDecl
	} else {
		if functionCall.name != "println" {
			log.Print("Error: can not find definition of function " + functionCall.name)
		}
	}
}
func (ref *RefResolver) findFunctionDecl(prog *Prog, name string) (*FunctionDecl, bool) {
	for _, x := range prog.stmts {
		if functionDecl, ok := x.(*FunctionDecl); ok && functionDecl.name == name {
			return functionDecl, true
		}
	}

	return &FunctionDecl{}, false
}

// 解释器
type Intepretor struct {
	AstVisitor
}

func NewIntepretor() *Intepretor {
	return &Intepretor{}
}

func (i *Intepretor) visitProg(prog *Prog) int {
	var retVal int

	for _, x := range prog.stmts {
		if functionCall, ok := x.(*FunctionCall); ok {
			retVal = i.runFunction(functionCall)
		}
	}
	return retVal
}

func (i *Intepretor) visitFunctionBody(functionBody *FunctionBody) {
	for _, x := range functionBody.stmts {
		i.runFunction(x)
	}
}

func (i *Intepretor) runFunction(functionCall *FunctionCall) int {
	if functionCall.name == "println" {
		if len(functionCall.parameters) > 0 {
			fmt.Println(functionCall.parameters[0])
		} else {
			fmt.Println()
		}
	} else {
		if functionCall.definition != nil {
			i.visitFunctionBody(functionCall.definition.body)
		}
	}
	return 0
}
