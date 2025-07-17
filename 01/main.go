package main

import (
	"fmt"
	"log"
)

// 第一节 精简语言版本
// 内容：递归下降，引用消解，遍历AST执行
// 运行：go run main.go

// 语法规则，只能定义函数和调用函数
// prog = (functionDecl | functionCall)* ;
// functionDecl: "function" Identifier "(" ")"  functionBody;
// functionBody : '{' functionCall* '}' ;
// functionCall : Identifier '(' parameterList? ')' ;
// parameterList : StringLiteral (',' StringLiteral)* ;
func main() {
	// 词法分析，这里为了降低难度，直接使用处理好的token数组
	tokenizer := NewTokenizer(tokenArray)
	fmt.Println("程序使用的Token:")
	for _, token := range tokenArray {
		fmt.Printf("{kind: %d, text: '%s'}\n", token.kind, token.text)
	}
	// 语法分析，prog是抽象语法树AST
	prog := NewParser(tokenizer).parseProg()
	fmt.Println("语法分析后的AST")
	prog.dump("")
	// 语义分析，通过访问者模式访问AST
	NewRefResolver().visitProg(prog)
	fmt.Println("语义分析后的AST，注意自定义函数的调用已被消解:")
	prog.dump("")
	// 运行程序，通过访问者模式
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

// 返回下一个token
func (t *Tokenizer) Next() Token {
	// 返回下一个，当前位置+1
	if t.pos < len(t.tokens)-1 {
		curpos := t.pos
		t.pos++
		return t.tokens[curpos]
	} else {
		// 到了末尾，总是返回EOF
		return t.tokens[t.pos]
	}
}

// 返回当前位置
func (t *Tokenizer) Position() int {
	return t.pos
}

// 把指针回溯到上一个位置
func (t *Tokenizer) TraceBack(newPos int) {
	t.pos = newPos
}

// ///////////////////////////////////////////////////////////////////////
// 语法分析
// 节点基类，ts中这里是一个抽象类，我们用接口代替
type AstNode interface {
	//打印对象信息，prefix是前缀填充内容，通常用于缩进
	dump(prefix string)
}

// 语句，包括函数声明和函数调用
// 在ts中，这里有一个静态方法isStatementNode()，因为这里有断言，可以省略
type Statement interface {
	AstNode
}

// 程序节点，AST的根节点
type Prog struct {
	stmts []Statement // 程序中包含多个语句
}

// 构造函数
func NewProg(stmts []Statement) *Prog {
	return &Prog{stmts: stmts}
}
func (p *Prog) dump(prefix string) {
	fmt.Println(prefix + "Prog")
	// 遍历所有语句并打印信息
	for _, stmt := range p.stmts {
		stmt.dump(prefix + "\t")
	}
}

// 函数声明节点
type FunctionDecl struct {
	name string        // 函数名称
	body *FunctionBody // 函数体
}

func NewFunctionDecl(name string, body *FunctionBody) *FunctionDecl {
	return &FunctionDecl{name: name, body: body}
}
func (f *FunctionDecl) dump(prefix string) {
	fmt.Println(prefix + "FunctionDecl " + f.name)
	f.body.dump(prefix + "\t")
}

// 函数体，这个版本只有函数调用
type FunctionBody struct {
	stmts []*FunctionCall
}

func NewFunctionBody(stmts []*FunctionCall) *FunctionBody {
	return &FunctionBody{stmts: stmts}
}
func (f *FunctionBody) dump(prefix string) {
	fmt.Println(prefix + "FunctionBody")
	// 遍历所有语句并打印信息
	for _, stmt := range f.stmts {
		stmt.dump(prefix + "\t")
	}
}

// 函数调用
type FunctionCall struct {
	name       string
	parameters []string      // 参数列表
	definition *FunctionDecl // 指向函数声明
}

func NewFunctionCall(name string, parameters []string) *FunctionCall {
	return &FunctionCall{name: name, parameters: parameters, definition: nil}
}

func (f *FunctionCall) dump(prefix string) {
	fmt.Print(prefix + "FunctionCall " + f.name)
	// 显示函数是否已经解引用了
	if f.definition != nil {
		fmt.Print(", resolved")
	} else {
		fmt.Print(", not resolved")
	}
	fmt.Println()
	for _, param := range f.parameters { // 参数也打印出来
		fmt.Println(prefix + "\t" + "Parameter: " + param)
	}
}

// 解析器
type Parser struct {
	tokenizer *Tokenizer // 包含词法分析器
}

func NewParser(tokenizer *Tokenizer) *Parser {
	return &Parser{tokenizer: tokenizer}
}

// 解析整个程序
// 语法规则：prog = (functionDecl | functionCall)* ;
func (p *Parser) parseProg() *Prog {
	stmts := []Statement{}
	for { // 循环解析函数声明和函数调用
		// 这里猜测为函数声明，并按规则尝试解析，如果成功，则加入语句列表
		stmt, ok := p.parseFunctionDecl()
		if ok {
			stmts = append(stmts, stmt)
			continue
		}
		// 如果函数声明解析失败，这里再尝试按函数调用解析
		stmt, ok = p.parseFunctionCall()
		if ok {
			stmts = append(stmts, stmt) // 成功则加入语句列表
			continue
		}
		// 都没成功，跳出并结束
		break
	}
	// 返回程序根节点
	return NewProg(stmts)
}

// 解析函数声明，使用递归下降
func (p *Parser) parseFunctionDecl() (Statement, bool) {
	oldPos := p.tokenizer.Position() // 保存当前位置
	t := p.tokenizer.Next()
	if t.kind == Keyword && t.text == "function" { //第一个token是'function'
		t = p.tokenizer.Next()
		if t.kind == Identifier { //第二个是标识符
			t1 := p.tokenizer.Next()
			if t1.text == "(" { // 第三个是'('
				t2 := p.tokenizer.Next()
				if t2.text == ")" { // 第四个是')'
					functionBody, ok := p.parseFunctionBody() // 尝试解析函数体
					if ok {                                   // 解析成功，返回函数声明节点
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

	// 解析失败，回溯到旧位置
	p.tokenizer.TraceBack(oldPos)
	return nil, false
}
func (p *Parser) parseFunctionCall() (Statement, bool) {
	oldPos := p.tokenizer.Position() //保存当前位置
	params := []string{}             //参数列表
	t := p.tokenizer.Next()
	if t.kind == Identifier { //匹配标识符
		t1 := p.tokenizer.Next()
		if t1.text == "(" { //匹配左括号
			t2 := p.tokenizer.Next()
			for t2.text != ")" { //只要不是右括号，就循环解析参数
				if t2.kind == StringLiteral { //只接受字符串字面量参数
					params = append(params, t2.text)
				} else {
					log.Print("Expecting parameter in FunctionCall, while we got a " + t2.text)
					return nil, false
				}
				t2 = p.tokenizer.Next()
				if t2.text != ")" {
					if t2.text == "," { //逗号后面是下一个参数
						t2 = p.tokenizer.Next() //取下一个token
					} else {
						log.Print("Expecting a comma in FunctionCall, while we got a " + t2.text)
						return nil, false
					}
				}
			}
		}
		t2 := p.tokenizer.Next()
		if t2.text == ";" { //结束了，返回函数调用
			return NewFunctionCall(t.text, params), true
		} else {
			log.Print("Expecting a comma in FunctionCall, while we got a ", t2.text)
			return nil, false
		}
	}
	// 解析失败，回溯到旧位置
	p.tokenizer.TraceBack(oldPos)
	return nil, false
}

func (p *Parser) parseFunctionBody() (*FunctionBody, bool) {
	oldPos := p.tokenizer.Position() //保存当前位置
	stmts := []*FunctionCall{}       //当前版本只有函数调用语句
	t := p.tokenizer.Next()
	if t.text == "{" { //函数体开始
		functionCall, ok := p.parseFunctionCall() //尝试解析函数调用
		for ok {
			//成功则加入语句列表
			stmts = append(stmts, functionCall.(*FunctionCall))
			//继续尝试解析，失败后跳出循环
			functionCall, ok = p.parseFunctionCall()
		}
		t = p.tokenizer.Next()
		if t.text == "}" { //函数体结束，返回函数体对象
			return NewFunctionBody(stmts), true
		} else {
			log.Print("expecting '}' in FunctionBody, while we got a " + t.text)
		}
	} else {
		log.Print("Expecting '{' in FunctionBody, while we got a " + t.text)
	}
	// 解析失败，回溯到旧位置
	p.tokenizer.TraceBack(oldPos)
	return nil, false
}

// 语义分析，使用了访问者模式，遍历AST上的节点并进行处理
// 这里作为一个抽象基类，实现公共方法
type AstVisitor struct{}

// 访问Program节点
func (v *AstVisitor) visitProg(prog Prog) any {
	var retVal any
	//遍历语句，检查是函数声明还是函数调用，分别处理
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

// 访问函数声明，进入函数体访问
func (v *AstVisitor) visitFunctionDecl(functionDecl *FunctionDecl) any {
	return v.visitFunctionBody(functionDecl.body)
}

// 访问函数体，遍历语句
func (v *AstVisitor) visitFunctionBody(functionBody *FunctionBody) any {
	var retVal any
	for _, x := range functionBody.stmts {
		retVal = v.visitFunctionCall(x)
	}
	return retVal
}

// 访问函数调用，这里不需要处理，由子类补充具体实现
func (v *AstVisitor) visitFunctionCall(*FunctionCall) any {
	return nil
}

// ///////////////////////////////////////////////////////////////////////
// 语义分析
// 处理引用消解
type RefResolver struct {
	AstVisitor // 嵌入基类，相当于继承
	prog       *Prog
}

func NewRefResolver() *RefResolver {
	return &RefResolver{}
}

// 访问程序根节点
func (ref *RefResolver) visitProg(prog *Prog) {
	ref.prog = prog
	// 遍历语句
	for _, x := range prog.stmts {
		funtionCall, ok := x.(*FunctionCall)
		if ok { //如果是函数调用，则处理引用消解
			ref.resolveFunctionCall(prog, funtionCall)
		} else { //是函数声明，则继续遍历节点
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

// 处理引用消解
func (ref *RefResolver) resolveFunctionCall(prog *Prog, functionCall *FunctionCall) {
	functionDecl, ok := ref.findFunctionDecl(prog, functionCall.name)
	if ok { // 如果找到函数声明，保存引用
		functionCall.definition = functionDecl
	} else {
		// 没找到函数声明，如果是`println`则为内置函数，否则报错
		if functionCall.name != "println" {
			log.Print("Error: can not find definition of function " + functionCall.name)
		}
	}
}

// 查找函数声明
func (ref *RefResolver) findFunctionDecl(prog *Prog, name string) (*FunctionDecl, bool) {
	// 遍历每条语句，如果是函数声明，且与查找名称一致，则返回
	for _, x := range prog.stmts {
		if functionDecl, ok := x.(*FunctionDecl); ok && functionDecl.name == name {
			return functionDecl, true
		}
	}

	return nil, false
}

// ///////////////////////////////////////////////////////////////////////
// 解释器
type Intepretor struct {
	AstVisitor // 继承其方法
}

func NewIntepretor() *Intepretor {
	return &Intepretor{}
}

func (i *Intepretor) visitProg(prog *Prog) int {
	var retVal int
	// 遍历所有语句
	for _, x := range prog.stmts {
		//只处理函数调用
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

// 处理函数调用
func (i *Intepretor) runFunction(functionCall *FunctionCall) int {
	if functionCall.name == "println" { // 内置函数，直接打印参数
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
