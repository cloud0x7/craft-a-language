package main

import (
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
	// 打印源码
	fmt.Println("源代码:")
	fmt.Println(string(program))

	// 词法分析
	fmt.Println("词法分析结果:")
	// 这里是一个真实可用的词法分析器
	tokenizer := NewTokenizer(NewCharStream(string(program)))
	// 打印所有token
	for tokenizer.peek().kind != EOF {
		fmt.Printf("%+v\n", tokenizer.next())
	}
	// 重新定位到第一个token
	tokenizer = NewTokenizer(NewCharStream(string(program)))
	// 语法分析，prog是抽象语法树AST
	prog := NewParser(tokenizer).parseProg()
	fmt.Println("语法分析后的AST:")
	prog.dump("")
	// 语义分析，通过访问者模式访问AST
	NewRefResolver().visitProg(prog)
	fmt.Println("语法分析后的AST，注意自定义函数的调用已被消解:")
	prog.dump("")
	// 运行程序，通过访问者模式
	fmt.Println("运行当前的程序:")
	retVal := NewIntepretor().visitProg(prog)
	fmt.Printf("程序返回值:%d\n", retVal)
}

type TokenKind int

// token类型的枚举
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

// 字符串流
type CharStream struct {
	data string // 字符串
	pos  uint   // 指针位置
	line uint   // 行号
	col  uint   // 列号
}

func NewCharStream(data string) *CharStream {
	return &CharStream{data: data} // 初始化，其它属性默认为0
}

// 预读下一个字符，但不移动指针
func (cs *CharStream) peek() string {
	if int(cs.pos) < len(cs.data) {
		return string(cs.data[cs.pos])
	}
	return ""
}

// 读取下一个字符，并移动指针
func (cs *CharStream) next() string {
	ch := cs.data[cs.pos]
	cs.pos++
	if ch == '\n' { // 换行
		cs.line++
		cs.col = 0
	} else {
		cs.col++
	}
	return string(ch)
}

// 判断是否到达结尾
func (cs *CharStream) eof() bool {
	return cs.peek() == ""
}

// 词法分析器
type Tokenizer struct {
	stream    *CharStream // 字符流
	nextToken Token       // 下一个token
}

func NewTokenizer(stream *CharStream) *Tokenizer {
	return &Tokenizer{
		stream:    stream,
		nextToken: Token{kind: EOF, text: ""}, // 初始值为EOF，首次调用next()时得到第一个token
	}
}

// 下一个token
func (t *Tokenizer) next() Token {
	// 首次调用，先解析一个token
	if t.nextToken.kind == EOF && !t.stream.eof() {
		t.nextToken = t.getAToken()
	}
	lastToken := t.nextToken
	// 预处理下一个token
	t.nextToken = t.getAToken()

	return lastToken
}

// 预读一个token，这里不进行预处理
func (t *Tokenizer) peek() Token {
	if t.nextToken.kind == EOF && !t.stream.eof() {
		t.nextToken = t.getAToken()
	}

	return t.nextToken
}

// 解析一个token
func (t *Tokenizer) getAToken() Token {
	t.skipWhiteSpaces() // 跳过空白字符

	if t.stream.eof() { // 到达字符流结尾
		return Token{kind: EOF, text: ""}
	} else {
		ch := t.stream.peek()
		if t.isLetter(ch) || t.isDigit(ch) { // 是字母或数字，则解析标识符
			return t.parseIdentifer()
		} else if ch == "\"" { // 是双引号，则解析字符串字面量
			return t.parseStringLIteral()
		} else if ch == "(" || ch == ")" || ch == "{" || ch == "}" || ch == ";" || ch == "," { // 是分隔符，则返回对应的token
			t.stream.next()
			return Token{kind: Separator, text: ch}
		} else if ch == "/" {
			t.stream.next()
			ch1 := t.stream.peek()
			if ch1 == "*" { // 处理多行注释
				t.skipMultipleLineComments()
				return t.getAToken()
			} else if ch1 == "/" { // 处理单行注释
				t.skipSingleLineComment()
				return t.getAToken()
			} else if ch1 == "=" { // 处理 /=
				t.stream.next()
				return Token{kind: Operator, text: "/="}
			} else {
				return Token{kind: Operator, text: "/"}
			}
		} else if ch == "+" {
			t.stream.next()
			ch1 := t.stream.peek()
			if ch1 == "+" { // ++是一个完整的token
				t.stream.next()
				return Token{kind: Operator, text: "++"}
			} else if ch1 == "=" {
				t.stream.next()
				return Token{kind: Operator, text: "+="}
			} else {
				return Token{kind: Operator, text: "+"}
			}
		} else if ch == "-" {
			t.stream.next()
			ch1 := t.stream.peek()
			if ch1 == "-" { // --是一个完整的token
				t.stream.next()
				return Token{kind: Operator, text: "--"}
			} else if ch1 == "=" {
				t.stream.next()
				return Token{kind: Operator, text: "-="}
			} else {
				return Token{kind: Operator, text: "-"}
			}
		} else if ch == "*" {
			t.stream.next()
			ch1 := t.stream.peek()
			if ch1 == "=" {
				t.stream.next()
				return Token{kind: Operator, text: "*="}
			} else {
				return Token{kind: Operator, text: "*"}
			}
		} else {
			log.Printf("Unrecognized pattern meeting ': %s, at line: %d, col: %d\n", ch, t.stream.line, t.stream.col)
		}
	}

	return Token{}
}

// 跳过单行注释
func (t *Tokenizer) skipSingleLineComment() {
	t.stream.next()
	// 走到换行或字符流结尾停下
	for t.stream.peek() != "\n" && !t.stream.eof() {
		t.stream.next()
	}
}

// 跳过多行注释
func (t *Tokenizer) skipMultipleLineComments() {
	t.stream.next()

	if !t.stream.eof() {
		ch1 := t.stream.next()
		for !t.stream.eof() { // 没到达字符流结尾，就继续循环
			ch2 := t.stream.next()
			if ch1 == "*" && ch2 == "/" { // 找到 "*/"则跳出循环
				return
			}
			ch1 = ch2
		}
	}

	log.Printf("Failed to find matching */ for multiple line comments at ': line: %d, col: %d\n", t.stream.line, t.stream.col)
}

// 跳过空白字符
func (t *Tokenizer) skipWhiteSpaces() {
	for t.isWhiteSpace(t.stream.peek()) {
		t.stream.next()
	}
}

// 解析字符串字面量，当前只支持双引号，不支持转义
func (t *Tokenizer) parseStringLIteral() Token {
	// 生成默认空字符token
	token := Token{kind: StringLiteral, text: ""}
	t.stream.next()
	// 解析到达下一双引号之前所有的字符，加入text属性
	for !t.stream.eof() && t.stream.peek() != "\"" {
		token.text += t.stream.next()
	}

	if t.stream.peek() == "\"" {
		t.stream.next() // 消化掉结尾的引号
	} else {
		log.Printf("Expecting an \" at line: %d col: %d\n", t.stream.line, t.stream.col)
	}

	return token
}

// 解析标识符，挑出关键字
func (t *Tokenizer) parseIdentifer() Token {
	// 生成默认空字符token
	token := Token{kind: Identifier, text: ""}
	token.text += t.stream.next()
	// 符合规则（字母、数字、下划线）就继续添加
	for !t.stream.eof() && t.isLetterDigitOrUnderScore(t.stream.peek()) {
		token.text += t.stream.next()
	}
	// 如果是关键字，则修改kind
	if token.text == "function" {
		token.kind = Keyword
	}

	return token
}

// 标识符的词法规则
func (t Tokenizer) isLetterDigitOrUnderScore(ch string) bool {
	return ch >= "A" && ch <= "Z" ||
		ch >= "a" && ch <= "z" ||
		ch >= "0" && ch <= "9" ||
		ch == "_"
}
func (t Tokenizer) isLetter(ch string) bool {
	return ch >= "A" && ch <= "Z" || ch >= "a" && ch <= "z"
}
func (t Tokenizer) isDigit(ch string) bool {
	return ch >= "0" && ch <= "9"
}
func (t Tokenizer) isWhiteSpace(ch string) bool {
	return (ch == " " || ch == "\n" || ch == "\t")
}

// ///////////////////////////////////////////////////////////////////////
// 语法分析
// 基类，这里使用接口替代
type AstNode interface {
	dump(prefix string)
}

// 语句
type Statement interface {
	AstNode
}

// 程序节点，AST根节点
type Prog struct {
	stmts []Statement // 程序内多个语句
}

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

func (f FunctionCall) dump(prefix string) {
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
	var stmt Statement
	token := p.tokenizer.peek()
	var ok bool

	for token.kind != EOF { // 循环解析函数声明和函数调用
		// !!注意这里不再猜测语句类型尝试解析，而是使用了预读
		// 如果token为function，则直接解析函数声明
		if token.kind == Keyword && token.text == "function" {
			stmt, ok = p.parseFunctionDecl()
		} else if token.kind == Identifier { // 如果token为标识符，则尝试解析函数调用
			stmt, ok = p.parseFunctionCall()
		}

		if ok { // 解析成功，加入语句列表
			stmts = append(stmts, stmt)
			// log.Println("success")
		}

		token = p.tokenizer.peek()
	}
	// 返回程序根节点
	return NewProg(stmts)
}

// 解析函数声明，使用递归下降
// 语法规则：functionDecl: "function" Identifier "(" ")"  functionBody;
func (p *Parser) parseFunctionDecl() (Statement, bool) {
	// !!不再保存当前token的位置，因为不需要回溯了

	// 跳过关键字'function'
	p.tokenizer.next()

	t := p.tokenizer.next()
	if t.kind == Identifier { // 从标识符开始匹配，因为'function'在函数外已经匹配过了
		t1 := p.tokenizer.next()
		// 读取()
		if t1.text == "(" {
			t2 := p.tokenizer.next()
			if t2.text == ")" {
				functionBody, ok := p.parseFunctionBody() // 尝试解析函数体
				if ok {                                   // 解析成功，返回函数声明节点
					return NewFunctionDecl(t.text, functionBody), true
				} else {
					log.Println("Error parsing FunctionBody in FunctionDecl")
					return nil, false
				}
			} else {
				log.Println("Expecting ')' in FunctionDecl, while we got a " + t.text)
				return nil, false
			}
		}
	} else {
		log.Println("Expecting '(' in FunctionDecl, while we got a " + t.text)
		return nil, false
	}
	log.Println("Expecting a function name, while we got a " + t.text)
	return nil, false
}

// 解析函数体
// 语法规则：functionBody : '{' functionCall* '}' ;
func (p *Parser) parseFunctionBody() (*FunctionBody, bool) {
	stmts := []*FunctionCall{} // 函数调用语句列表
	t := p.tokenizer.next()

	if t.text == "{" {
		for p.tokenizer.peek().kind == Identifier {
			functionCall, ok := p.parseFunctionCall()
			if ok { //成功则加入语句列表
				stmts = append(stmts, functionCall)
			} else {
				log.Print("Error parsing a FunctionCall in FunctionBody.")
				return &FunctionBody{}, false
			}
		}
		t = p.tokenizer.next()
		if t.text == "}" { //函数体结束，返回函数体对象
			return NewFunctionBody(stmts), true
		} else {
			log.Printf("Expecting '}' in FunctionBody, while we got a " + t.text)
			return nil, false
		}
	} else {
		log.Printf("Expecting '{' in FunctionBody, while we got a " + t.text)
		return nil, false
	}
}

// 解析函数调用
// 语法规则：functionCall : Identifier '(' parameterList? ')' ;parameterList : StringLiteral (',' StringLiteral)* ;
func (p *Parser) parseFunctionCall() (*FunctionCall, bool) {
	params := []string{} //参数列表
	t := p.tokenizer.next()

	if t.kind == Identifier { //匹配标识符
		t1 := p.tokenizer.next()
		if t1.text == "(" {
			t2 := p.tokenizer.next()
			for t2.text != ")" { //只要不是右括号，就循环解析参数
				if t2.kind == StringLiteral {
					params = append(params, t2.text)
				} else {
					log.Print("Expecting parameter in FunctionCall, while we got a " + t2.text)
					return nil, false
				}
				t2 = p.tokenizer.next()
				if t2.text != ")" {
					if t2.text == "," { //逗号后面是下一个参数
						t2 = p.tokenizer.next()
					} else {
						log.Print("Expecting a comma in FunctionCall, while we got a " + t2.text)
						return nil, false
					}
				}
			}

			t2 = p.tokenizer.next()
			if t2.text == ";" { //结束了，返回函数调用
				return NewFunctionCall(t.text, params), true
			} else {
				log.Print("Expecting a comma in FunctionCall, while we got a ", t2.text)
				return nil, false
			}
		}
	}

	return nil, false
}

// 语义分析，使用了访问者模式，遍历AST上的节点并进行处理
// 这里作为一个抽象基类，实现公共方法
type AstVisitor struct{}

// 访问Program节点
func (v *AstVisitor) visitProg(prog *Prog) any {
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
func (v *AstVisitor) visitFunctionCall(functionCall *FunctionCall) any {
	return nil
}

// ///////////////////////////////////////////////////////////////////////
// 语义分析
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

// 查找函数声明，从根节点开始找
func (ref *RefResolver) findFunctionDecl(prog *Prog, name string) (*FunctionDecl, bool) {
	// 遍历每条语句，如果是函数声明，且与查找名称一致，则返回
	for _, x := range prog.stmts {
		if functionDecl, ok := x.(*FunctionDecl); ok && functionDecl.name == name {
			return functionDecl, true
		}
	}

	return &FunctionDecl{}, false
}

// ///////////////////////////////////////////////////////////////////////
// 解释器
type Intepretor struct {
	AstVisitor
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
