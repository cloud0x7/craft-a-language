package main

import (
	"fmt"
	"log"
	"os"
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
	tokenizer := NewTokenizer(NewCharStream(string(program)))

	for tokenizer.peek().kind != EOF {
		fmt.Printf("%+v\n", tokenizer.next())
	}

	tokenizer = NewTokenizer(NewCharStream(string(program)))
	// 语法分析
	prog := NewParser(tokenizer).parseProg()
	fmt.Println("语法分析后的AST:")
	prog.dump("")

	NewRefResolver().visitProg(prog)
	fmt.Println("语法分析后的AST，注意自定义函数的调用已被消解:")
	prog.dump("")

	fmt.Println("运行当前的程序:")
	retVal := NewIntepretor().visitProg(prog)
	fmt.Printf("程序返回值:%d\n", retVal)
}

type TokenKind int

const (
	Keyword TokenKind = iota
	Identifier
	StringLiteral
	Separator
	Operator
	EOF
)

type Token struct {
	kind TokenKind
	text string
}

type CharStream struct {
	data string
	pos  uint
	line uint
	col  uint
}

func NewCharStream(data string) *CharStream {
	return &CharStream{data: data}
}
func (cs *CharStream) peek() string {
	if int(cs.pos) < len(cs.data) {
		return string(cs.data[cs.pos])
	}
	return ""
}
func (cs *CharStream) next() string {
	ch := cs.data[cs.pos]
	cs.pos++
	if ch == '\n' {
		cs.line++
		cs.col = 0
	} else {
		cs.col++
	}
	return string(ch)
}
func (cs *CharStream) eof() bool {
	return cs.peek() == ""
}

type Tokenizer struct {
	stream    *CharStream
	nextToken Token
}

func NewTokenizer(stream *CharStream) *Tokenizer {
	return &Tokenizer{
		stream:    stream,
		nextToken: Token{kind: EOF, text: ""},
	}
}
func (t *Tokenizer) next() Token {
	if t.nextToken.kind == EOF && !t.stream.eof() {
		t.nextToken = t.getAToken()
	}
	lastToken := t.nextToken
	t.nextToken = t.getAToken()

	return lastToken
}

func (t *Tokenizer) peek() Token {
	if t.nextToken.kind == EOF && !t.stream.eof() {
		t.nextToken = t.getAToken()
	}

	return t.nextToken
}

func (t *Tokenizer) getAToken() Token {
	t.skipWhiteSpaces()

	if t.stream.eof() {
		return Token{kind: EOF, text: ""}
	} else {
		ch := t.stream.peek()
		if t.isLetter(ch) || t.isDigit(ch) {
			return t.parseIdentifer()
		} else if ch == "\"" {
			return t.parseStringLIteral()
		} else if ch == "(" || ch == ")" || ch == "{" || ch == "}" || ch == ";" || ch == "," {
			t.stream.next()
			return Token{kind: Separator, text: ch}
		} else if ch == "/" {
			t.stream.next()
			ch1 := t.stream.peek()
			if ch1 == "*" {
				t.skipMultipleLineComments()
				return t.getAToken()
			} else if ch1 == "/" {
				t.skipSingleLineComment()
				return t.getAToken()
			} else if ch1 == "=" {
				t.stream.next()
				return Token{kind: Operator, text: "/="}
			} else {
				return Token{kind: Operator, text: "/"}
			}
		} else if ch == "+" {
			t.stream.next()
			ch1 := t.stream.peek()
			if ch1 == "+" {
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
			if ch1 == "-" {
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
func (t *Tokenizer) skipSingleLineComment() {
	t.stream.next()

	for t.stream.peek() != "\n" && !t.stream.eof() {
		t.stream.next()
	}
}
func (t *Tokenizer) skipMultipleLineComments() {
	t.stream.next()

	if !t.stream.eof() {
		ch1 := t.stream.next()
		for !t.stream.eof() {
			ch2 := t.stream.next()
			if ch1 == "*" && ch2 == "/" {
				return
			}
			ch1 = ch2
		}
	}

	log.Printf("Failed to find matching */ for multiple line comments at ': line: %d, col: %d\n", t.stream.line, t.stream.col)
}
func (t *Tokenizer) skipWhiteSpaces() {
	for t.isWhiteSpace(t.stream.peek()) {
		t.stream.next()
	}
}
func (t *Tokenizer) parseStringLIteral() Token {
	token := Token{kind: StringLiteral, text: ""}
	t.stream.next()

	for !t.stream.eof() && t.stream.peek() != "\"" {
		token.text += t.stream.next()
	}

	if t.stream.peek() == "\"" {
		t.stream.next()
	} else {
		log.Printf("Expecting an \" at line: %d col: %d\n", t.stream.line, t.stream.col)
	}

	return token
}
func (t *Tokenizer) parseIdentifer() Token {
	token := Token{kind: Identifier, text: ""}
	token.text += t.stream.next()

	for !t.stream.eof() && t.isLetterDigitOrUnderScore(t.stream.peek()) {
		token.text += t.stream.next()
	}

	if token.text == "function" {
		token.kind = Keyword
	}

	return token
}
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

// 语法分析器
type AstNode interface {
	dump(prefix string)
}
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

func (f FunctionCall) dump(prefix string) {
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
	var stmt Statement
	token := p.tokenizer.peek()
	var ok bool

	for token.kind != EOF {
		if token.kind == Keyword && token.text == "function" {
			stmt, ok = p.parseFunctionDecl()
		} else if token.kind == Identifier {
			stmt, ok = p.parseFunctionCall()
		}

		if ok {
			stmts = append(stmts, stmt)
			log.Println("success")
		}

		token = p.tokenizer.peek()
	}
	return NewProg(stmts)
}

func (p *Parser) parseFunctionDecl() (Statement, bool) {
	fmt.Println("in FunctionDecl")
	p.tokenizer.next()

	t := p.tokenizer.next()
	if t.kind == Identifier {
		t1 := p.tokenizer.next()
		if t1.text == "(" {
			t2 := p.tokenizer.next()
			if t2.text == ")" {
				functionBody, ok := p.parseFunctionBody()
				if ok {
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

func (p *Parser) parseFunctionBody() (*FunctionBody, bool) {
	stmts := []*FunctionCall{}
	t := p.tokenizer.next()

	if t.text == "{" {
		for p.tokenizer.peek().kind == Identifier {
			functionCall, ok := p.parseFunctionCall()
			if ok {
				stmts = append(stmts, functionCall.(*FunctionCall))
			} else {
				log.Print("Error parsing a FunctionCall in FunctionBody.")
				return &FunctionBody{}, false
			}
		}
		t = p.tokenizer.next()
		if t.text == "}" {
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

func (p *Parser) parseFunctionCall() (Statement, bool) {
	params := []string{}
	t := p.tokenizer.next()

	if t.kind == Identifier {
		t1 := p.tokenizer.next()
		if t1.text == "(" {
			t2 := p.tokenizer.next()
			for t2.text != ")" {
				if t2.kind == StringLiteral {
					params = append(params, t2.text)
				} else {
					log.Print("Expecting parameter in FunctionCall, while we got a " + t2.text)
					return nil, false
				}
				t2 = p.tokenizer.next()
				if t2.text != ")" {
					if t2.text == "," {
						t2 = p.tokenizer.next()
					} else {
						log.Print("Expecting a comma in FunctionCall, while we got a " + t2.text)
						return nil, false
					}
				}
			}

			t2 = p.tokenizer.next()
			if t2.text == ";" {
				return NewFunctionCall(t.text, params), true
			} else {
				log.Print("Expecting a comma in FunctionCall, while we got a ", t2.text)
				return nil, false
			}
		}
	}

	return nil, false
}

type AstVisitor struct{}

func (v *AstVisitor) visitProg(prog *Prog) any {
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
	var retVal interface{}
	for _, x := range functionBody.stmts {
		retVal = v.visitFunctionCall(x)
	}
	return retVal
}
func (v *AstVisitor) visitFunctionCall(functionCall *FunctionCall) any {
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
