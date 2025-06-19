package parser

import (
	// "craft-language/compiler"

	// "craft-language/mytype"

	"craft-language/scanner"
	myscanner "craft-language/scanner"
	"fmt"
	"slices"
	"strconv"
)

type Parser struct {
	scanner  *scanner.Scanner
	opPrec   map[myscanner.OP]int
	errors   []*CompilerError
	warnings []*CompilerError
}

func NewParser(scanner *scanner.Scanner) *Parser {
	return &Parser{
		scanner: scanner,
		opPrec: map[myscanner.OP]int{
			myscanner.Assign:                     2,
			myscanner.PlusAssign:                 2,
			myscanner.MinusAssign:                2,
			myscanner.MultiplyAssign:             2,
			myscanner.DivideAssign:               2,
			myscanner.ModulusAssign:              2,
			myscanner.BitAndAssign:               2,
			myscanner.BitOrAssign:                2,
			myscanner.BitXorAssign:               2,
			myscanner.LeftShiftArithmeticAssign:  2,
			myscanner.RightShiftArithmeticAssign: 2,
			myscanner.RightShiftLogicalAssign:    2,
			myscanner.Or:                         4,
			myscanner.And:                        5,
			myscanner.BitOr:                      6,
			myscanner.BitXOr:                     7,
			myscanner.BitAnd:                     8,
			myscanner.EQ:                         9,
			myscanner.IdentityEquals:             9,
			myscanner.NE:                         9,
			myscanner.IdentityNotEquals:          9,
			myscanner.G:                          10,
			myscanner.GE:                         10,
			myscanner.L:                          10,
			myscanner.LE:                         10,
			myscanner.LeftShiftArithmetic:        11,
			myscanner.RightShiftArithmetic:       11,
			myscanner.RightShiftLogical:          11,
			myscanner.Plus:                       12,
			myscanner.Minus:                      12,
			myscanner.Divide:                     13,
			myscanner.Multiply:                   13,
			myscanner.Modulus:                    13,
		},
	}
}
func (p *Parser) getPrec(op interface{}) int {
	if _, ok := op.(scanner.OP); !ok {
		return -1
	}
	ret, _ := p.opPrec[op.(scanner.OP)]
	return ret
}

func (p *Parser) ParseProg() *Prog {
	beginPos := p.scanner.Peek().Pos
	stmts := p.parseStatementList()

	return NewProg(beginPos, p.scanner.GetLastPos(), stmts)
}
func (p *Parser) parseStatementList() []IStatement {
	stmts := []IStatement{}
	t := p.scanner.Peek()
	for t.Kind != scanner.EOF && t.Code != scanner.CloseBrace {
		stmt := p.parseStatement()
		stmts = append(stmts, stmt)
		t = p.scanner.Peek()
	}

	return stmts
}

func (p *Parser) parseStatement() IStatement {
	t := p.scanner.Peek()
	if t.Code == scanner.Function {
		return p.parseFunctionDecl()
	} else if t.Code == scanner.Let {
		return p.parseVariableStatement()
	} else if t.Code == scanner.Return {
		return p.parseReturnStatement()
	} else if t.Code == scanner.OpenBrace {
		return p.parseBlock()
	} else if t.Kind == myscanner.Identifier ||
		t.Kind == myscanner.DecimalLiteral ||
		t.Kind == myscanner.IntegerLiteral ||
		t.Kind == myscanner.StringLiteral ||
		t.Code == scanner.OpenParen {
		return p.parseExpressionStatement()
	} else {
		p.addError("Can not recognize a statement starting with: "+p.scanner.Peek().Text, p.scanner.GetLastPos())
		beginPos := p.scanner.GetNextPos()
		p.skip(nil)
		return NewErrorStmt(beginPos, p.scanner.GetLastPos())
	}
}

func (p *Parser) parseReturnStatement() *ReturnStatement {
	beginPos := p.scanner.GetNextPos()
	var exp IExpression

	p.scanner.Next()
	t := p.scanner.Peek()
	if t.Code != scanner.SemiColon {
		exp = p.parseExpression()
	}

	t = p.scanner.Peek()
	if t.Code == scanner.SemiColon {
		p.scanner.Next()
	} else {
		p.addError("Expect ';' after return statement", p.scanner.GetLastPos())
	}

	return NewReturnStatement(beginPos, p.scanner.GetLastPos(), exp, false)
}

// variableStatement : 'let' variableDecl ';';
func (p *Parser) parseVariableStatement() (rt *VariableStatement) {
	beginPos := p.scanner.GetNextPos()
	isErrorNode := false

	// 跳过'let'
	p.scanner.Next()
	variableDecl := p.parseVariableDecl()
	t := p.scanner.Peek()
	if t.Code == scanner.SemiColon {
		p.scanner.Next()
	} else {
		p.skip(nil)
		isErrorNode = true
	}

	return NewVariableStatement(beginPos, p.scanner.GetLastPos(), variableDecl, isErrorNode)
}

func (p *Parser) parseVariableDecl() (rt *VariableDecl) {
	beginPos := p.scanner.GetNextPos()
	t := p.scanner.Next()
	if t.Kind == scanner.Identifier {
		varName := t.Text
		varType := "any"
		var init IExpression
		isErrorNode := false

		t1 := p.scanner.Peek()
		if t1.Code == scanner.Colon {
			p.scanner.Next()
			t1 = p.scanner.Peek()
			if t1.Kind == scanner.Identifier {
				p.scanner.Next()
				varType = t1.Text
			} else {
				p.addError("Error parsing type annotation in VariableDecl", p.scanner.GetLastPos())
				p.skip([]string{"="})
				isErrorNode = true
			}
		}

		t1 = p.scanner.Peek()
		if t1.Code == scanner.Assign {
			p.scanner.Next()
			init = p.parseExpression()
		}
		return NewVariableDecl(beginPos, p.scanner.GetLastPos(), varName, p.parseType(varType), init, isErrorNode)
	} else {
		p.addError("Expecting variable name in VariableDecl, while we meet "+t.Text, p.scanner.GetLastPos())
		p.skip(nil)
		return NewVariableDecl(beginPos, p.scanner.GetLastPos(), "unknown", Any, nil, true)
	}
}

func (p *Parser) parseType(typeName string) IType {
	switch typeName {
	case "any":
		return Any
	case "number":
		return Number
	case "boolean":
		return Boolean
	case "string":
		return String
	case "undefined":
		return Undefined
	case "null":
		return Null
	case "void":
		return Undefined
	default:
		p.addError("Unrecognized type: "+typeName, p.scanner.GetLastPos())
		return Any
	}
}

func (p *Parser) parseFunctionDecl() (rt *FunctionDecl) {
	beginPos := p.scanner.GetNextPos()
	isErrorNode := false

	// 跳过'function'
	p.scanner.Next()

	t := p.scanner.Next()
	if t.Kind != scanner.Identifier {
		p.addError("Expecting a function name, while we got a "+t.Text, p.scanner.GetLastPos())
		p.skip(nil)
		isErrorNode = true
	}

	var callSignature *CallSignature
	t1 := p.scanner.Peek()
	if t1.Code == scanner.OpenParen {
		callSignature = p.parseCallSignature()
	} else {
		p.addError("Expecting '(' in FunctionDecl, while we got a "+t1.Text, p.scanner.GetLastPos())
		p.skip(nil)
		callSignature = NewCallSignature(beginPos, p.scanner.GetLastPos(), nil, Any, true)
	}

	var functionBody *Block
	t1 = p.scanner.Peek()
	if t1.Code == scanner.OpenBrace {
		functionBody = p.parseBlock()
	} else {
		p.addError("Expecting '{' in FunctionDecl, while we got a "+t1.Text, p.scanner.GetLastPos())
		p.skip(nil)
		functionBody = NewBlock(beginPos, p.scanner.GetLastPos(), []IStatement{}, true)
	}

	return NewFunctionDecl(beginPos, t.Text, callSignature, functionBody, isErrorNode)
}

func (p *Parser) parseCallSignature() (rt *CallSignature) {
	beginPos := p.scanner.GetLastPos()
	t := p.scanner.Next()

	var paramList *ParameterList
	if p.scanner.Peek().Code != myscanner.CloseParen {
		paramList = p.parseParameterList()
	}

	t = p.scanner.Peek()
	if t.Code == scanner.CloseParen {
		p.scanner.Next()

		theType := "any"
		if p.scanner.Peek().Code == scanner.Colon {
			theType = p.parseTypeAnnotation()
		}
		return NewCallSignature(beginPos, p.scanner.GetLastPos(), paramList, p.parseType(theType), false)
	} else {
		p.addError("Expecting a ')' after for a call signature", p.scanner.GetLastPos())
		return NewCallSignature(beginPos, p.scanner.GetLastPos(), paramList, Any, true)
	}
}

func (p *Parser) parseParameterList() *ParameterList {
	var params []*VariableDecl
	beginPos := p.scanner.GetNextPos()
	isErrorNode := false
	t := p.scanner.Peek()

	for t.Code != scanner.CloseParen && t.Kind != scanner.EOF {
		if t.Kind == scanner.Identifier {
			p.scanner.Next()
			t1 := p.scanner.Peek()
			theType := "any"
			if t1.Code == scanner.Colon {
				theType = p.parseTypeAnnotation()
			}
			params = append(params, NewVariableDecl(beginPos, p.scanner.GetLastPos(), t.Text, p.parseType(theType), nil, false))

			t = p.scanner.Peek()
			if t.Code != scanner.CloseParen {
				if t.Code == scanner.Comma {
					p.scanner.Next()
					t = p.scanner.Peek()
				} else {
					p.addError("Expecting a ',' or '）' after a parameter", p.scanner.GetLastPos())
					p.skip(nil)
					isErrorNode = true
					t2 := p.scanner.Peek()
					if t2.Code == scanner.Comma {
						p.scanner.Next()
						t = p.scanner.Peek()
					} else {
						break
					}
				}
			}
		} else {
			p.addError("Expecting an identifier as name of a Parameter", p.scanner.GetLastPos())
			p.skip(nil)
			isErrorNode = true
			if t.Code == scanner.Comma {
				p.scanner.Next()
				t = p.scanner.Peek()
			} else {
				break
			}
		}
	}

	return NewParameterList(beginPos, p.scanner.GetLastPos(), params, isErrorNode)
}

func (p *Parser) parseTypeAnnotation() string {
	theType := "any"
	p.scanner.Next()

	t := p.scanner.Peek()
	if t.Kind == scanner.Identifier {
		p.scanner.Next()
		theType = t.Text
	} else {
		p.addError("Expecting a type name in type annotation", p.scanner.GetLastPos())
	}

	return theType
}

// 解析函数体
// parseFunctionBody取消用这个代替
func (p *Parser) parseBlock() (rt *Block) {
	beginPos := p.scanner.GetNextPos()
	t := p.scanner.Peek()

	p.scanner.Next()
	stmts := p.parseStatementList()
	t = p.scanner.Peek()
	if t.Code == scanner.CloseBrace {
		p.scanner.Next()
		return NewBlock(beginPos, p.scanner.GetLastPos(), stmts, false)
	} else {
		p.addError("Expecting '}' while parsing a block, but we got a "+t.Text, p.scanner.GetLastPos())
		p.skip(nil)
		return NewBlock(beginPos, p.scanner.GetLastPos(), stmts, true)
	}
}

// 解析表达式语句
func (p *Parser) parseExpressionStatement() (rt *ExpressionStatement) {
	exp := p.parseExpression()

	t := p.scanner.Peek()
	stmt := NewExpressionStatement(p.scanner.GetNextPos(), p.scanner.GetLastPos(), exp, false)

	if t.Code == scanner.SemiColon {
		p.scanner.Next()
	} else {
		p.addError("Expecting a semicolon at the end of an expresson statement, while we got a "+t.Text, p.scanner.GetLastPos())
		p.skip(nil)
		stmt.endPos = p.scanner.GetLastPos()
		stmt.isErrorNode = true
	}

	return stmt
}

// 解析表达式
func (p *Parser) parseExpression() (rt IExpression) {
	return p.parseAssignment()
}

func (p *Parser) parseAssignment() (rt IExpression) {
	assignPrec := p.getPrec(scanner.Assign)

	exp1 := p.parseBinary(assignPrec)
	t := p.scanner.Peek()
	tprec := p.getPrec(t.Code)

	var expStack []IExpression
	expStack = append(expStack, exp1)
	var opStack []scanner.OP

	for t.Kind == scanner.Operator && tprec == assignPrec {
		opStack = append(opStack, t.Code.(scanner.OP))
		p.scanner.Next()

		exp1 = p.parseBinary(assignPrec)
		expStack = append(expStack, exp1)

		t = p.scanner.Peek()
		tprec = p.getPrec(t.Code)
	}

	exp1 = expStack[len(expStack)-1]

	if len(opStack) > 0 {
		for i := len(expStack) - 2; i >= 0; i-- {
			exp1 = NewBinary(opStack[i], expStack[i], exp1, false)
		}
	}

	return exp1
}

func (p *Parser) parseBinary(prec int) (rt IExpression) {
	exp1 := p.parseUnary()
	t := p.scanner.Peek()

	tprec := p.getPrec(t.Code)

	for t.Kind == scanner.Operator && tprec > prec {
		p.scanner.Next()
		exp2 := p.parseBinary(tprec)
		exp := NewBinary(t.Code.(scanner.OP), exp1, exp2, false)
		exp1 = exp
		t = p.scanner.Peek()
		tprec = p.getPrec(t.Code)
	}

	return exp1
}

func (p *Parser) parseUnary() (rt IExpression) {
	beginPos := p.scanner.GetNextPos()
	t := p.scanner.Peek()

	if t.Kind == scanner.Operator {
		p.scanner.Next()
		exp := p.parseUnary()
		return NewUnary(beginPos, p.scanner.GetLastPos(), t.Code.(scanner.OP), exp, true, false)
	} else {
		exp := p.parsePrimary()
		t1 := p.scanner.Peek()
		if t1.Kind == scanner.Operator && (t1.Code == scanner.Inc || t1.Code == scanner.Dec) {
			p.scanner.Next()
			return NewUnary(beginPos, p.scanner.GetLastPos(), t1.Code.(scanner.OP), exp, false, true)
		} else {
			return exp
		}
	}
}

func (p *Parser) parsePrimary() (rt IExpression) {
	beginPos := p.scanner.GetNextPos()
	t := p.scanner.Peek()

	if t.Kind == scanner.Identifier {
		if p.scanner.Peek2().Code == scanner.OpenParen {
			return p.parseFunctionCall()
		} else {
			p.scanner.Next()
			return NewVariable(beginPos, p.scanner.GetLastPos(), t.Text, false)
		}
	} else if t.Kind == scanner.IntegerLiteral {
		p.scanner.Next()
		if i, err := strconv.Atoi(t.Text); err == nil {
			return NewIntegerLiteral(beginPos, i, false)
		}
	} else if t.Kind == scanner.DecimalLiteral {
		p.scanner.Next()
		if i, err := strconv.ParseFloat(t.Text, 64); err == nil {
			return NewDecimalLiteral(beginPos, i, false)
		}
	} else if t.Kind == scanner.StringLiteral {
		p.scanner.Next()
		return NewStringLiteral(beginPos, t.Text, false)
	} else if t.Code == myscanner.OpenParen {
		p.scanner.Next()
		exp := p.parseExpression()
		t1 := p.scanner.Peek()
		if t1.Code == scanner.CloseParen {
			p.scanner.Next()
		} else {
			p.addError("Expecting a ')' at the end of a primary expresson, while we got a "+t.Text, p.scanner.GetLastPos())
			p.skip(nil)
		}
		return exp
	} else {
		p.addError("Can not recognize a primary expression starting with: "+t.Text, p.scanner.GetLastPos())
		exp := NewErrorExp(beginPos, p.scanner.GetLastPos())
		return exp
	}
	return
}

func (p *Parser) parseFunctionCall() (rt *FunctionCall) {
	beginPos := p.scanner.GetNextPos()
	params := []IExpression{}
	name := p.scanner.Next().Text

	p.scanner.Next()

	t1 := p.scanner.Peek()
	for t1.Code != scanner.CloseParen && t1.Kind != scanner.EOF {
		exp := p.parseExpression()
		params = append(params, exp)

		t1 = p.scanner.Peek()
		if t1.Code != scanner.CloseParen {
			if t1.Code == scanner.Comma {
				t1 = p.scanner.Next()
			} else {
				p.addError("Expecting a comma at the end of a parameter, while we got a "+t1.Text, p.scanner.GetLastPos())
				p.skip([]string{})
				return NewFunctionCall(beginPos, p.scanner.GetLastPos(), name, params, true)
			}
		}
	}

	if t1.Code == scanner.CloseParen {
		p.scanner.Next()
	}

	return NewFunctionCall(beginPos, p.scanner.GetLastPos(), name, params, false)
}

func (p *Parser) addError(msg string, pos *scanner.Position) {
	p.errors = append(p.errors, NewCompilerError(msg, pos, false))
	fmt.Println("@" + pos.ToString() + " : " + msg)
}
func (p *Parser) addWarning(msg string, pos *scanner.Position) {
	p.warnings = append(p.warnings, NewCompilerError(msg, pos, false))
	fmt.Println("@" + pos.ToString() + " : " + msg)
}

func (p *Parser) skip(seperators []string) {
	t := p.scanner.Peek()
	for t.Kind != scanner.EOF {
		if t.Kind == scanner.Keyword {
			return
		} else if t.Kind == scanner.Seperator && (t.Text == "," || t.Text == ";" ||
			t.Text == "{" || t.Text == "}" || t.Text == "(" || t.Text == ")" ||
			slices.Index(seperators, t.Text) != -1) {
			return
		} else {
			p.scanner.Next()
			t = p.scanner.Peek()
		}
	}
}
