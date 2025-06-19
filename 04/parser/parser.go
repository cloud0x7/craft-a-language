package parser

import (
	"craft-language/ast"
	"craft-language/scanner"
	"fmt"
	"log"
	"strconv"
)

type Parser struct {
	scanner *scanner.Scanner
	opPrec  map[string]int
}

func NewParser(scanner *scanner.Scanner) *Parser {
	return &Parser{
		scanner: scanner,
		opPrec: map[string]int{
			"=":    2,
			"+=":   2,
			"-=":   2,
			"*=":   2,
			"%=":   2,
			"&=":   2,
			"|=":   2,
			"^=":   2,
			"~=":   2,
			"<<=":  2,
			">>=":  2,
			">>>=": 2,
			"||":   4,
			"&&":   5,
			"|":    6,
			"^":    7,
			"&":    8,
			"==":   9,
			"===":  9,
			"!=":   9,
			"!==":  9,
			">":    10,
			">=":   10,
			"<":    10,
			"<=":   10,
			"<<":   11,
			">>":   11,
			">>>":  11,
			"+":    12,
			"-":    12,
			"*":    13,
			"/":    13,
			"%":    13,
		},
	}
}
func (p *Parser) ParseProg() *ast.Prog {
	return ast.NewProg(p.parseStatementList())
}
func (p *Parser) parseStatementList() []ast.Statement {
	stmts := []ast.Statement{}
	t := p.scanner.Peek()
	for t.Kind != scanner.EOF && t.Text != "}" {
		stmt, ok := p.parseStatement()
		if ok {
			stmts = append(stmts, stmt)
		}
		t = p.scanner.Peek()
	}

	return stmts
}

func (p *Parser) parseStatement() (ast.Statement, bool) {
	t := p.scanner.Peek()
	if t.Kind == scanner.Keyword && t.Text == "function" {
		return p.parseFunctionDecl()
	} else if t.Text == "let" {
		return p.parseVariableDecl()
	} else if t.Kind == scanner.Identifier ||
		t.Kind == scanner.DecimalLiteral ||
		t.Kind == scanner.IntegerLiteral ||
		t.Kind == scanner.StringLiteral ||
		t.Text == "(" {
		return p.parseExpressionStatement()
	} else {
		log.Print("Can not recognize a expression starting with: " + p.scanner.Peek().Text)
		return nil, false
	}
}

func (p *Parser) parseVariableDecl() (rt *ast.VariableDecl, ok bool) {
	p.scanner.Next()

	t := p.scanner.Next()
	if t.Kind == scanner.Identifier {
		varName := t.Text
		varType := "any"
		var init ast.Expression

		t1 := p.scanner.Peek()

		if t1.Text == ":" {
			p.scanner.Next()
			t1 = p.scanner.Peek()
			if t1.Kind == scanner.Identifier {
				p.scanner.Next()
				varType = t1.Text
				t1 = p.scanner.Peek()
			} else {
				log.Print("Error parsing type annotation in VariableDecl")
				return nil, false
			}
		}

		if t1.Text == "=" {
			p.scanner.Next()
			init, _ = p.parseExpression()
		}

		t1 = p.scanner.Peek()
		if t1.Text == ";" {
			p.scanner.Next()
			return ast.NewVariableDecl(varName, varType, init), true
		} else {
			log.Print("Expecting ; at the end of varaible declaration, while we meet " + t1.Text)
			return
		}
	} else {
		return
	}
}

func (p *Parser) parseFunctionDecl() (rt *ast.FunctionDecl, bl bool) {
	p.scanner.Next()

	t := p.scanner.Next()
	if t.Kind == scanner.Identifier {
		t1 := p.scanner.Next()
		if t1.Text == "(" {
			t2 := p.scanner.Next()
			if t2.Text == ")" {
				functionBody, ok := p.parseFunctionBody()
				if ok {
					return ast.NewFunctionDecl(t.Text, functionBody), true
				} else {
					log.Print("Error parsing FunctionBody in FunctionDecl")
					return
				}
			} else {
				log.Print("Expecting ')' in FunctionDecl, while we got a " + t.Text)
				return
			}
		} else {
			log.Print("Expecting '(' in FunctionDecl, while we got a " + t.Text)
			return
		}
	} else {
		log.Print("Expecting a function name, while we got a " + t.Text)
		return
	}
}

func (p *Parser) parseFunctionBody() (rt *ast.Block, bl bool) {
	t := p.scanner.Peek()
	if t.Text == "{" {
		p.scanner.Next()
		stmts := p.parseStatementList()
		t = p.scanner.Next()
		if t.Text == "}" {
			return ast.NewBlock(stmts), true
		} else {
			log.Print("Expecting '}' in FunctionBody, while we got a " + t.Text)
			return
		}
	} else {
		log.Print("Expecting '{' in FunctionBody, while we got a " + t.Text)
		return
	}
}

func (p *Parser) parseExpressionStatement() (rt *ast.ExpressionStatement, bl bool) {
	exp, ok := p.parseExpression()
	if ok {
		t := p.scanner.Peek()
		if t.Text == ";" {
			p.scanner.Next()
			return ast.NewExpressionStatement(exp), true
		} else {
			log.Print("Expecting a semicolon at the end of an expresson statement, while we got a " + t.Text)
		}
	} else {
		log.Print("Error parsing ExpressionStatement")
	}

	return
}

func (p *Parser) parseExpression() (rt ast.Expression, bl bool) {
	return p.parseBinary(0)
}

func (p *Parser) getPrec(op string) int {
	if ret, ok := p.opPrec[op]; ok {
		return ret
	}
	return -1
}

func (p *Parser) parseBinary(prec int) (rt ast.Expression, bl bool) {
	exp1, ok := p.parsePrimary()
	if ok {
		t := p.scanner.Peek()
		tprec := p.getPrec(t.Text)

		for t.Kind == scanner.Operator && tprec > prec {
			p.scanner.Next()
			exp2, ok := p.parseBinary(tprec)
			if ok {
				exp := ast.NewBinary(t.Text, exp1, exp2)
				exp1 = exp
				t = p.scanner.Peek()
				tprec = p.getPrec(t.Text)
			} else {
				fmt.Print("Can not recognize a expression starting with: " + t.Text)
			}
		}
		return exp1, ok
	} else {
		fmt.Print("Can not recognize a expression starting with: " + p.scanner.Peek().Text)
		return
	}
}

func (p *Parser) parsePrimary() (rt ast.Expression, bl bool) {
	t := p.scanner.Peek()

	if t.Kind == scanner.Identifier {
		if p.scanner.Peek2().Text == "(" {
			return p.parseFunctionCall()
		} else {
			p.scanner.Next()
			return ast.NewVariable(t.Text), true
		}
	} else if t.Kind == scanner.IntegerLiteral {
		p.scanner.Next()
		tmp, _ := strconv.Atoi(t.Text)
		return ast.NewIntegerLiteral(tmp), true
	} else if t.Kind == scanner.DecimalLiteral {
		p.scanner.Next()
		tmp, _ := strconv.ParseFloat(t.Text, 64)
		return ast.NewDecimalLiteral(tmp), true
	} else if t.Kind == scanner.StringLiteral {
		p.scanner.Next()
		return ast.NewStringLiteral(t.Text), true
	} else if t.Text == "(" {
		p.scanner.Next()
		exp, _ := p.parseExpression()
		t1 := p.scanner.Peek()
		if t1.Text == ")" {
			p.scanner.Next()
			return exp, true
		} else {
			log.Print("Expecting a ')' at the end of a primary expresson, while we got a " + t.Text)
			return
		}
	} else {
		log.Print("Can not recognize a primary expression starting with: " + t.Text)
		return
	}
}

func (p *Parser) parseFunctionCall() (rt *ast.FunctionCall, bl bool) {
	params := []ast.Expression{}
	t := p.scanner.Next()
	if t.Kind == scanner.Identifier {
		t1 := p.scanner.Next()
		if t1.Text == "(" {
			t1 = p.scanner.Peek()
			for t1.Text != ")" {
				exp, ok := p.parseExpression()
				if ok {
					params = append(params, exp)
				} else {
					log.Print("Error parsing parameter in function call")
					return
				}

				t1 = p.scanner.Peek()
				if t1.Text != ")" {
					if t1.Text == "," {
						t1 = p.scanner.Next()
					} else {
						log.Print("Expecting a comma at the end of a function call, while we got a " + t.Text)
						return
					}
				}
			}

			p.scanner.Next()
			return ast.NewFunctionCall(t.Text, params), true
		}
	}

	return
}
