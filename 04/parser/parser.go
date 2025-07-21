package parser

import (
	"craft-language/ast"
	"craft-language/scanner"
	"fmt"
	"log"
	"strconv"
)

// 语法解析器
// 通常用parseProg()作为入口，解析整个程序。也可以用下级的某个节点作为入口，只解析一部分语法
type Parser struct {
	scanner *scanner.Scanner // 词法分析器
	opPrec  map[string]int   // 操作符的优先级
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

// 解析Prog
func (p *Parser) ParseProg() *ast.Prog {
	return ast.NewProg(p.parseStatementList())
}

// 解析语句列表
func (p *Parser) parseStatementList() []ast.Statement {
	stmts := []ast.Statement{}
	t := p.scanner.Peek()                        // 预读
	for t.Kind != scanner.EOF && t.Text != "}" { // 没到结尾
		stmt, ok := p.parseStatement() // 解析语句，成功则加入列表
		if ok {
			stmts = append(stmts, stmt)
		}
		t = p.scanner.Peek()
	}

	return stmts
}

// 解析语句
func (p *Parser) parseStatement() (ast.Statement, bool) {
	t := p.scanner.Peek()
	if t.Kind == scanner.Keyword && t.Text == "function" { // 此关键字直接解析函数声明
		return p.parseFunctionDecl()
	} else if t.Text == "let" { // 变量声明
		return p.parseVariableDecl()
	} else if t.Kind == scanner.Identifier ||
		t.Kind == scanner.DecimalLiteral ||
		t.Kind == scanner.IntegerLiteral ||
		t.Kind == scanner.StringLiteral ||
		t.Text == "(" {
		return p.parseExpressionStatement() // 解析表达式语句
	} else {
		log.Print("Can not recognize a expression starting with: " + p.scanner.Peek().Text)
		return nil, false
	}
}

// 解析变量声明
func (p *Parser) parseVariableDecl() (rt *ast.VariableDecl, ok bool) {
	p.scanner.Next() // 跳过'let'

	t := p.scanner.Next()
	if t.Kind == scanner.Identifier {
		varName := t.Text // 变量名
		varType := "any"  // 类型未知，先默认为any
		var init ast.Expression

		t1 := p.scanner.Peek()
		// 类型标注
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
		// 变量有初始化值
		if t1.Text == "=" {
			p.scanner.Next()
			init, _ = p.parseExpression() // 解析表达式作为值
		}

		t1 = p.scanner.Peek()
		if t1.Text == ";" {
			p.scanner.Next()
			// 返回变量声明节点
			return ast.NewVariableDecl(varName, varType, init), true
		} else {
			log.Print("Expecting ; at the end of varaible declaration, while we meet " + t1.Text)
			return
		}
	} else {
		return
	}
}

// 解析函数声明
func (p *Parser) parseFunctionDecl() (rt *ast.FunctionDecl, bl bool) {
	p.scanner.Next() // 跳过关键字'function'

	t := p.scanner.Next()
	if t.Kind == scanner.Identifier {
		// 读取()
		t1 := p.scanner.Next()
		if t1.Text == "(" {
			t2 := p.scanner.Next()
			if t2.Text == ")" {
				functionBody, ok := p.parseFunctionBody() // 解析函数体
				if ok {                                   // 解析成功，返回函数声明
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

// 解析函数体
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

// 解析表达式语句
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

// 解析表达式
func (p *Parser) parseExpression() (rt ast.Expression, bl bool) {
	return p.parseBinary(0)
}

// 获取操作符优先级
func (p *Parser) getPrec(op string) int {
	if ret, ok := p.opPrec[op]; ok {
		return ret
	}
	return -1
}

// 解析二元表达式，采用运算符优先级算法，递归计算，首次调用时prec为0
func (p *Parser) parseBinary(prec int) (rt ast.Expression, bl bool) {
	exp1, ok := p.parsePrimary() // 解析基础表达式
	if ok {
		// 预读下一运算符并查优先级
		t := p.scanner.Peek() //
		tprec := p.getPrec(t.Text)

		// 如果右边新运算符优先级更高，则把右边作为右子节点
		// 对于2+3*5
		// 第一次循环，遇到+号，优先级大于零，所以做一次递归的binary
		// 在递归的binary中，遇到乘号，优先级大于+号，所以形成3*5返回，又变成上一级的右子节点。
		//
		// 反过来，如果是3*5+2
		// 第一次循环还是一样。
		// 在递归中，新的运算符的优先级要小，所以只返回一个5，跟前一个节点形成3*5.
		for t.Kind == scanner.Operator && tprec > prec {
			p.scanner.Next()
			exp2, ok := p.parseBinary(tprec) // 解析后续的二元表达式
			if ok {
				exp := ast.NewBinary(t.Text, exp1, exp2)
				exp1 = exp
				t = p.scanner.Peek()
				tprec = p.getPrec(t.Text)
			} else {
				fmt.Print("Can not recognize a expression starting with: " + t.Text)
			}
		}
		// 右边优先级低，直接返回右子节点
		return exp1, ok
	} else {
		fmt.Print("Can not recognize a expression starting with: " + p.scanner.Peek().Text)
		return
	}
}

// 解析基础表达式
func (p *Parser) parsePrimary() (rt ast.Expression, bl bool) {
	t := p.scanner.Peek()

	// 标识符，有可能是变量或函数调用，所以再向前预读一个token
	// 相当于在局部使用了LL(2)算法
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

// 解析函数调用
func (p *Parser) parseFunctionCall() (rt *ast.FunctionCall, bl bool) {
	params := []ast.Expression{}
	t := p.scanner.Next()
	if t.Kind == scanner.Identifier {
		t1 := p.scanner.Next()
		if t1.Text == "(" {
			t1 = p.scanner.Peek()
			for t1.Text != ")" {
				exp, ok := p.parseExpression() // 解析所有参数
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
