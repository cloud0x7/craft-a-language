package vm

import (
	"craft-language/parser"
	"craft-language/scanner"
	"fmt"
	"maps"
	"reflect"
	"slices"
	"strconv"
	"strings"
)

const (
	iconst_0     = 0x03
	iconst_1     = 0x04
	iconst_2     = 0x05
	iconst_3     = 0x06
	iconst_4     = 0x07
	iconst_5     = 0x08
	bipush       = 0x10 //8位整数入栈
	sipush       = 0x11 //16位整数入栈
	ldc          = 0x12 //从常量池加载，load const
	iload        = 0x15 //本地变量入栈
	iload_0      = 0x1a
	iload_1      = 0x1b
	iload_2      = 0x1c
	iload_3      = 0x1d
	istore       = 0x36
	istore_0     = 0x3b
	istore_1     = 0x3c
	istore_2     = 0x3d
	istore_3     = 0x3e
	iadd         = 0x60
	isub         = 0x64
	imul         = 0x68
	idiv         = 0x6c
	iinc         = 0x84
	lcmp         = 0x94
	ifeq         = 0x99
	ifne         = 0x9a
	iflt         = 0x9b
	ifge         = 0x9c
	ifgt         = 0x9d
	ifle         = 0x9e
	if_icmpeq    = 0x9f
	if_icmpne    = 0xa0
	if_icmplt    = 0xa1
	if_icmpge    = 0xa2
	if_icmpgt    = 0xa3
	if_icmple    = 0xa4
	s_goto       = 0xa7
	ireturn      = 0xac
	s_return     = 0xb1
	invokestatic = 0xb8 //调用函数

	//自行扩展的操作码
	sadd = 0x61 //字符串连接
	sldc = 0x13 //把字符串常量入栈。字符串放在常量区，用两个操作数记录下标。
)

type BCModule struct {
	consts []any
	_main  *parser.FunctionSymbol
}

func NewBCModule() *BCModule {
	b := &BCModule{
		consts: make([]any, 0),
	}
	for _, fun := range slices.Sorted(maps.Keys(parser.Built_ins)) {
		b.consts = append(b.consts, parser.Built_ins[fun])
	}

	return b
}

type BCModuleDumper struct{}

func (b *BCModuleDumper) Dump(bcModule *BCModule) {
	symbolDumper := parser.SymbolDumper{}
	for _, v := range bcModule.consts {
		_, ok1 := v.(int)
		_, ok2 := v.(float64)
		if ok1 || ok2 {
			fmt.Printf("Number: %d\n", v)
		} else if _, ok := v.(string); ok {
			fmt.Printf("String: %s\n", v)
		} else if _, ok := v.(parser.ISymbol); ok {
			symbolDumper.Visit(v.(*parser.FunctionSymbol), "")
		} else {
			fmt.Printf("unknown const:%s\n", v)
		}
	}
}

type BCGenerator struct {
	parser.AstVisitor
	m            BCModule
	functionSym  *parser.FunctionSymbol
	inExpression bool
}

func NewBCGenerator() *BCGenerator {
	r := &BCGenerator{
		m: *NewBCModule(),
	}
	r.AstVisitor.Curr = r

	return r
}
func (b *BCGenerator) VisitProg(prog *parser.Prog, additional any) any {
	b.functionSym = prog.Sym
	if b.functionSym != nil {
		b.m.consts = append(b.m.consts, b.functionSym)
		b.m._main = b.functionSym
		b.functionSym.ByteCode = b.VisitBlock(prog.Block, additional).([]int)
	}

	return b.m
}

func (b *BCGenerator) VisitFunctionDecl(functionDecl *parser.FunctionDecl, additional any) any {
	lastFunctionSym := b.functionSym
	b.functionSym = functionDecl.Sym

	b.m.consts = append(b.m.consts, b.functionSym)

	code1 := b.Visit(functionDecl.CallSignature, additional)
	code2 := b.Visit(functionDecl.Body, additional)

	b.addOffsetToJumpOp(code2.([]int), len(code1.([]int)))

	if b.functionSym != nil {
		b.functionSym.ByteCode = append(code1.([]int), code2.([]int)...)
	}

	b.functionSym = lastFunctionSym
	return nil
}

func (b *BCGenerator) VisitBlock(block *parser.Block, additional any) any {
	ret := []int{}

	for _, x := range block.Stmts {
		var code []int
		b.inExpression = false
		r := b.Visit(x, additional)
		if r != nil {
			code = r.([]int)
		}
		if code != nil {
			b.addOffsetToJumpOp(code, len(ret))
			ret = append(ret, code...)
		}
	}

	return ret
}

func (b *BCGenerator) VisitVariableDecl(variableDecl *parser.VariableDecl, additional any) any {
	var code []int
	if variableDecl.Init != nil {
		ret := b.Visit(variableDecl.Init, additional).([]int)
		code = append(code, ret...)
		code = append(code, (b.setVariableValue(variableDecl.Sym)).([]int)...)
	}

	return code
}

func (b *BCGenerator) VisitReturnStatement(stmt *parser.ReturnStatement, additional any) any {
	var code []int

	if stmt.Exp != nil {

		code1 := b.Visit(stmt.Exp, additional).([]int)
		code = append(code, code1...)
		code = append(code, ireturn)
		return code
	} else {
		code = append(code, s_return)
		return code
	}
}

func (b *BCGenerator) VisitFunctionCall(functionCall *parser.FunctionCall, additional any) any {
	var code []int

	for _, param := range functionCall.Arguments {
		code1 := b.Visit(param, additional)
		code = append(code, code1.([]int)...)
	}

	index := slices.IndexFunc(b.m.consts, func(ele any) bool {
		return ele == functionCall.Sym
	})
	if index == -1 {
		panic("生成字节码时，在模块中查找函数失败！")
	}
	code = append(code, invokestatic)
	code = append(code, index>>8)
	code = append(code, index)

	return code
}

func (b *BCGenerator) VisitIfStatement(stmt *parser.IfStatement, additional any) any {
	var code []int

	code_condition := b.Visit(stmt.Condition, additional).([]int)
	b.inExpression = false

	code_ifBlock := b.Visit(stmt.Stmt, additional).([]int)
	b.inExpression = false

	code_elseBlock := b.Visit(stmt.ElseStmt, additional).([]int)
	b.inExpression = false

	offset_ifBlock := len(code_condition) + 3
	offset_elseBlock := len(code_condition) + len(code_ifBlock) + 6
	offset_nextStmt := offset_elseBlock + len(code_elseBlock)

	b.addOffsetToJumpOp(code_ifBlock, offset_ifBlock)
	b.addOffsetToJumpOp(code_elseBlock, offset_elseBlock)

	code = append(code, code_condition...)

	code = append(code, ifeq)
	code = append(code, offset_elseBlock>>8)
	code = append(code, offset_elseBlock)

	code = append(code, code_ifBlock...)

	code = append(code, s_goto)
	code = append(code, offset_nextStmt>>8)
	code = append(code, offset_nextStmt)

	code = append(code, code_elseBlock...)

	return code
}

func (b *BCGenerator) VisitForStatement(stmt *parser.ForStatement, additional any) any {
	var code []int
	var code_init []int

	if stmt.Init != nil {
		code_init = b.Visit(stmt.Init, additional).([]int)
	}
	b.inExpression = false

	var code_condition []int
	if stmt.Condition != nil {
		code_condition = b.Visit(stmt.Condition, additional).([]int)
	}
	b.inExpression = false

	var code_increment []int
	if stmt.Increment != nil {
		code_increment = b.Visit(stmt.Increment, additional).([]int)
	}
	b.inExpression = false

	var code_stmt []int
	if stmt.Stmt != nil {
		code_stmt = b.Visit(stmt.Stmt, additional).([]int)
	}
	b.inExpression = false

	offset_condition := len(code_init)
	offset_stmt := offset_condition + len(code_condition)
	if len(code_condition) > 0 {
		offset_stmt += 3
	}

	offset_increment := offset_stmt + len(code_stmt)
	offset_nextStmt := offset_increment + len(code_increment) + 3

	b.addOffsetToJumpOp(code_condition, offset_condition)
	b.addOffsetToJumpOp(code_increment, offset_increment)
	b.addOffsetToJumpOp(code_stmt, offset_stmt)

	code = append(code, code_init...)
	if len(code_condition) > 0 {
		code = append(code, code_condition...)
		code = append(code, ifeq)
		code = append(code, offset_nextStmt>>8)
		code = append(code, offset_nextStmt)
	}

	code = append(code, code_stmt...)
	code = append(code, code_increment...)

	code = append(code, s_goto)
	code = append(code, offset_condition>>8)
	code = append(code, offset_condition)

	return code
}

func (b *BCGenerator) addOffsetToJumpOp(code []int, offset int) []int {
	if offset == 0 {
		return code
	}
	codeIndex := 0
	for codeIndex < len(code) {
		switch code[codeIndex] {
		case iadd, sadd, isub, imul, idiv, iconst_0, iconst_1, iconst_2, iconst_3, iconst_4, iconst_5, istore_0, istore_1, istore_2, istore_3, iload_0, iload_1, iload_2, iload_3, ireturn, s_return, lcmp:
			codeIndex += 1
			continue
		case iload, istore, bipush, ldc, sldc:
			codeIndex += 2
			continue
			//指令后面带2个字节的操作数
		case iinc, invokestatic, sipush:
			codeIndex += 3
		case if_icmpeq, if_icmpne, if_icmpge, if_icmpgt, if_icmple, if_icmplt, ifeq, ifne, ifge, ifgt, ifle, iflt, s_goto:
			byte1 := code[codeIndex+1]
			byte2 := code[codeIndex+2]
			address := byte1<<8 | byte2 + offset
			code[codeIndex+1] = address >> 8
			code[codeIndex+2] = address
			codeIndex += 3
		default:
			fmt.Println("unrecognized Op Code in addOffsetToJumpOp: ", code[codeIndex])
			return code
		}
	}

	return code
}

func (b *BCGenerator) getVriableValue(sym *parser.VarSymbol) any {
	var code []int

	if sym != nil {
		index := slices.IndexFunc(b.functionSym.Vars, func(v *parser.VarSymbol) bool {
			return v == sym
		})
		if index == -1 {
			panic("生成字节码时（获取变量的值），在函数符号中获取本地变量下标失败！")
		}

		switch index {
		case 0:
			code = append(code, iload_0)
		case 1:
			code = append(code, iload_1)
		case 2:
			code = append(code, iload_2)
		case 3:
			code = append(code, iload_3)
		default:
			code = append(code, iload)
			code = append(code, index)
		}
	}

	return code
}

func (b *BCGenerator) setVariableValue(sym *parser.VarSymbol) any {
	var code []int

	if sym != nil {
		index := slices.IndexFunc(b.functionSym.Vars, func(v *parser.VarSymbol) bool {
			return v == sym
		})
		if index == -1 {
			panic("生成字节码时（获取变量的值），在函数符号中查找变量失败！")
		}

		switch index {
		case 0:
			code = append(code, istore_0)
		case 1:
			code = append(code, istore_1)
		case 2:
			code = append(code, istore_2)
		case 3:
			code = append(code, istore_3)
		default:
			code = append(code, istore)
			code = append(code, index)
		}
	}

	return code
}

func (b *BCGenerator) VisitBinary(exp *parser.Binary, additional any) any {
	b.inExpression = true

	var code []int
	code1 := b.Visit(exp.Exp1, additional)
	code2 := b.Visit(exp.Exp2, additional)

	address1 := 0
	address2 := 0
	tempCode := 0

	if exp.Op == scanner.Assign {
		varSymbol := code1.(*parser.VarSymbol)
		// fmt.Printf("varSymol: %v\n", varSymbol)
		code = code2.([]int)
		code = append(code, (b.setVariableValue(varSymbol)).([]int)...)
	} else {
		code = code1.([]int)
		code = append(code, code2.([]int)...)

		switch exp.Op {
		case scanner.Plus:
			if exp.TheType == parser.String {
				code = append(code, sadd)
			} else {
				code = append(code, iadd)
			}
		case scanner.Minus:
			code = append(code, isub)
		case scanner.Multiply:
			code = append(code, imul)
		case scanner.Divide:
			code = append(code, idiv)
		case scanner.G, scanner.GE, scanner.L, scanner.LE, scanner.EQ, scanner.NE:
			if exp.Op == scanner.G {
				tempCode = if_icmple
			} else if exp.Op == scanner.GE {
				tempCode = if_icmplt
			} else if exp.Op == scanner.L {
				tempCode = if_icmpge
			} else if exp.Op == scanner.LE {
				tempCode = if_icmpgt
			} else if exp.Op == scanner.EQ {
				tempCode = if_icmpne
			} else if exp.Op == scanner.NE {
				tempCode = if_icmpeq
			}
			address1 = len(code) + 7
			address2 = address1 + 1
			code = append(code, tempCode)
			code = append(code, address1>>8)
			code = append(code, address1)
			code = append(code, iconst_1)
			code = append(code, s_goto)
			code = append(code, address2>>8)
			code = append(code, address2)
			code = append(code, iconst_0)
		default:
			fmt.Printf("unsupported binary operation: %s\n", exp.Op.ToString())
			return []int{}
		}
	}

	return code
}
func (b *BCGenerator) VisitUnary(exp *parser.Unary, additional any) any {
	var code []int

	v := b.Visit(exp.Exp, additional)
	var varSymbol *parser.VarSymbol
	var varIndex int = -1

	if exp.Op == scanner.Inc {
		varSymbol, _ = v.(*parser.VarSymbol)
		varIndex = slices.IndexFunc(b.functionSym.Vars, func(v *parser.VarSymbol) bool {
			return v == varSymbol
		})
		if exp.IsPrefix {
			code = append(code, iinc)
			code = append(code, varIndex)
			code = append(code, 1)
			if b.inExpression {
				code = append(code, b.getVriableValue(varSymbol).(int))

			}
		} else {
			if b.inExpression {
				code = append(code, b.getVriableValue(varSymbol).(int))
			}
			code = append(code, iinc)
			code = append(code, varIndex)
			code = append(code, 1)
		}
	} else if exp.Op == scanner.Dec {
		varSymbol = v.(*parser.VarSymbol)
		varIndex = slices.IndexFunc(b.functionSym.Vars, func(v *parser.VarSymbol) bool {
			return v == varSymbol
		})
		if exp.IsPrefix {
			code = append(code, iinc)
			code = append(code, varIndex)
			code = append(code, -1)
			if b.inExpression {
				code = append(code, b.getVriableValue(varSymbol).(int))

			}
		} else {
			if b.inExpression {
				code = append(code, b.getVriableValue(varSymbol).(int))
			}
			code = append(code, iinc)
			code = append(code, varIndex)
			code = append(code, -1)
		}
	} else {
		fmt.Printf("Unsupported unary oprator : %s\n", exp.Op.ToString())
	}

	return code
}
func (b *BCGenerator) VisitVariable(variable *parser.Variable, additional any) any {
	if variable.IsLeftValue() {
		return variable.Sym
	} else {
		return b.getVriableValue(variable.Sym)
	}
}
func (b *BCGenerator) VisitIntegerLiteral(exp *parser.IntegerLiteral, additional any) any {
	var ret []int
	value := exp.Value

	if value >= 0 && value <= 5 {
		switch value {
		case 0:
			ret = append(ret, iconst_0)
		case 1:
			ret = append(ret, iconst_1)
		case 2:
			ret = append(ret, iconst_2)
		case 3:
			ret = append(ret, iconst_3)
		case 4:
			ret = append(ret, iconst_4)
		case 5:
			ret = append(ret, iconst_5)
		}
	} else if value >= -128 && value < 128 {
		ret = append(ret, bipush)
		ret = append(ret, value)
	} else if value >= -32768 && value < 32768 {
		ret = append(ret, sipush)
		ret = append(ret, value>>8)
		ret = append(ret, value&0x00ff)
	} else {
		ret = append(ret, ldc)
		b.m.consts = append(b.m.consts, value)
		ret = append(ret, len(b.m.consts)-1)
	}

	return ret
}

func (b *BCGenerator) VisitStringLiteral(exp *parser.StringLiteral, additional any) any {
	var ret []int

	value := exp.Value
	b.m.consts = append(b.m.consts, value)
	ret = append(ret, sldc)
	ret = append(ret, len(b.m.consts)-1)

	return ret
}

type VM struct {
	callStack []*StackFrame
}

func NewVM() *VM {
	return &VM{
		callStack: []*StackFrame{},
	}
}

func (v *VM) Execute(bcModule *BCModule) int {
	var functionSym *parser.FunctionSymbol
	if bcModule._main == nil {
		fmt.Println("Can not find main function.")
		return -1
	} else {
		functionSym = bcModule._main
	}

	frame := NewStackFrame(functionSym)
	v.callStack = append(v.callStack, frame)

	var code []int
	if functionSym.ByteCode != nil {
		code = functionSym.ByteCode
	} else {
		fmt.Println("Cant not find code for ", frame.functionSym.Name)
		return -1
	}

	codeIndex := 0
	opCode := code[codeIndex]

	byte1 := 0
	byte2 := 0
	var vleft any
	var vright any
	constIndex := 0
	numValue := 0
	strValue := ""

	for {
		switch opCode {
		case iconst_0:
			frame.oprandStack = append(frame.oprandStack, 0)
			codeIndex++
			opCode = code[codeIndex]
		case iconst_1:
			frame.oprandStack = append(frame.oprandStack, 1)
			codeIndex++
			opCode = code[codeIndex]
		case iconst_2:
			frame.oprandStack = append(frame.oprandStack, 2)
			codeIndex++
			opCode = code[codeIndex]
		case iconst_3:
			frame.oprandStack = append(frame.oprandStack, 3)
			codeIndex++
			opCode = code[codeIndex]
		case iconst_4:
			frame.oprandStack = append(frame.oprandStack, 4)
			codeIndex++
			opCode = code[codeIndex]
		case iconst_5:
			frame.oprandStack = append(frame.oprandStack, 5)
			codeIndex++
			opCode = code[codeIndex]
		case bipush:
			codeIndex++
			frame.oprandStack = append(frame.oprandStack, code[codeIndex])
			codeIndex++
			opCode = code[codeIndex]
		case sipush:
			codeIndex++
			byte1 = code[codeIndex]
			codeIndex++
			byte2 = code[codeIndex]
			frame.oprandStack = append(frame.oprandStack, byte1<<8|byte2)
			codeIndex++
			opCode = code[codeIndex]
		case ldc:
			codeIndex++
			constIndex = code[codeIndex]
			numValue = bcModule.consts[constIndex].(int)
			frame.oprandStack = append(frame.oprandStack, numValue)
			codeIndex++
			opCode = code[codeIndex]
		case sldc:
			codeIndex++
			constIndex = code[codeIndex]
			strValue = bcModule.consts[constIndex].(string)
			frame.oprandStack = append(frame.oprandStack, strValue)
			codeIndex++
			opCode = code[codeIndex]
		case iload:
			codeIndex++
			frame.oprandStack = append(frame.oprandStack, frame.localvars[code[codeIndex]])
			codeIndex++
			opCode = code[codeIndex]
		case iload_0:
			frame.oprandStack = append(frame.oprandStack, frame.localvars[0])
			codeIndex++
			opCode = code[codeIndex]
		case iload_1:
			frame.oprandStack = append(frame.oprandStack, frame.localvars[1])
			codeIndex++
			opCode = code[codeIndex]
		case iload_2:
			frame.oprandStack = append(frame.oprandStack, frame.localvars[2])
			codeIndex++
			opCode = code[codeIndex]
		case iload_3:
			frame.oprandStack = append(frame.oprandStack, frame.localvars[3])
			codeIndex++
			opCode = code[codeIndex]
		case istore:
			codeIndex++
			frame.localvars[code[codeIndex]] = frame.oprandStack[len(frame.oprandStack)-1].(int)
			frame.oprandStack = frame.oprandStack[:len(frame.oprandStack)-1]
			codeIndex++
			opCode = code[codeIndex]
		case istore_0:
			frame.localvars[0] = frame.oprandStack[len(frame.oprandStack)-1].(int)
			frame.oprandStack = frame.oprandStack[:len(frame.oprandStack)-1]
			codeIndex++
			opCode = code[codeIndex]
		case istore_1:
			frame.localvars[1] = frame.oprandStack[len(frame.oprandStack)-1].(int)
			frame.oprandStack = frame.oprandStack[:len(frame.oprandStack)-1]
			codeIndex++
			opCode = code[codeIndex]
		case istore_2:
			frame.localvars[2] = frame.oprandStack[len(frame.oprandStack)-1].(int)
			frame.oprandStack = frame.oprandStack[:len(frame.oprandStack)-1]
			codeIndex++
			opCode = code[codeIndex]
		case istore_3:
			frame.localvars[3] = frame.oprandStack[len(frame.oprandStack)-1].(int)
			frame.oprandStack = frame.oprandStack[:len(frame.oprandStack)-1]
			codeIndex++
			opCode = code[codeIndex]
		case iadd:
			vright = frame.oprandStack[len(frame.oprandStack)-1]
			vleft = frame.oprandStack[len(frame.oprandStack)-2]
			frame.oprandStack = frame.oprandStack[:len(frame.oprandStack)-2]
			frame.oprandStack = append(frame.oprandStack, vleft.(int)+vright.(int))
			codeIndex++
			opCode = code[codeIndex]
		case sadd:
			vright = frame.oprandStack[len(frame.oprandStack)-1]
			vleft = frame.oprandStack[len(frame.oprandStack)-2]
			frame.oprandStack = frame.oprandStack[:len(frame.oprandStack)-2]
			frame.oprandStack = append(frame.oprandStack, vleft.(string)+vright.(string))
			codeIndex++
			opCode = code[codeIndex]
		case isub:
			vright = frame.oprandStack[len(frame.oprandStack)-1]
			vleft = frame.oprandStack[len(frame.oprandStack)-2]
			frame.oprandStack = frame.oprandStack[:len(frame.oprandStack)-2]
			frame.oprandStack = append(frame.oprandStack, vleft.(int)-vright.(int))
			codeIndex++
			opCode = code[codeIndex]
		case imul:
			vright = frame.oprandStack[len(frame.oprandStack)-1]
			vleft = frame.oprandStack[len(frame.oprandStack)-2]
			frame.oprandStack = frame.oprandStack[:len(frame.oprandStack)-2]
			frame.oprandStack = append(frame.oprandStack, vleft.(int)*vright.(int))
			codeIndex++
			opCode = code[codeIndex]
		case idiv:
			vright = frame.oprandStack[len(frame.oprandStack)-1]
			vleft = frame.oprandStack[len(frame.oprandStack)-2]
			frame.oprandStack = frame.oprandStack[:len(frame.oprandStack)-2]
			frame.oprandStack = append(frame.oprandStack, vleft.(int)/vright.(int))
			codeIndex++
			opCode = code[codeIndex]
		case iinc:
			codeIndex++
			varIndex := code[codeIndex]
			codeIndex++
			offset := code[codeIndex]
			frame.localvars[varIndex] = frame.localvars[varIndex] + offset
			codeIndex++
			opCode = code[codeIndex]
		case ireturn:
			fallthrough
		case s_return:
			var retValue any
			if opCode == ireturn {
				retValue = frame.oprandStack[len(frame.oprandStack)-1]
				frame.oprandStack = frame.oprandStack[:len(frame.oprandStack)-1]
			}
			v.callStack = v.callStack[:len(v.callStack)-1]
			if len(v.callStack) == 0 {
				return 0
			} else {
				frame = v.callStack[len(v.callStack)-1]
				if opCode == ireturn {
					frame.oprandStack = append(frame.oprandStack, retValue)
				}
				if frame.functionSym.ByteCode != nil {
					code = frame.functionSym.ByteCode
					codeIndex = frame.returnIndex
					opCode = code[codeIndex]
				} else {
					fmt.Println("Can not find code for ", frame.functionSym.Name)
					return -1
				}
			}
		case invokestatic:
			codeIndex++
			byte1 = code[codeIndex]
			codeIndex++
			byte2 = code[codeIndex]

			functionSym = bcModule.consts[byte1<<8|byte2].(*parser.FunctionSymbol)

			if functionSym.Name == "println" {
				param := frame.oprandStack[len(frame.oprandStack)-1]
				frame.oprandStack = frame.oprandStack[:len(frame.oprandStack)-1]
				codeIndex++
				opCode = code[codeIndex]
				fmt.Println(param)
			} else if functionSym.Name == "tick" {
			} else if functionSym.Name == "integer_to_string" {
				codeIndex++
				opCode = code[codeIndex]
				numValue = frame.oprandStack[len(frame.oprandStack)-1].(int)
				frame.oprandStack = frame.oprandStack[:len(frame.oprandStack)-1]
				frame.oprandStack = append(frame.oprandStack, strconv.Itoa(numValue))
			} else {
				frame.returnIndex = codeIndex + 1

				lastFrame := frame
				frame = NewStackFrame(functionSym)
				v.callStack = append(v.callStack, frame)

				paramCount := len(functionSym.TheType.(*parser.FunctionType).ParamTypes)
				for i := paramCount - 1; i >= 0; i-- {
					frame.localvars[i] = lastFrame.oprandStack[len(lastFrame.oprandStack)-1].(int)
					lastFrame.oprandStack = lastFrame.oprandStack[:len(lastFrame.oprandStack)-1]
				}

				if frame.functionSym.ByteCode != nil {
					code = frame.functionSym.ByteCode
					codeIndex = 0
					opCode = code[codeIndex]
				} else {
					fmt.Println("Can not find code for ", frame.functionSym.Name)
					return -1
				}
			}

		case ifeq:
			codeIndex++
			byte1 = code[codeIndex]
			codeIndex++
			byte2 = code[codeIndex]
			tmp := frame.oprandStack[len(frame.oprandStack)-1]
			frame.oprandStack = frame.oprandStack[:len(frame.oprandStack)-1]
			if tmp == 0 {
				codeIndex = byte1<<8 | byte2
				opCode = code[codeIndex]
			} else {
				codeIndex++
				opCode = code[codeIndex]
			}

		case ifne:
			codeIndex++
			byte1 = code[codeIndex]
			codeIndex++
			byte2 = code[codeIndex]
			tmp := frame.oprandStack[len(frame.oprandStack)-1]
			frame.oprandStack = frame.oprandStack[:len(frame.oprandStack)-1]
			if tmp != 0 {
				codeIndex = byte1<<8 | byte2
				opCode = code[codeIndex]
			} else {
				codeIndex++
				opCode = code[codeIndex]
			}

		case if_icmplt:
			codeIndex++
			byte1 = code[codeIndex]
			codeIndex++
			byte2 = code[codeIndex]
			vright = frame.oprandStack[len(frame.oprandStack)-1]
			vleft = frame.oprandStack[len(frame.oprandStack)-2]
			frame.oprandStack = frame.oprandStack[:len(frame.oprandStack)-2]
			if vleft.(int) < vright.(int) {
				codeIndex = byte1<<8 | byte2
				opCode = code[codeIndex]
			} else {
				codeIndex++
				opCode = code[codeIndex]
			}

		case if_icmpge:
			codeIndex++
			byte1 = code[codeIndex]
			codeIndex++
			byte2 = code[codeIndex]
			vright = frame.oprandStack[len(frame.oprandStack)-1]
			vleft = frame.oprandStack[len(frame.oprandStack)-2]
			frame.oprandStack = frame.oprandStack[:len(frame.oprandStack)-2]
			if vleft.(int) >= vright.(int) {
				codeIndex = byte1<<8 | byte2
				opCode = code[codeIndex]
			} else {
				codeIndex++
				opCode = code[codeIndex]
			}

		case if_icmpgt:
			codeIndex++
			byte1 = code[codeIndex]
			codeIndex++
			byte2 = code[codeIndex]
			vright = frame.oprandStack[len(frame.oprandStack)-1]
			vleft = frame.oprandStack[len(frame.oprandStack)-2]
			frame.oprandStack = frame.oprandStack[:len(frame.oprandStack)-2]
			if vleft.(int) > vright.(int) {
				codeIndex = byte1<<8 | byte2
				opCode = code[codeIndex]
			} else {
				codeIndex++
				opCode = code[codeIndex]
			}

		case if_icmple:
			codeIndex++
			byte1 = code[codeIndex]
			codeIndex++
			byte2 = code[codeIndex]
			vright = frame.oprandStack[len(frame.oprandStack)-1]
			vleft = frame.oprandStack[len(frame.oprandStack)-2]
			frame.oprandStack = frame.oprandStack[:len(frame.oprandStack)-2]
			if vleft.(int) <= vright.(int) {
				codeIndex = byte1<<8 | byte2
				opCode = code[codeIndex]
			} else {
				codeIndex++
				opCode = code[codeIndex]
			}

		case s_goto:
			codeIndex++
			byte1 = code[codeIndex]
			codeIndex++
			byte2 = code[codeIndex]
			codeIndex = byte1<<8 | byte2
			opCode = code[codeIndex]

		default:
			fmt.Printf("Unknown op code: %x\n", opCode)
			return -2
		}
	}
}

type StackFrame struct {
	functionSym *parser.FunctionSymbol
	returnIndex int
	localvars   []int
	oprandStack []any
}

func NewStackFrame(functionSym *parser.FunctionSymbol) *StackFrame {
	return &StackFrame{
		functionSym: functionSym,
		localvars:   make([]int, len(functionSym.Vars)),
		oprandStack: make([]any, 0),
		returnIndex: 0,
	}
}

// 生成字节码
type BCModuleWriter struct {
	types []parser.IType
}

func NewBCModuleWriter() *BCModuleWriter {
	return &BCModuleWriter{
		types: make([]parser.IType, 0),
	}
}

func (b *BCModuleWriter) Write(bcModule *BCModule) []int {
	var bc2 []int
	b.types = []parser.IType{}

	numConsts := 0
	for _, c := range bcModule.consts {
		cc := reflect.ValueOf(c)
		if isNumber(cc) {
			bc2 = append(bc2, 1)
			bc2 = append(bc2, c.(int))
			numConsts++
		} else if _, ok := c.(string); ok {
			bc2 = append(bc2, 2)
			b.writeString(&bc2, c.(string))
			numConsts++
		} else if functionSym, ok := c.(*parser.FunctionSymbol); ok {
			if _, ok := parser.Built_ins[functionSym.Name]; !ok {
				bc2 = append(bc2, 3)
				bc2 = append(bc2, b.writeFunctionSymbol(functionSym)...)
				numConsts++
			}
		} else {
			fmt.Println("Unsupported const in BCModuleWriter.")
			fmt.Println(c)
		}
	}

	var bc1 []int
	b.writeString(&bc1, "types")
	bc1 = append(bc1, len(b.types))
	for _, t := range b.types {
		if parser.IsFunctionType(t) {
			bc1 = append(bc1, b.writeFunctionType(t.(*parser.FunctionType))...)
		} else if parser.IsSimpleType(t) {
			bc1 = append(bc1, b.writeSimpleType(t.(*parser.SimpleType))...)
		} else if parser.IsUnionType(t) {
			bc1 = append(bc1, b.writeUnionType(t.(*parser.UnionType))...)
		} else {
			fmt.Println("Unsupported type in BCModuleWriter")
			fmt.Println(t)
		}
	}

	b.writeString(&bc1, "consts")
	bc1 = append(bc1, numConsts)

	return append(bc1, bc2...)
}

func (b *BCModuleWriter) writeVarSymbol(sym *parser.VarSymbol) []int {
	var bc []int
	b.writeString(&bc, sym.Name)
	b.writeString(&bc, sym.TheType.GetName())
	if !parser.IsSysType(sym.TheType) && slices.Index(b.types, sym.TheType) == -1 {
		b.types = append(b.types, sym.TheType)
	}

	return bc
}

func (b *BCModuleWriter) writeFunctionSymbol(sym *parser.FunctionSymbol) []int {
	var bc []int

	b.writeString(&bc, sym.Name)
	b.writeString(&bc, sym.TheType.GetName())
	if !parser.IsSysType(sym.TheType) && slices.Index(b.types, sym.TheType) == -1 {
		b.types = append(b.types, sym.TheType)
	}

	bc = append(bc, sym.OpStackSize)
	bc = append(bc, len(sym.Vars))

	for _, v := range sym.Vars {
		bc = append(bc, b.writeVarSymbol(v)...)
	}

	if sym.ByteCode == nil {
		bc = append(bc, 0)
	} else {
		bc = append(bc, len(sym.ByteCode))
		bc = append(bc, sym.ByteCode...)
	}

	return bc
}

func (b *BCModuleWriter) writeSimpleType(sym *parser.SimpleType) []int {
	var bc []int

	if parser.IsSysType(sym) {
		return bc
	}
	bc = append(bc, 1)
	b.writeString(&bc, sym.Name)
	bc = append(bc, len(sym.UpperTypes))
	for _, ut := range sym.UpperTypes {
		b.writeString(&bc, ut.GetName())
		if !parser.IsSysType(ut) && slices.Index(b.types, ut) == -1 {
			b.types = append(b.types, ut)
		}
	}

	return bc
}
func (b *BCModuleWriter) writeFunctionType(sym *parser.FunctionType) []int {
	var bc []int

	bc = append(bc, 2)
	b.writeString(&bc, sym.Name)

	b.writeString(&bc, sym.ReturnType.GetName())
	bc = append(bc, len(sym.ParamTypes))

	for _, pt := range sym.ParamTypes {
		b.writeString(&bc, pt.GetName())
		if slices.Index(b.types, pt) == -1 {
			b.types = append(b.types, pt)
		}
	}

	return bc
}

func (b *BCModuleWriter) writeUnionType(sym *parser.UnionType) []int {
	var bc []int

	bc = append(bc, 3)
	b.writeString(&bc, sym.Name)

	for _, ut := range sym.Types {
		b.writeString(&bc, ut.GetName())
		if slices.Index(b.types, ut) == -1 {
			b.types = append(b.types, ut)
		}
	}

	return bc
}

func (b *BCModuleWriter) writeString(bc *[]int, str string) {
	*bc = append(*bc, len(str))
	for i := 0; i < len(str); i++ {
		*bc = append(*bc, int(str[i]))
	}
}

type BCModuleReader struct {
	index     int
	types     map[string]parser.IType
	typeInfos map[string]any
}

func NewBCModuleReader() *BCModuleReader {
	return &BCModuleReader{
		index:     0,
		types:     map[string]parser.IType{},
		typeInfos: map[string]any{},
	}
}

func (r *BCModuleReader) Read(bc []int) *BCModule {
	r.index = 0
	r.types = map[string]parser.IType{}

	bcModule := NewBCModule()

	r.addSystemTypes()

	str := r.readString(bc)
	if str != "types" {
		panic("从字节码中读取的字符串不是'types'")
	}
	numTypes := bc[r.index]
	r.index++
	for i := 0; i < numTypes; i++ {
		typeKind := bc[r.index]
		r.index++
		switch typeKind {
		case 1:
			r.readSimpleType(bc)
		case 2:
			r.readFunctionType(bc)
		case 3:
			r.readUnionType(bc)
		default:
			fmt.Println("Unsupported type kind:", typeKind)
		}
	}

	r.buildTypes()

	str = r.readString(bc)
	if str != "consts" {
		panic("从字节码中读取的字符串不是'consts'")
	}
	numConsts := bc[r.index]
	r.index++
	for range numConsts {
		constType := bc[r.index]
		r.index++
		switch constType {
		case 1:
			bcModule.consts = append(bcModule.consts, bc[r.index])
			r.index++
		case 2:
			str := r.readString(bc)
			bcModule.consts = append(bcModule.consts, str)
		case 3:
			functionSym := r.readFunctionSymbol(bc)
			bcModule.consts = append(bcModule.consts, functionSym)
			if functionSym.Name == "main" {
				bcModule._main = functionSym
			}
		default:
			fmt.Println("Unsupported const type:", constType)
		}
	}

	return bcModule
}

func (r *BCModuleReader) readString(bc []int) string {
	blen := bc[r.index]
	r.index++
	var str strings.Builder

	for range blen {
		str.WriteString(string(rune(bc[r.index])))
		r.index++
	}

	return str.String()
}

func (r *BCModuleReader) readSimpleType(bc []int) {
	typeName := r.readString(bc)
	numUpperTypes := bc[r.index]
	r.index++
	upperTypes := []string{}
	for range numUpperTypes {
		upperTypes = append(upperTypes, r.readString(bc))
	}
	t := parser.NewSimpleType(typeName, []parser.IType{})
	r.types[typeName] = t
	r.typeInfos[typeName] = upperTypes
}

type tmpStruct struct {
	returnType string
	paramTypes []string
}

func (r *BCModuleReader) readFunctionType(bc []int) {
	typeName := r.readString(bc)
	returnType := r.readString(bc)
	numParams := bc[r.index]
	r.index++
	paramTypes := []string{}
	for range numParams {
		paramTypes = append(paramTypes, r.readString(bc))
	}

	t := parser.NewFunctionType(parser.Any, []parser.IType{}, typeName)
	r.types[typeName] = t
	r.typeInfos[typeName] = tmpStruct{returnType: returnType, paramTypes: paramTypes} // ?
}

func (r *BCModuleReader) readUnionType(bc []int) {
	typeName := r.readString(bc)
	numTypes := bc[r.index]
	r.index++
	unionTypes := []string{}
	for range numTypes {
		unionTypes = append(unionTypes, r.readString(bc))
	}

	t := parser.NewUnionType([]parser.IType{}, typeName)
	r.types[typeName] = t
	r.typeInfos[typeName] = unionTypes
}
func (r *BCModuleReader) addSystemTypes() {
	r.types["any"] = parser.Any
	r.types["number"] = parser.Number
	r.types["string"] = parser.String
	r.types["integer"] = parser.Integer
	r.types["decimal"] = parser.Decimal
	r.types["boolean"] = parser.Boolean
	r.types["null"] = parser.Null
	r.types["undefined"] = parser.Undefined
	r.types["void"] = parser.Void
}

func (r *BCModuleReader) buildTypes() {
	for typeName, _ := range r.typeInfos {
		t := r.types[typeName]
		if parser.IsSimpleType(t) {
			simpleType := t.(*parser.SimpleType)
			upperTypes := r.typeInfos[typeName].([]string)
			for _, utName := range upperTypes {
				ut := r.types[utName]
				simpleType.UpperTypes = append(simpleType.UpperTypes, ut)
			}
		} else if parser.IsFunctionType(t) {
			functionType := t.(*parser.FunctionType)
			returnType := r.typeInfos[typeName].(tmpStruct).returnType
			paramTypes := r.typeInfos[typeName].(tmpStruct).paramTypes
			functionType.ReturnType = r.types[returnType]
			for _, utName := range paramTypes {
				ut := r.types[utName]
				functionType.ParamTypes = append(functionType.ParamTypes, ut)
			}
		} else if parser.IsUnionType(t) {
			unionType := t.(*parser.UnionType)
			types := r.typeInfos[typeName].([]string)
			for _, utName := range types {
				ut := r.types[utName]
				unionType.Types = append(unionType.Types, ut)
			}
		} else {
			fmt.Println("Unsupported type in BCModuleReader.")
			fmt.Println(t)
		}
	}

	r.typeInfos = make(map[string]any)
}

func (r *BCModuleReader) readFunctionSymbol(bc []int) *parser.FunctionSymbol {
	functionName := r.readString(bc)
	typeName := r.readString(bc)
	functionType := r.types[typeName].(*parser.FunctionType)
	opStackSize := bc[r.index]
	r.index++
	numVars := bc[r.index]
	r.index++

	vars := []*parser.VarSymbol{}
	for range numVars {
		vars = append(vars, r.readVarSymbol(bc))
	}

	numByteCodes := bc[r.index]
	r.index++
	var byteCodes []int
	if numByteCodes == 0 {
		byteCodes = nil
	} else {
		byteCodes = bc[r.index : r.index+numByteCodes]
		r.index += numByteCodes
	}

	functionSym := parser.NewFunctionSymbol(functionName, functionType, vars)
	functionSym.OpStackSize = opStackSize
	functionSym.ByteCode = byteCodes

	return functionSym
}

func (r *BCModuleReader) readVarSymbol(bc []int) *parser.VarSymbol {
	varName := r.readString(bc)
	typeName := r.readString(bc)
	varType := r.types[typeName]

	return parser.NewVarSymbol(varName, varType)
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
