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

// 指令及编码，参考了JVM的操作码
const (
	iconst_0     = 0x03
	iconst_1     = 0x04
	iconst_2     = 0x05
	iconst_3     = 0x06
	iconst_4     = 0x07
	iconst_5     = 0x08
	bipush       = 0x10 // 8位整数入栈
	sipush       = 0x11 // 16位整数入栈
	ldc          = 0x12 // 从常量池加载，load const
	iload        = 0x15 // 把指定下标的本地变量入栈
	iload_0      = 0x1a // 把指令和操作数压缩在一起，减少字节码
	iload_1      = 0x1b
	iload_2      = 0x1c
	iload_3      = 0x1d
	istore       = 0x36 // 把栈顶的变量弹出并保存到指定下标的变量
	istore_0     = 0x3b
	istore_1     = 0x3c
	istore_2     = 0x3d
	istore_3     = 0x3e
	iadd         = 0x60 // 二元运算
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
	s_goto       = 0xa7 // 名称冲突，加个's_'
	ireturn      = 0xac // 返回
	s_return     = 0xb1 // 名称冲突，加个's_'
	invokestatic = 0xb8 // 调用函数

	//自行扩展的操作码
	sadd = 0x61 //字符串连接
	sldc = 0x13 //把字符串常量入栈。字符串放在常量区，用两个操作数记录下标。
)

func GetOpCode(op int) string {
	s := map[int]string{
		0x03: "iconst_0",
		0x04: "iconst_1",
		0x05: "iconst_2",
		0x06: "iconst_3",
		0x07: "iconst_4",
		0x08: "iconst_5",
		0x10: "bipush",
		0x11: "sipush",
		0x12: "ldc",
		0x15: "iload",
		0x1a: "iload_0",
		0x1b: "iload_1",
		0x1c: "iload_2",
		0x1d: "iload_3",
		0x36: "istore",
		0x3b: "istore_0",
		0x3c: "istore_1",
		0x3d: "istore_2",
		0x3e: "istore_3",
		0x60: "iadd",
		0x64: "isub",
		0x68: "imul",
		0x6c: "idiv",
		0x84: "iinc",
		0x94: "lcmp",
		0x99: "ifeq",
		0x9a: "ifne",
		0x9b: "iflt",
		0x9c: "ifge",
		0x9d: "ifgt",
		0x9e: "ifle",
		0x9f: "if_icmpeq",
		0xa0: "if_icmpne",
		0xa1: "if_icmplt",
		0xa2: "if_icmpge",
		0xa3: "if_icmpgt",
		0xa4: "if_icmple",
		0xa7: "s_goto",
		0xac: "ireturn",
		0xb1: "s_return",
		0xb8: "invokestatic",
		0x61: "sadd",
		0x13: "sldc",
	}
	return s[op]
}

// 字节码模块，代表一个程序。最重要的是常量表（字符串、数字、函数符号）
type BCModule struct {
	consts []any                  // 常量表
	_main  *parser.FunctionSymbol // 入口函数
}

func NewBCModule() *BCModule {
	b := &BCModule{
		consts: make([]any, 0),
	}
	// 把内置函数加入常量表，这里加上排序是因为map遍历顺序不能保证
	for _, fun := range slices.Sorted(maps.Keys(parser.Built_ins)) {
		b.consts = append(b.consts, parser.Built_ins[fun])
	}

	return b
}

// 打印调试信息
type BCModuleDumper struct{}

func (b *BCModuleDumper) Dump(bcModule *BCModule) {
	symbolDumper := new(parser.SymbolDumper)
	// 遍历常量表，打印字符串常量、数值常量、函数符号
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

// 字节码生成器
type BCGenerator struct {
	parser.AstVisitor
	m            BCModule               // 模块
	functionSym  *parser.FunctionSymbol // 当前函数
	inExpression bool                   // 当前节点是否属于表达式
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
		b.m.consts = append(b.m.consts, b.functionSym) // main函数加入常量表
		b.m._main = b.functionSym                      // 设置模块main函数
		// 设置字节码
		b.functionSym.ByteCode = b.VisitBlock(prog.Block, additional).([]int)
	}

	return b.m
}

func (b *BCGenerator) VisitFunctionDecl(functionDecl *parser.FunctionDecl, additional any) any {
	// 设置当前函数符号
	lastFunctionSym := b.functionSym
	b.functionSym = functionDecl.Sym

	// 函数加入常量表
	b.m.consts = append(b.m.consts, b.functionSym)

	// 为函数体生成代码
	code1 := b.Visit(functionDecl.CallSignature, additional)
	code2 := b.Visit(functionDecl.Body, additional)
	// 添加偏移量
	b.addOffsetToJumpOp(code2.([]int), len(code1.([]int)))
	// 设置字节码
	if b.functionSym != nil {
		b.functionSym.ByteCode = append(code1.([]int), code2.([]int)...)
	}

	// 恢复当前函数
	b.functionSym = lastFunctionSym
	return nil
}

func (b *BCGenerator) VisitBlock(block *parser.Block, additional any) any {
	ret := []int{}

	for _, x := range block.Stmts {
		var code []int
		b.inExpression = false      // 每个语句重置属性
		r := b.Visit(x, additional) // 生成字节码
		if r != nil {
			code = r.([]int) // 转为数组
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
	// 变量有初始化值，要做赋值操作
	if variableDecl.Init != nil {
		ret := b.Visit(variableDecl.Init, additional).([]int)
		code = append(code, ret...)
		// 生成变量赋值指令
		code = append(code, (b.setVariableValue(variableDecl.Sym)).([]int)...)
	}

	return code
}

func (b *BCGenerator) VisitReturnStatement(stmt *parser.ReturnStatement, additional any) any {
	var code []int

	// 为return后面的表达式生成代码
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
	// 依次生成参数相关指令，就是把参数压到栈里
	for _, param := range functionCall.Arguments {
		code1 := b.Visit(param, additional)
		code = append(code, code1.([]int)...)
	}
	// 生成invoke指令，查找函数在常量表里的位置
	index := slices.IndexFunc(b.m.consts, func(ele any) bool {
		return ele == functionCall.Sym
	})
	if index == -1 { // 找不到就报错
		panic("生成字节码时，在模块中查找函数失败！")
	}
	code = append(code, invokestatic)
	code = append(code, index>>8)
	code = append(code, index)

	return code
}

// 为if语句生成字节码
func (b *BCGenerator) VisitIfStatement(stmt *parser.IfStatement, additional any) any {
	var code []int

	code_condition := b.Visit(stmt.Condition, additional).([]int)
	b.inExpression = false // 重置

	code_ifBlock := b.Visit(stmt.Stmt, additional).([]int)
	b.inExpression = false

	code_elseBlock := b.Visit(stmt.ElseStmt, additional).([]int)
	b.inExpression = false

	offset_ifBlock := len(code_condition) + 3                       // if语句块的地址
	offset_elseBlock := len(code_condition) + len(code_ifBlock) + 6 // else语句块的地址
	offset_nextStmt := offset_elseBlock + len(code_elseBlock)

	b.addOffsetToJumpOp(code_ifBlock, offset_ifBlock)
	b.addOffsetToJumpOp(code_elseBlock, offset_elseBlock)

	code = append(code, code_condition...)
	// 跳转：去执行else语句块
	code = append(code, ifeq)
	code = append(code, offset_elseBlock>>8)
	code = append(code, offset_elseBlock)
	// 条件为true时执行的语句
	code = append(code, code_ifBlock...)

	code = append(code, s_goto)
	code = append(code, offset_nextStmt>>8)
	code = append(code, offset_nextStmt)
	// 条件为false时执行的语句
	code = append(code, code_elseBlock...)

	return code
}

// 为for循环生成字节码
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

	// 循环条件起始位置
	offset_condition := len(code_init)
	// 循环体起始位置
	offset_stmt := offset_condition + len(code_condition)
	if len(code_condition) > 0 {
		offset_stmt += 3
	}
	// 递增部分起始位置
	offset_increment := offset_stmt + len(code_stmt)
	// 循环结束位置
	offset_nextStmt := offset_increment + len(code_increment) + 3

	b.addOffsetToJumpOp(code_condition, offset_condition)
	b.addOffsetToJumpOp(code_increment, offset_increment)
	b.addOffsetToJumpOp(code_stmt, offset_stmt)

	// 初始化部分
	code = append(code, code_init...)
	// 循环条件
	if len(code_condition) > 0 {
		code = append(code, code_condition...)
		// 根据条件值跳转
		code = append(code, ifeq)
		code = append(code, offset_nextStmt>>8)
		code = append(code, offset_nextStmt)
	}
	// 循环体
	code = append(code, code_stmt...)
	// 递增部分
	code = append(code, code_increment...)
	// 跳回循环条件
	code = append(code, s_goto)
	code = append(code, offset_condition>>8)
	code = append(code, offset_condition)

	return code
}

// 在跳转地址上添加偏移量
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
		// 指令后面带2个字节的操作数
		case iinc, invokestatic, sipush:
			codeIndex += 3
		// 跳转语句，需要加offset
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

// 生成获取本地变量值的指令
func (b *BCGenerator) getVriableValue(sym *parser.VarSymbol) any {
	var code []int

	if sym != nil {
		// 本地变量下标
		index := slices.IndexFunc(b.functionSym.Vars, func(v *parser.VarSymbol) bool {
			return v == sym
		})
		if index == -1 {
			panic("生成字节码时（获取变量的值），在函数符号中获取本地变量下标失败！")
		}
		// 根据不同下标生成指令
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
		// 本地变量的下标
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
	// 处理左右子树代码
	var code []int
	code1 := b.Visit(exp.Exp1, additional)
	code2 := b.Visit(exp.Exp2, additional)

	address1 := 0
	address2 := 0
	tempCode := 0

	if exp.Op == scanner.Assign { // 赋值运算
		varSymbol := code1.(*parser.VarSymbol)
		// fmt.Printf("varSymol: %v\n", varSymbol)
		code = code2.([]int)
		code = append(code, (b.setVariableValue(varSymbol)).([]int)...)
	} else { // 其它二元运算
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

// 左值返回符号，否则生成iload指令
func (b *BCGenerator) VisitVariable(variable *parser.Variable, additional any) any {
	if variable.IsLeftValue() {
		return variable.Sym
	} else {
		return b.getVriableValue(variable.Sym)
	}
}

// 生成常量入栈指令
func (b *BCGenerator) VisitIntegerLiteral(exp *parser.IntegerLiteral, additional any) any {
	var ret []int
	value := exp.Value

	if value >= 0 && value <= 5 { // 0-5使用快捷指令
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
	} else if value >= -128 && value < 128 { // 8位整数用bipush指令
		ret = append(ret, bipush)
		ret = append(ret, value)
	} else if value >= -32768 && value < 32768 { // 16位整数用sipush指令
		ret = append(ret, sipush)
		// 拆成两个字节
		ret = append(ret, value>>8)
		ret = append(ret, value&0x00ff)
	} else { // 大于16位用ldc指令，从常量池中取
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

// 虚拟机
type VM struct {
	callStack []*StackFrame
}

func NewVM() *VM {
	return &VM{
		callStack: []*StackFrame{},
	}
}

// 运行一个模块
func (v *VM) Execute(bcModule *BCModule) int {
	// 找到入口函数
	var functionSym *parser.FunctionSymbol
	if bcModule._main == nil {
		fmt.Println("Can not find main function.")
		return -1
	} else {
		functionSym = bcModule._main
	}
	// 创建栈帧
	frame := NewStackFrame(functionSym)
	v.callStack = append(v.callStack, frame)
	// 当前运行的代码
	var code []int
	if functionSym.ByteCode != nil {
		code = functionSym.ByteCode
	} else {
		fmt.Println("Cant not find code for ", frame.functionSym.Name)
		return -1
	}

	codeIndex := 0            // 当前代码位置
	opCode := code[codeIndex] // 当前操作码
	// 临时变量
	byte1 := 0
	byte2 := 0
	var vleft any
	var vright any
	constIndex := 0
	numValue := 0
	strValue := ""

	// 考虑性能，几乎所有虚拟机核心引擎都是这样放在一个for循环里，读取指令，处理操作
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
		case bipush: // 取出1个字节
			codeIndex++
			frame.oprandStack = append(frame.oprandStack, code[codeIndex])
			codeIndex++
			opCode = code[codeIndex]
		case sipush: // 取出2个字节
			codeIndex++
			byte1 = code[codeIndex]
			codeIndex++
			byte2 = code[codeIndex]
			frame.oprandStack = append(frame.oprandStack, byte1<<8|byte2)
			codeIndex++
			opCode = code[codeIndex]
		case ldc: // 从常量池加载
			codeIndex++
			constIndex = code[codeIndex]
			numValue = bcModule.consts[constIndex].(int)
			frame.oprandStack = append(frame.oprandStack, numValue)
			codeIndex++
			opCode = code[codeIndex]
		case sldc: // 从字符串常量池加载
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
		case ireturn, s_return: // 返回值
			var retValue any
			if opCode == ireturn {
				retValue = frame.oprandStack[len(frame.oprandStack)-1]
				frame.oprandStack = frame.oprandStack[:len(frame.oprandStack)-1]
			}
			// 弹出栈帧，返回上一级函数，继续执行
			v.callStack = v.callStack[:len(v.callStack)-1]
			if len(v.callStack) == 0 { // 主程序返回，结束运行
				return 0
			} else { // 返回上一级调用者
				frame = v.callStack[len(v.callStack)-1] // 上一级栈帧
				if opCode == ireturn {
					frame.oprandStack = append(frame.oprandStack, retValue)
				}
				// 设置新的code, index和opcode
				if frame.functionSym.ByteCode != nil {
					code = frame.functionSym.ByteCode // 切换到调用者代码
					codeIndex = frame.returnIndex
					opCode = code[codeIndex]
				} else {
					fmt.Println("Can not find code for ", frame.functionSym.Name)
					return -1
				}
			}
		case invokestatic:
			// 从常量池找到被调用函数
			codeIndex++
			byte1 = code[codeIndex]
			codeIndex++
			byte2 = code[codeIndex]

			functionSym = bcModule.consts[byte1<<8|byte2].(*parser.FunctionSymbol)
			// 处理内置函数
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
				// 设置返回值地址，为函数调用的下一条指令
				frame.returnIndex = codeIndex + 1
				// 创建新栈帧
				lastFrame := frame
				frame = NewStackFrame(functionSym)
				v.callStack = append(v.callStack, frame)

				// 传递参数
				paramCount := len(functionSym.TheType.(*parser.FunctionType).ParamTypes)
				for i := paramCount - 1; i >= 0; i-- {
					// 把上一级调用栈的参数放到本地
					frame.localvars[i] = lastFrame.oprandStack[len(lastFrame.oprandStack)-1].(int)
					lastFrame.oprandStack = lastFrame.oprandStack[:len(lastFrame.oprandStack)-1]
				}
				// 设置新的code、codeIndex和opcode
				if frame.functionSym.ByteCode != nil {
					// 切换到被调用函数的代码
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

// 栈帧
type StackFrame struct {
	functionSym *parser.FunctionSymbol // 当前函数符号
	returnIndex int                    // 返回地址
	localvars   []int                  // 本地变量
	oprandStack []any                  // 操作数栈
}

func NewStackFrame(functionSym *parser.FunctionSymbol) *StackFrame {
	return &StackFrame{
		functionSym: functionSym,
		localvars:   make([]int, len(functionSym.Vars)),
		oprandStack: make([]any, 0),
		returnIndex: 0,
	}
}

// //////////////////////////////////////////////////////////
// 生成字节码
type BCModuleWriter struct {
	types []parser.IType // 保存该模块所涉及的类型
}

func NewBCModuleWriter() *BCModuleWriter {
	return &BCModuleWriter{
		types: make([]parser.IType, 0),
	}
}

// 从bcModule生成字节码
func (b *BCModuleWriter) Write(bcModule *BCModule) []int {
	var bc2 []int
	b.types = []parser.IType{}
	// 写入常量
	numConsts := 0
	for _, c := range bcModule.consts {
		cc := reflect.ValueOf(c)
		if isNumber(cc) {
			bc2 = append(bc2, 1) // 代表接下来是一个number
			bc2 = append(bc2, c.(int))
			numConsts++
		} else if _, ok := c.(string); ok {
			bc2 = append(bc2, 2) // 代表接下来是一个string
			b.writeString(&bc2, c.(string))
			numConsts++
		} else if functionSym, ok := c.(*parser.FunctionSymbol); ok {
			if _, ok := parser.Built_ins[functionSym.Name]; !ok { // 不是内置函数
				bc2 = append(bc2, 3) // 代表接下来是一个functionSymbol
				bc2 = append(bc2, b.writeFunctionSymbol(functionSym)...)
				numConsts++
			}
		} else {
			fmt.Println("Unsupported const in BCModuleWriter.")
			fmt.Println(c)
		}
	}

	// 写入类型
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
	b.writeString(&bc, sym.Name)              // 写入变量名称
	b.writeString(&bc, sym.TheType.GetName()) // 写入类型名称
	if !parser.IsSysType(sym.TheType) && slices.Index(b.types, sym.TheType) == -1 {
		b.types = append(b.types, sym.TheType)
	}

	return bc
}

func (b *BCModuleWriter) writeFunctionSymbol(sym *parser.FunctionSymbol) []int {
	var bc []int

	b.writeString(&bc, sym.Name)              // 写入函数名称
	b.writeString(&bc, sym.TheType.GetName()) // 写入类型名称
	if !parser.IsSysType(sym.TheType) && slices.Index(b.types, sym.TheType) == -1 {
		b.types = append(b.types, sym.TheType)
	}

	bc = append(bc, sym.OpStackSize) // 写入栈大小
	bc = append(bc, len(sym.Vars))   // 写入本地变量个数
	// 写入变量
	for _, v := range sym.Vars {
		bc = append(bc, b.writeVarSymbol(v)...)
	}

	if sym.ByteCode == nil { // 内置函数
		bc = append(bc, 0)
	} else { // 自定义函数
		bc = append(bc, len(sym.ByteCode))
		bc = append(bc, sym.ByteCode...)
	}

	return bc
}

func (b *BCModuleWriter) writeSimpleType(sym *parser.SimpleType) []int {
	var bc []int

	if parser.IsSysType(sym) { // 内置类型
		return bc
	}
	bc = append(bc, 1)                   // 代表SimpleType
	b.writeString(&bc, sym.Name)         // 写入类型名称
	bc = append(bc, len(sym.UpperTypes)) // 写入父类型数量
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

	bc = append(bc, 2)           // 代表FunctionType
	b.writeString(&bc, sym.Name) // 写入类型名称

	b.writeString(&bc, sym.ReturnType.GetName()) // 写入返回值名称
	bc = append(bc, len(sym.ParamTypes))         // 写入参数类型数量

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

	bc = append(bc, 3) // 代表UnionType
	b.writeString(&bc, sym.Name)

	for _, ut := range sym.Types {
		b.writeString(&bc, ut.GetName())
		if slices.Index(b.types, ut) == -1 {
			b.types = append(b.types, ut)
		}
	}

	return bc
}

// 把字符串添加到字节码数组中
func (b *BCModuleWriter) writeString(bc *[]int, str string) {
	*bc = append(*bc, len(str)) // 写入字符串长度
	for i := 0; i < len(str); i++ {
		*bc = append(*bc, int(str[i]))
	}
}

// 读取字节码生成BCModule
type BCModuleReader struct {
	index     int                     // 当前读取的字节码下标
	types     map[string]parser.IType // 解析出的所有类型
	typeInfos map[string]any
}

func NewBCModuleReader() *BCModuleReader {
	return &BCModuleReader{
		index:     0,
		types:     map[string]parser.IType{},
		typeInfos: map[string]any{},
	}
}

// 从字节码生成BCModule
func (r *BCModuleReader) Read(bc []int) *BCModule {
	// 重置状态变量
	r.index = 0
	r.types = map[string]parser.IType{}

	bcModule := NewBCModule()
	// 读取类型，加入内置类型
	r.addSystemTypes()
	// 从字节码中读取类型
	str := r.readString(bc)
	if str != "types" {
		panic("从字节码中读取的字符串不是'types'")
	}
	numTypes := bc[r.index]
	r.index++
	for range numTypes {
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

	// 读取常量
	str = r.readString(bc)
	if str != "consts" {
		panic("从字节码中读取的字符串不是'consts'")
	}
	numConsts := bc[r.index]
	r.index++
	for range numConsts {
		constType := bc[r.index]
		r.index++
		if constType == 1 {
			bcModule.consts = append(bcModule.consts, bc[r.index])
			r.index++
		} else if constType == 2 {
			str := r.readString(bc)
			bcModule.consts = append(bcModule.consts, str)
		} else if constType == 3 {
			functionSym := r.readFunctionSymbol(bc)
			bcModule.consts = append(bcModule.consts, functionSym)
			if functionSym.Name == "main" {
				bcModule._main = functionSym
			}
		} else {
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

// 添加内置系统类型
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

// 生成类型，并建立类型之间正确的引用关系
func (r *BCModuleReader) buildTypes() {
	for typeName := range r.typeInfos {
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

// 从字节码中读取FunctionSymbol
func (r *BCModuleReader) readFunctionSymbol(bc []int) *parser.FunctionSymbol {
	functionName := r.readString(bc) // 函数名称
	typeName := r.readString(bc)     // 类型名称
	functionType := r.types[typeName].(*parser.FunctionType)
	opStackSize := bc[r.index] // 操作数栈大小
	r.index++
	numVars := bc[r.index] // 局部变量个数
	r.index++
	// 读取变量
	vars := []*parser.VarSymbol{}
	for range numVars {
		vars = append(vars, r.readVarSymbol(bc))
	}

	numByteCodes := bc[r.index]
	r.index++
	byteCodes := []int{}
	if numByteCodes == 0 {
		byteCodes = nil
	} else {
		byteCodes = bc[r.index : r.index+numByteCodes]
		r.index += numByteCodes
	}
	// 创建函数符号
	functionSym := parser.NewFunctionSymbol(functionName, functionType, vars)
	functionSym.OpStackSize = opStackSize
	functionSym.ByteCode = byteCodes

	return functionSym
}

// 从字节码中读取VarSymbol
func (r *BCModuleReader) readVarSymbol(bc []int) *parser.VarSymbol {
	varName := r.readString(bc)  // 变量名称
	typeName := r.readString(bc) // 类型名称
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
