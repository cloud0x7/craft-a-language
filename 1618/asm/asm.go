package asm

import (
	"craft-language/parser"
	"craft-language/scanner"
	"fmt"
	"maps"
	"reflect"
	"slices"
	"strings"
)

type OpCode int

const (
	jmp OpCode = iota
	je
	jne
	jle
	jl
	jge
	jg
)
const (
	sete OpCode = iota + 20
	setne
	setl
	setle
	setg
	setge
)
const (
	//8字节指令
	movq OpCode = iota + 40
	addq
	subq
	mulq
	imulq
	divq
	idivq
	negq
	incq
	decq
	xorq
	orq
	andq
	notq
	leaq
	callq
	retq
	pushq
	popq
	cmpq
)
const (
	//4字节指令
	movl OpCode = iota + 80
	addl
	subl
	mull
	imull
	divl
	idivl
	negl
	incl
	decl
	xorl
	orl
	andl
	notl
	leal
	calll
	retl
	pushl
	popl
	cmpl
)
const (
	//2字节指令
	movw OpCode = iota + 120
	addw
	subw
	mulw
	imulw
	divw
	idivw
	negw
	incw
	decw
	xorw
	orw
	andw
	notw
	leaw
	callw
	retw
	pushw
	popw
	cmpw
)
const (
	//单字节指令
	movb OpCode = iota + 160
	addb
	subb
	mulb  //无符号乘
	imulb //有符号乘
	divb  //无符号除
	idivb //有符号除
	negb
	incb
	decb
	xorb
	orb
	andb
	notb
	leab
	callb
	retb
	pushb
	popb
	cmpb
)

var OpCodeMap = map[OpCode]string{
	0: "jmp",
	1: "je",
	2: "jne",
	3: "jle",
	4: "jl",
	5: "jge",
	6: "jg",

	20: "sete",
	21: "setne",
	22: "setl",
	23: "setle",
	24: "setg",
	25: "setge",

	40: "movq",
	41: "addq",
	42: "subq",
	43: "mulq",
	44: "imulq",
	45: "divq",
	46: "idivq",
	47: "negq",
	48: "incq",
	49: "decq",
	50: "xorq",
	51: "orq",
	52: "andq",
	53: "notq",
	54: "leaq",
	55: "callq",
	56: "retq",
	57: "pushq",
	58: "popq",
	59: "cmpq",

	80: "movl",
	81: "addl",
	82: "subl",
	83: "mull",
	84: "imull",
	85: "divl",
	86: "idivl",
	87: "negl",
	88: "incl",
	89: "decl",
	90: "xorl",
	91: "orl",
	92: "andl",
	93: "notl",
	94: "leal",
	95: "calll",
	96: "retl",
	97: "pushl",
	98: "popl",
	99: "cmpl",

	120: "movw",
	121: "addw",
	122: "subw",
	123: "mulw",
	124: "imulw",
	125: "divw",
	126: "idivw",
	127: "negw",
	128: "incw",
	129: "decw",
	130: "xorw",
	131: "orw",
	132: "andw",
	133: "notw",
	134: "leaw",
	135: "callw",
	136: "retw",
	137: "pushw",
	138: "popw",
	139: "cmpw",

	160: "movb",
	161: "addb",
	162: "subb",
	163: "mulb",  //无符号乘
	164: "imulb", //有符号乘
	165: "divb",  //无符号除
	166: "idivb", //有符号除
	167: "negb",
	168: "incb",
	169: "decb",
	170: "xorb",
	171: "orb",
	172: "andb",
	173: "notb",
	174: "leab",
	175: "callb",
	176: "retb",
	177: "pushb",
	178: "popb",
	179: "cmpb",
}

type IInst interface {
	toString() string
	getOp() int
}
type AbsInst struct {
	op OpCode
}

func (i *AbsInst) toString() string {
	return OpCodeMap[i.op]
}
func (i *AbsInst) getOp() int {
	return int(i.op)
}

type Inst_0 struct {
	AbsInst
}

func NewInst_0(op OpCode) *Inst_0 {
	return &Inst_0{AbsInst{op}}
}

type Inst_1 struct {
	AbsInst
	oprand IOprand
}

func NewInst_1(op OpCode, oprand IOprand) *Inst_1 {
	return &Inst_1{AbsInst{op}, oprand}
}
func (i *Inst_1) toString() string {
	return OpCodeMap[i.op] + "\t" + i.oprand.toString()
}
func isInst_1(inst IInst) bool {
	_, ok := inst.(*Inst_1)
	return ok
}

type Inst_2 struct {
	AbsInst
	oprand1 IOprand
	oprand2 IOprand
}

func NewInst_2(op OpCode, oprand1 IOprand, oprand2 IOprand) *Inst_2 {
	return &Inst_2{AbsInst{op}, oprand1, oprand2}
}
func (i *Inst_2) toString() string {
	return OpCodeMap[i.op] + "\t" + i.oprand1.toString() + ", " + i.oprand2.toString()
}

func isInst_2(inst IInst) bool {
	_, ok := inst.(*Inst_2)
	return ok
}

type IOprand interface {
	toString() string
}
type Oprand struct {
	kind  OprandKind
	value any
}

func NewOprand(kind OprandKind, value any) *Oprand {
	return &Oprand{kind, value}
}
func (o *Oprand) isSame(oprand1 *Oprand) bool {
	return o.kind == oprand1.kind && o.value == oprand1.value
}
func (o *Oprand) toString() string {
	if o.kind == bb {
		if reflect.TypeOf(o.value).Kind() == reflect.String {
			return o.value.(string)
		}
		return o.value.(*BasicBlock).toString()
	} else if o.kind == immediate {
		return "$" + fmt.Sprintf("%d", o.value)
	} else if o.kind == returnSlot {
		return "returnSlot"
	} else {
		return o.kind.toString() + fmt.Sprintf("(%d)", o.value)
	}
}

type FunctionOprand struct {
	Oprand
	args       []*Oprand
	returnType parser.IType
}

func NewFunctionOprand(functionName string, args []*Oprand, returnType parser.IType) *FunctionOprand {
	return &FunctionOprand{*NewOprand(function, functionName), args, returnType}
}
func (f *FunctionOprand) toString() string {
	return "_" + f.value.(string)
}

type OprandKind int

const (
	//抽象度较高的操作数
	varIndex    OprandKind = iota
	returnSlot             //用于存放返回值的位置（通常是一个寄存器）
	bb                     //跳转指令指向的基本块
	function               //函数调用
	stringConst            //字符串常量

	//抽象度较低的操作数
	register  //物理寄存器
	memory    //内存访问
	immediate //立即数

	//cmp指令的结果，是设置寄存器的标志位
	//后面可以根据flag和比较操作符的类型，来确定后续要生成的代码
	flag
)

var OprandKindString = []string{
	"varIndex",
	"returnSlot",
	"bb",
	"function",
	"stringConst",
	"register",
	"memory",
	"immediate",
	"flag",
}

func (o OprandKind) toString() string {
	return OprandKindString[o]
}

// 基本块
type BasicBlock struct {
	insts         []IInst // 基本块内的指令
	funIndex      int     // 函数编号
	bbIndex       int     // 基本块编号
	isDestination bool    // 有其他块跳转到该块
}

func NewBasicBlock() *BasicBlock {
	return &BasicBlock{
		insts:         []IInst{},
		funIndex:      -1,
		bbIndex:       -1,
		isDestination: false,
	}
}
func (b *BasicBlock) getName() string {
	if b.bbIndex != -1 && b.funIndex != -1 {
		return fmt.Sprintf("LBB%d_%d", b.funIndex, b.bbIndex)
	} else {
		return "LBB"
	}
}
func (b *BasicBlock) toString() string {
	var str strings.Builder

	if b.isDestination {
		str.WriteString(b.getName() + ":\n")
	} else {
		str.WriteString(fmt.Sprintf("## bb.%d\n", b.bbIndex))
	}

	for _, inst := range b.insts {
		str.WriteString("    " + inst.toString() + "\n")
	}

	return str.String()
}

// 用Asm表示的一个模块。
// 可以输出成为asm文件。
type AsmModule struct {
	// 每个函数对应的指令数组
	fun2Code       map[*parser.FunctionSymbol][]*BasicBlock
	numTotalVars   map[*parser.FunctionSymbol]int
	isLeafFunction map[*parser.FunctionSymbol]bool
	stringConsts   []string // 字符串常量
	// 额外添加用来排序
	fun2CodeName map[string]*parser.FunctionSymbol
}

func NewAsmModule() *AsmModule {
	return &AsmModule{
		fun2Code:       map[*parser.FunctionSymbol][]*BasicBlock{},
		numTotalVars:   map[*parser.FunctionSymbol]int{},
		isLeafFunction: map[*parser.FunctionSymbol]bool{},
		fun2CodeName:   map[string]*parser.FunctionSymbol{},
	}
}
func (a *AsmModule) toString() string {
	var str strings.Builder
	str.WriteString("    .section	__TEXT,__text,regular,pure_instructions\n")

	for _, codeName := range slices.Sorted(maps.Keys(a.fun2CodeName)) {
		fun := a.fun2CodeName[codeName]
		funName := "_" + fun.Name
		str.WriteString("\n    .global " + funName + "\n")
		str.WriteString(funName + ":\n")
		str.WriteString("    .cfi_startproc\n")
		bbs := a.fun2Code[fun]
		for _, bb := range bbs {
			str.WriteString(bb.toString())
		}
		str.WriteString("    .cfi_endproc\n")
	}

	return str.String()
}

type TempStates struct {
	functionSym      *parser.FunctionSymbol
	bbs              []*BasicBlock
	nextTempVarIndex int
	deadTempVars     []int
	tempVarMap       map[parser.IExpression]int
	inExpression     bool
	postfixUnaryInst *Inst_1
}

type AsmGenerator struct {
	parser.AstVisitor

	asmModule  *AsmModule
	returnSlot *Oprand
	s          *TempStates
}

func NewAsmGenerator() *AsmGenerator {
	r := &AsmGenerator{
		asmModule:  NewAsmModule(),
		s:          &TempStates{},
		returnSlot: NewOprand(returnSlot, -1),
	}
	r.AstVisitor.Curr = r
	return r
}

// 分配一个临时变量的下标。尽量复用已经死掉的临时变量
func (a *AsmGenerator) allocateTempVar() int {
	var varIndex int

	if len(a.s.deadTempVars) > 0 {
		varIndex = a.s.deadTempVars[len(a.s.deadTempVars)-1]
		a.s.deadTempVars = a.s.deadTempVars[:len(a.s.deadTempVars)-1]
	} else {
		varIndex = a.s.nextTempVarIndex
		a.s.nextTempVarIndex++
	}
	return varIndex
}

func (a *AsmGenerator) isTempVar(oprand *Oprand) bool {
	if a.s.functionSym != nil {
		return oprand.kind == varIndex && oprand.value.(int) >= len(a.s.functionSym.Vars)
	}
	return false
}
func (a *AsmGenerator) isParamOrLocalVar(oprand Oprand) bool {
	if a.s.functionSym != nil {
		return oprand.kind == varIndex && oprand.value.(int) < len(a.s.functionSym.Vars)
	}
	return false
}

// 如果操作数不同，则生
// 成mov指令；否则，可以减少一次拷贝
func (a *AsmGenerator) movIfNotSame(src *Oprand, dest *Oprand) {
	if !src.isSame(dest) {
		a.getCurrentBB().insts = append(a.getCurrentBB().insts, NewInst_2(movl, src, dest))
	}
}
func (a *AsmGenerator) getCurrentBB() *BasicBlock {
	return a.s.bbs[len(a.s.bbs)-1]
}

// 创建新的基本块
func (a *AsmGenerator) newBlock() *BasicBlock {
	bb := NewBasicBlock()
	a.s.bbs = append(a.s.bbs, bb)

	return bb
}

func (a *AsmGenerator) VisitProg(prog *parser.Prog, additional any) any {
	a.s.functionSym = prog.Sym
	a.s.nextTempVarIndex = len(a.s.functionSym.Vars)

	a.newBlock()
	// a.VisitBlock(prog.Block, additional)
	a.AstVisitor.VisitProg(prog, additional)
	a.asmModule.fun2Code[a.s.functionSym] = a.s.bbs
	a.asmModule.fun2CodeName[a.s.functionSym.Name] = a.s.functionSym
	a.asmModule.numTotalVars[a.s.functionSym] = a.s.nextTempVarIndex

	return a.asmModule
}
func (a *AsmGenerator) VisitFunctionDecl(functionDecl *parser.FunctionDecl, additional any) any {
	s := a.s              // 保存原状态信息
	a.s = new(TempStates) // 建立新状态信息
	a.s.functionSym = functionDecl.Sym
	a.s.nextTempVarIndex = len(a.s.functionSym.Vars)

	a.asmModule.isLeafFunction[a.s.functionSym] = true

	a.newBlock()
	// 生成代码
	a.VisitBlock(functionDecl.Body, additional)
	a.asmModule.fun2Code[a.s.functionSym] = a.s.bbs
	a.asmModule.fun2CodeName[a.s.functionSym.Name] = a.s.functionSym
	a.asmModule.numTotalVars[a.s.functionSym] = a.s.nextTempVarIndex

	a.s = s // 恢复原状态信息

	return nil
}

func (a *AsmGenerator) VisitReturnStatement(stmt *parser.ReturnStatement, additional any) any {
	if stmt.Exp != nil {
		ret := a.Visit(stmt.Exp, additional).(*Oprand)
		//
		//  返回值赋给相应的寄存器
		a.movIfNotSame(ret, a.returnSlot)
	}
	return nil
}
func (a *AsmGenerator) VisitIfStatement(stmt *parser.IfStatement, additional any) any {
	bbCondition := a.getCurrentBB()
	compOprand := a.Visit(stmt.Condition, additional).(*Oprand)
	// if块
	bbIfBlock := a.newBlock()
	a.Visit(stmt.Stmt, additional)
	// else块
	var bbElseBlock *BasicBlock
	if stmt.ElseStmt != nil {
		bbElseBlock = a.newBlock()
		a.Visit(stmt.ElseStmt, additional)
	}
	// 新建基本块，用于if后面的语句
	bbFollowing := a.newBlock()

	op := a.getJumpOpCode(compOprand)
	var instConditionJump *Inst_1
	if bbElseBlock != nil { // 跳转到else块
		instConditionJump = NewInst_1(op, NewOprand(bb, bbElseBlock))
	} else { // 跳转到if之后的块
		instConditionJump = NewInst_1(op, NewOprand(bb, bbFollowing))
	}
	bbCondition.insts = append(bbCondition.insts, instConditionJump)

	if bbElseBlock != nil {
		instIfBlockJump := NewInst_1(jmp, NewOprand(bb, bbFollowing))
		bbIfBlock.insts = append(bbIfBlock.insts, instIfBlockJump)
	}

	return nil
}

func (a *AsmGenerator) getJumpOpCode(compOprand *Oprand) OpCode {
	op := jmp
	switch compOprand.value {
	case scanner.G:
		op = jg
	case scanner.GE:
		op = jge
	case scanner.L:
		op = jl
	case scanner.LE:
		op = jle
	case scanner.EQ:
		op = je
	case scanner.NE:
		op = jne
	default:
		fmt.Println("Unsupported compare operand in conditional expression: " + compOprand.value.(string))
	}

	return op
}

func (a *AsmGenerator) VisitForStatement(stmt *parser.ForStatement, additional any) any {
	if stmt.Init != nil {
		a.Visit(stmt.Init, additional)
	}

	bbCondition := a.newBlock()
	var compOprand *Oprand
	if stmt.Condition != nil {
		compOprand = a.Visit(stmt.Condition, additional).(*Oprand)
	}

	bbBody := a.newBlock()
	a.Visit(stmt.Stmt, additional)

	if stmt.Increment != nil {
		a.Visit(stmt.Increment, additional)
	}

	bbFollowing := a.newBlock()

	if compOprand != nil {
		op := a.getJumpOpCode(compOprand)
		instConditionJump := NewInst_1(op, NewOprand(bb, bbFollowing))
		bbCondition.insts = append(bbCondition.insts, instConditionJump)
	}

	var bbDest *BasicBlock
	if compOprand != nil {
		bbDest = bbCondition
	} else {
		bbDest = bbBody
	}
	instBodyJump := NewInst_1(jmp, NewOprand(bb, bbDest))
	bbBody.insts = append(bbBody.insts, instBodyJump)

	return nil
}

func (a *AsmGenerator) VisitVariableDecl(variableDecl *parser.VariableDecl, additional any) any {
	// 注意，这里原ts文件和js文件中的代码不一致，按照js文件中改写
	fmt.Println("p001")
	if variableDecl.Init != nil && a.s.functionSym != nil {
		fmt.Println("p002")
		right := a.Visit(variableDecl.Init, additional).(*Oprand)
		varIdx := slices.IndexFunc(a.s.functionSym.Vars, func(v *parser.VarSymbol) bool {
			return v == variableDecl.Sym
		})
		left := NewOprand(OprandKind(varIndex), varIdx)

		if a.isParamOrLocalVar(*right) || right.kind == immediate {
			fmt.Println("p003")
			newRight := NewOprand(OprandKind(varIndex), a.allocateTempVar())
			a.getCurrentBB().insts = append(a.getCurrentBB().insts, NewInst_2(movl, right, newRight))
			if a.isTempVar(right) {
				a.s.deadTempVars = append(a.s.deadTempVars, right.value.(int))
			}
			right = newRight
		}
		a.movIfNotSame(right, left)
		return left
	}

	return nil
}

func (a *AsmGenerator) VisitBinary(exp *parser.Binary, additional any) any {
	a.s.inExpression = true
	insts := a.getCurrentBB().insts

	left := a.Visit(exp.Exp1, additional).(*Oprand)
	right := a.Visit(exp.Exp2, additional).(*Oprand)

	dest := NewOprand(left.kind, left.value)
	if !a.isTempVar(dest) {
		dest = NewOprand(varIndex, a.allocateTempVar())
		a.getCurrentBB().insts = append(insts, NewInst_2(movl, left, dest))
	}

	if a.isTempVar(right) {
		a.s.deadTempVars = append(a.s.deadTempVars, right.value.(int))
	}

	switch exp.Op {
	case scanner.Plus:
		if exp.TheType == parser.String {
			var args []*Oprand
			args = append(args, left)
			args = append(args, right)
			a.callIntrinsics("string_concat", args)
		} else {
			a.getCurrentBB().insts = append(a.getCurrentBB().insts, NewInst_2(addl, right, dest))
		}
	case scanner.Minus:
		a.getCurrentBB().insts = append(a.getCurrentBB().insts, NewInst_2(subl, right, dest))
	case scanner.Multiply:
		a.getCurrentBB().insts = append(a.getCurrentBB().insts, NewInst_2(imull, right, dest))
	case scanner.Divide:
		a.getCurrentBB().insts = append(a.getCurrentBB().insts, NewInst_2(idivl, right, dest))
	case scanner.Assign:
		a.getCurrentBB().insts = append(a.getCurrentBB().insts, NewInst_2(movl, right, dest))
		a.movIfNotSame(dest, left)
	case scanner.G, scanner.L, scanner.GE, scanner.LE, scanner.EQ, scanner.NE:
		a.getCurrentBB().insts = append(a.getCurrentBB().insts, NewInst_2(cmpl, right, dest))
		dest = NewOprand(flag, a.getOpsiteOp(exp.Op))
	default:
		fmt.Println("Unsupported OpCode in AsmGenerator.visitBinary: " + exp.Op.ToString())
	}

	a.s.inExpression = false
	return dest
}
func (a *AsmGenerator) getOpsiteOp(op scanner.OP) scanner.OP {
	newOp := op
	switch op {
	case scanner.G:
		newOp = scanner.LE
	case scanner.L:
		newOp = scanner.GE
	case scanner.GE:
		newOp = scanner.L
	case scanner.LE:
		newOp = scanner.G
	case scanner.EQ:
		newOp = scanner.NE
	case scanner.NE:
		newOp = scanner.EQ
	default:
		fmt.Println("Unsupport Op '" + op.ToString() + "' in getOpsiteOpCode.")
	}
	return newOp
}

func (a *AsmGenerator) VisitUnary(exp *parser.Unary, additional any) any {
	insts := a.getCurrentBB().insts
	oprand := a.Visit(exp.Exp, additional).(*Oprand)
	result := oprand

	if exp.Op == scanner.Inc || exp.Op == scanner.Dec {
		tempVar := NewOprand(varIndex, a.allocateTempVar())
		insts = append(insts, NewInst_2(movl, oprand, tempVar))
		if exp.IsPrefix {
			result = tempVar
		} else {
			result = NewOprand(varIndex, a.allocateTempVar())
			insts = append(insts, NewInst_2(movl, oprand, result))
			a.s.deadTempVars = append(a.s.deadTempVars, tempVar.value.(int))
		}
		var opCode OpCode
		if exp.Op == scanner.Inc {
			opCode = addl
		} else {
			opCode = subl
		}
		insts = append(insts, NewInst_2(opCode, NewOprand(immediate, 1), tempVar))
		insts = append(insts, NewInst_2(movl, tempVar, oprand))
	} else if exp.Op == scanner.Plus {
		result = oprand
	} else if exp.Op == scanner.Minus {
		tempVar := NewOprand(varIndex, a.allocateTempVar())
		insts = append(insts, NewInst_2(movl, NewOprand(immediate, 0), tempVar))
		insts = append(insts, NewInst_2(subl, oprand, tempVar))
		result = tempVar
		if a.isTempVar(oprand) {
			a.s.deadTempVars = append(a.s.deadTempVars, oprand.value.(int))
		}
	}
	a.getCurrentBB().insts = insts

	return result
}
func (a *AsmGenerator) VisitExpressionStatement(stmt *parser.ExpressionStatement, additional any) any {
	return a.AstVisitor.VisitExpressionStatement(stmt, additional)
}
func (a *AsmGenerator) VisitVariable(variable *parser.Variable, additional any) any {
	if a.s.functionSym != nil && variable.Sym != nil {
		return NewOprand(varIndex, slices.Index(a.s.functionSym.Vars, variable.Sym))
	}
	return nil
}
func (a *AsmGenerator) VisitIntegerLiteral(exp *parser.IntegerLiteral, additional any) any {
	return NewOprand(immediate, exp.Value)
}
func (a *AsmGenerator) VisitStringLiteral(exp *parser.StringLiteral, additional any) any {
	strIndex := slices.Index(a.asmModule.stringConsts, exp.Value)
	if strIndex == -1 {
		a.asmModule.stringConsts = append(a.asmModule.stringConsts, exp.Value)
		strIndex = len(a.asmModule.stringConsts) - 1
	}

	args := []*Oprand{}
	args = append(args, NewOprand(stringConst, strIndex))
	return a.callIntrinsics("string_create_by_str", args)
}

func (a *AsmGenerator) callIntrinsics(intrinsic string, args []*Oprand) any {
	insts := a.getCurrentBB().insts
	functionSym := parser.Intrinsics["string_create_by_str"]
	functionType := functionSym.TheType.(*parser.FunctionType)

	insts = append(insts, NewInst_1(callq, NewFunctionOprand("string_create_by_str", args, functionType.ReturnType)))

	if functionType.ReturnType != parser.Void {
		dest := NewOprand(varIndex, a.allocateTempVar())
		insts = append(insts, NewInst_2(movl, a.returnSlot, dest))
		a.getCurrentBB().insts = insts
		return dest
	}

	a.getCurrentBB().insts = insts
	return nil
}
func (a *AsmGenerator) VisitFunctionCall(functionCall *parser.FunctionCall, additional any) any {
	a.asmModule.isLeafFunction[a.s.functionSym] = false

	args := []*Oprand{}
	for _, arg := range functionCall.Arguments {
		oprand := a.Visit(arg, additional).(*Oprand)
		args = append(args, oprand)
	}
	functionSym := functionCall.Sym
	functionType := functionSym.TheType.(*parser.FunctionType)

	a.getCurrentBB().insts = append(a.getCurrentBB().insts, NewInst_1(callq, NewFunctionOprand(functionCall.Name, args, functionType.ReturnType)))

	if functionType.ReturnType != parser.Void {
		dest := NewOprand(varIndex, a.allocateTempVar())
		a.getCurrentBB().insts = append(a.getCurrentBB().insts, NewInst_2(movl, a.returnSlot, dest))
		return dest
	}

	return nil
}

// /////////////////////////////////////////////////////////////////////////
// Lower
type Register struct {
	Oprand
	bits int
}

func NewRegister(registerName string, bits int) *Register {
	return &Register{
		Oprand: *NewOprand(register, registerName),
		bits:   bits,
	}
}
func (r *Register) toString() string {
	return "%" + r.value.(string)
}

var (
	// 可供分配的寄存器的数量，16个通用寄存器中，扣除rbp和rsp，然后保留一个寄存器，用来作为与内存变量交换的区域
	numAvailableRegs = 13
	// 32位寄存器
	edi = NewRegister("edi", 32)
	esi = NewRegister("esi", 32)
	edx = NewRegister("edx", 32)
	ecx = NewRegister("ecx", 32)
	r8d = NewRegister("r8d", 32)
	r9d = NewRegister("r9d", 32)
	// 通用寄存器:caller（调用者）负责保护
	r10d = NewRegister("r10d", 32)
	r11d = NewRegister("r11d", 32)
	// 返回值，也由Caller保护
	eax = NewRegister("eax", 32)
	// 通用寄存器:callee（调用者）负责保护
	ebx  = NewRegister("ebx", 32)
	r12d = NewRegister("r12d", 32)
	r13d = NewRegister("r13d", 32)
	r14d = NewRegister("r14d", 32)
	r15d = NewRegister("r15d", 32)
	// 栈顶和栈底
	esp = NewRegister("esp", 32)
	ebp = NewRegister("ebp", 32)

	registers32 = []*Register{
		eax,
		r10d, r11d,
		edi, esi, edx, ecx, r8d, r9d,
		ebx, r12d, r13d, r14d, r15d,
	}
	// 用于传参的寄存器
	paramRegisters32 = []*Register{
		edi, esi, edx, ecx, r8d, r9d,
	}
	// Callee保护的寄存器
	calleeProtected32 = []*Register{
		ebx, r12d, r13d, r14d, r15d,
	}
	// Caller保护的寄存器
	callerProtected32 = []*Register{
		edi, esi, edx, ecx, r8d, r9d,
		r10d, r11d,
		eax,
	}

	rdi = NewRegister("rdi", 64)
	rsi = NewRegister("rsi", 64)
	rdx = NewRegister("rdx", 64)
	rcx = NewRegister("rcx", 64)
	r8  = NewRegister("r8", 64)
	r9  = NewRegister("r9", 64)

	r10 = NewRegister("r10", 64)
	r11 = NewRegister("r11", 64)

	rax = NewRegister("rax", 64)

	rbx = NewRegister("rbx", 64)
	r12 = NewRegister("r12", 64)
	r13 = NewRegister("r13", 64)
	r14 = NewRegister("r14", 64)
	r15 = NewRegister("r15", 64)

	rsp = NewRegister("rsp", 64)
	rbp = NewRegister("rbp", 64)

	registers64 = []*Register{
		rax,
		r10, r11,
		rdi, rsi, rdx, rcx, r8, r9,
		rbx, r12, r13, r14, r15,
	}
	calleeProtected64 = []*Register{
		rbx, r12, r13, r14, r15,
	}
	callerProtected64 = []*Register{
		rdi, rsi, rdx, rcx, r8, r9, r10, r11, rax,
	}
)

// 内存寻址，只支持基于寄存器的偏移量
type MemAddress struct {
	Oprand
	register *Register
	offset   int
}

func NewMemAddress(register *Register, offset int) *MemAddress {
	return &MemAddress{
		Oprand:   *NewOprand(memory, "undefined"),
		register: register,
		offset:   offset,
	}
}

// 输出结果类似于：8(%rbp)，如果offset为0，那么不显示，即：(%rbp)
func (m *MemAddress) toString() string {
	var ret string
	if m.offset != 0 {
		ret = fmt.Sprintf("%d", m.offset)
	}
	ret += "(" + m.register.toString() + ")"
	return ret
}

// 对AsmModule做Lower处理。
// 1.把寄存器改成具体的物理寄存器
// 2.把本地变量也换算成具体的内存地址
// 3.把抽象的指令转换成具体的指令
// 4.计算标签名称
// 5.添加序曲和尾声
// 6.内存对齐
type Lower struct {
	usedCallerProtectedRegs []*Register       // 当前函数使用到的那些Caller保护的寄存器
	usedCalleeProtectedRegs []*Register       // 当前函数使用到的那些Callee保护的寄存器
	numTotalVars            int               // 所有变量的总数，包括参数、本地变量和临时变量
	numParams               int               // 当前函数的参数数量
	numLocalVars            int               // 当前函数的本地变量数量
	numTempVars             int               // 临时变量的数量
	lowedVars               map[int]IOprand   // 保存已经被Lower的Oprand，用于提高效率
	numArgsOnStack          int               // 需要在栈里保存的为函数传参（超过6个之后的参数）保留的空间
	rspOffset               int               // rsp应该移动的量
	canUseRedZone           bool              // 是否使用RedZone，也就是栈顶之外的128个字节
	allocatedRegisters      map[*Register]int // 已被分配的寄存器
	asmModule               *AsmModule
}

func NewLower(asmModule *AsmModule) *Lower {
	return &Lower{
		asmModule:               asmModule,
		usedCallerProtectedRegs: []*Register{},
		usedCalleeProtectedRegs: []*Register{},
		lowedVars:               map[int]IOprand{},
		allocatedRegisters:      map[*Register]int{},
		canUseRedZone:           false,
	}
}

func (l *Lower) lowerModule() {
	newFun2Code := map[*parser.FunctionSymbol][]*BasicBlock{}
	funIndex := 0
	for _, codeName := range slices.Sorted(maps.Keys(l.asmModule.fun2CodeName)) {
		fun := l.asmModule.fun2CodeName[codeName]
		bbs := l.asmModule.fun2Code[fun]
		newBBs := l.lowerFunction(fun, bbs, funIndex)
		funIndex++
		newFun2Code[fun] = newBBs
	}
	l.asmModule.fun2Code = newFun2Code
}
func (l *Lower) lowerFunction(functionSym *parser.FunctionSymbol, bbs []*BasicBlock, funIndex int) []*BasicBlock {
	l.initStates(functionSym)
	l.lowerVars() // 分配寄存器

	for i := range len(bbs) {
		bb := bbs[i]
		newInsts := []IInst{}
		l.lowerInsts(bb.insts, &newInsts)
		bb.insts = newInsts
	}
	// 添加序曲
	bbs[0].insts = l.addPrologue(bbs[0].insts)
	// 添加尾声
	l.addEpilogue(&bbs[len(bbs)-1].insts)
	// 基本块的标签和跳转指令
	newBBs := l.lowerBBLabelAndJumps(bbs, funIndex)
	return newBBs
}

// 初始化当前函数的一些状态变量
func (l *Lower) initStates(functionSym *parser.FunctionSymbol) {
	l.usedCalleeProtectedRegs = []*Register{}
	l.usedCallerProtectedRegs = []*Register{}
	l.numTotalVars = l.asmModule.numTotalVars[functionSym]
	l.numParams = functionSym.GetNumParams()
	l.numLocalVars = len(functionSym.Vars) - l.numParams
	l.numTempVars = l.numTotalVars - len(functionSym.Vars)
	l.numArgsOnStack = 0
	l.rspOffset = 0
	l.lowedVars = make(map[int]IOprand)
	l.allocatedRegisters = make(map[*Register]int)

	l.canUseRedZone = false
	isLeafFunction := l.asmModule.isLeafFunction[functionSym]
	if isLeafFunction {
		paramsToSave := min(6, l.numParams)
		ref := reflect.ValueOf(l.saveCalleeProtectedRegs)
		bytes := paramsToSave*4 + l.numLocalVars*4 + ref.Type().NumIn()*8
		l.canUseRedZone = bytes < 128
	}
}

// 把变量下标转换成内存地址或寄存器
func (l *Lower) lowerVars() {
	paramsToSave := min(6, l.numParams)
	// 处理参数
	for varIndex := 0; varIndex < l.numTotalVars; varIndex++ {
		var newOprand IOprand
		if varIndex < l.numParams {
			if varIndex < 6 { // 前6个参数，从自己的栈桢里访问，在序曲里这些参数拷贝到栈里的
				offset := -(varIndex + 1) * 4
				newOprand = NewMemAddress(rbp, offset)
			} else { // 超过6个的参数，从Caller的栈里访问参数
				// +16是因为有一个callq压入的返回地址，一个pushq rbp又加了8个字节
				offset := (varIndex-6)*8 + 16
				newOprand = NewMemAddress(rbp, offset)
			}
		} else if varIndex < l.numParams+l.numLocalVars { // 本地变量，在栈桢里
			offset := -(varIndex - l.numParams + paramsToSave + 1) * 4
			newOprand = NewMemAddress(rbp, offset)
		} else { // 临时变量，分配寄存器
			newOprand = l.allocatedRegister(varIndex)
		}
		// 缓存起来
		l.lowedVars[varIndex] = newOprand
	}
}

// 获取寄存器
func (l *Lower) allocatedRegister(varIndex int) *Register {
	for _, reg := range registers32 {
		if _, ok := l.allocatedRegisters[reg]; !ok {
			l.allocatedRegisters[reg] = varIndex
			if slices.Index(calleeProtected32, reg) != -1 {
				l.usedCalleeProtectedRegs = append(l.usedCalleeProtectedRegs, reg)
			} else if slices.Index(callerProtected32, reg) != -1 {
				l.usedCallerProtectedRegs = append(l.usedCallerProtectedRegs, reg)
			}
			return reg
		}
	}
	// 不应该执行到这里，执行到这里应该报错
	fmt.Println("Unable to allocate a Register, the generated asm is not reliable")
	return registers32[0]
}

// 预留eax寄存器，不参与分配
func (l *Lower) reserveReturnSlot() {
	l.allocatedRegisters[eax] = -1
}

// 归还已经分配的寄存器
func (l *Lower) freeRegister(reg *Register) {
	delete(l.allocatedRegisters, reg)
	var index int
	if index = slices.Index(l.usedCalleeProtectedRegs, reg); index != -1 {
		l.usedCalleeProtectedRegs = append(l.usedCalleeProtectedRegs[:index], l.usedCalleeProtectedRegs[index+1:]...)
	}
	if index = slices.Index(l.usedCallerProtectedRegs, reg); index != -1 {
		l.usedCallerProtectedRegs = append(l.usedCallerProtectedRegs[:index], l.usedCallerProtectedRegs[index+1:]...)
	}
}

// 添加序曲
func (l *Lower) addPrologue(insts []IInst) []IInst {
	var newInsts []IInst
	// 保存rbp的值
	newInsts = append(newInsts, NewInst_1(pushq, rbp))
	// 把原来的栈顶保存到rbp,成为现在的栈底
	newInsts = append(newInsts, NewInst_2(movq, rsp, rbp))
	// 前6个参数存入栈帧
	paramsToSave := min(l.numParams, 6)
	for i := range paramsToSave {
		offset := -(i + 1) * 4
		newInsts = append(newInsts, NewInst_2(movl, paramRegisters32[i], NewMemAddress(rbp, offset)))
	}
	// 计算栈顶指针需要移动多少位置
	if !l.canUseRedZone {
		l.rspOffset = paramsToSave*4 + l.numLocalVars*4 + len(l.usedCallerProtectedRegs)*4 + l.numArgsOnStack*8 + 16
		rem := (l.rspOffset + len(l.usedCalleeProtectedRegs)*8) % 16

		switch rem {
		case 8:
			l.rspOffset += 8
		case 4:
			l.rspOffset += 12
		case 12:
			l.rspOffset += 4
		}
		if l.rspOffset > 0 {
			newInsts = append(newInsts, NewInst_2(subq, NewOprand(immediate, l.rspOffset), rsp))
		}
	}

	l.saveCalleeProtectedRegs(&newInsts)

	newInsts = append(newInsts, insts...)

	return newInsts
}

// 添加尾声
func (l *Lower) addEpilogue(newInsts *[]IInst) {
	// 恢复Callee负责保护的寄存器
	l.restoreCalleeProtectedRegs(newInsts)
	// 缩小栈帧
	if !l.canUseRedZone && l.rspOffset > 0 {
		*newInsts = append(*newInsts, NewInst_2(addq, NewOprand(immediate, l.rspOffset), rsp))
	}

	*newInsts = append(*newInsts, NewInst_1(popq, rbp))

	*newInsts = append(*newInsts, NewInst_0(retq))
}

// 去除空的BasicBlock
func (l *Lower) lowerBBLabelAndJumps(bbs []*BasicBlock, funIndex int) []*BasicBlock {
	newBBs := []*BasicBlock{}
	bbIndex := 0

	for i := range len(bbs) {
		bb := bbs[i]
		if len(bb.insts) > 0 {
			bb.funIndex = funIndex
			bb.bbIndex = bbIndex
			bbIndex++
			newBBs = append(newBBs, bb)
		} else {
			for j := range len(newBBs) {
				lastInst := newBBs[j].insts[len(newBBs[j].insts)-1]
				if lastInst.(*AbsInst).op < 20 {
					jumpInst := lastInst.(*Inst_1)
					destBB := jumpInst.oprand.(*Oprand).value.(*BasicBlock)
					if destBB == bb {
						jumpInst.oprand.(*Oprand).value = bbs[i+1]
					}
				}
			}
		}
	}

	for i := range len(newBBs) {
		insts := newBBs[i].insts
		lastInst := insts[len(insts)-1]
		if lastInst.getOp() < 20 {
			jumpInst := lastInst.(*Inst_1)
			bbDest := jumpInst.oprand.(*Oprand).value.(*BasicBlock)
			jumpInst.oprand.(*Oprand).value = bbDest.getName()
			bbDest.isDestination = true
		}
	}

	return newBBs
}

// Lower指令
func (l *Lower) lowerInsts(insts []IInst, newInsts *[]IInst) {
	for i := range len(insts) {
		inst := insts[i]
		if isInst_2(inst) {
			inst_2 := inst.(*Inst_2)
			inst_2.oprand1 = l.lowerOprand(inst_2.oprand1)
			inst_2.oprand2 = l.lowerOprand(inst_2.oprand2)

			if !(inst_2.op == movl && inst_2.oprand1 == inst_2.oprand2) {
				*newInsts = append(*newInsts, inst_2)
			}
		} else if isInst_1(inst) { // 一个操作数
			inst_1 := inst.(*Inst_1)
			inst_1.oprand = l.lowerOprand(inst_1.oprand)

			if inst_1.op == callq {
				l.lowerFunctionCall(inst_1, newInsts)
			} else {
				*newInsts = append(*newInsts, inst_1)
			}
		} else {
			*newInsts = append(*newInsts, inst)
		}
	}
}
func (l *Lower) lowerFunctionCall(inst_1 *Inst_1, newInsts *[]IInst) {
	functionOprand := inst_1.oprand.(*FunctionOprand)
	args := functionOprand.args

	numArgs := len(args)
	if numArgs > 6 && numArgs-6 > l.numArgsOnStack {
		l.numArgsOnStack = numArgs - 6
	}

	paramsToSave := min(l.numParams, 6)
	offset := -(paramsToSave + l.numLocalVars + 1) * 4
	spilledTempVars := []int{}
	spilledRegs := []*Register{}
	for i := 0; i < len(l.usedCallerProtectedRegs); i++ {
		reg := l.usedCallerProtectedRegs[i]
		*newInsts = append(*newInsts, NewInst_2(movl, reg, NewMemAddress(rbp, offset-i*4)))
		varIndex := l.allocatedRegisters[reg]
		spilledRegs = append(spilledRegs, reg)
		spilledTempVars = append(spilledTempVars, varIndex)
	}
	for _, reg := range spilledRegs {
		l.freeRegister(reg)
	}

	for j := 0; j < numArgs && j < 6; j++ {
		regSrc := l.lowerOprand(args[j])
		regDest := paramRegisters32[j]
		if regDest != regSrc {
			*newInsts = append(*newInsts, NewInst_2(movl, regSrc, regDest))
		}
	}

	if len(args) > 6 {
		for j := 6; j < numArgs; j++ {
			offset := (j - 6) * 8
			*newInsts = append(*newInsts, NewInst_2(movl, functionOprand.args[j], NewMemAddress(rsp, offset)))
		}
	}

	*newInsts = append(*newInsts, inst_1)

	l.reserveReturnSlot()

	for i := 0; i < len(spilledTempVars); i++ {
		varIndex := spilledTempVars[i]
		reg := l.allocatedRegister(varIndex)
		*newInsts = append(*newInsts, NewInst_2(movl, NewMemAddress(rbp, offset-i*4), reg))
		l.lowedVars[varIndex] = reg
	}
}
func (l *Lower) lowerOprand(oprand IOprand) IOprand {
	newOprand := oprand

	if v, ok := oprand.(*Oprand); ok {
		if v.kind == varIndex {
			varIndex := v.value.(int)
			return l.lowedVars[varIndex]
		} else if oprand.(*Oprand).kind == returnSlot {
			newOprand = eax
		}
	} else if v, ok := oprand.(*FunctionOprand); ok {
		if v.kind == varIndex {
			varIndex := v.value.(int)
			return l.lowedVars[varIndex]
		} else if oprand.(*FunctionOprand).kind == returnSlot {
			newOprand = eax
		}
	}

	return newOprand
}

func (l *Lower) saveCalleeProtectedRegs(newInsts *[]IInst) {
	for i := 0; i < len(l.usedCalleeProtectedRegs); i++ {
		regIndex := slices.Index(calleeProtected32, l.usedCalleeProtectedRegs[i])
		reg64 := calleeProtected64[regIndex]
		*newInsts = append(*newInsts, NewInst_1(pushq, reg64))
	}
}

func (l *Lower) restoreCalleeProtectedRegs(newInsts *[]IInst) {
	for i := len(l.usedCalleeProtectedRegs) - 1; i >= 0; i-- {
		regIndex := slices.Index(calleeProtected32, l.usedCalleeProtectedRegs[i])
		reg64 := calleeProtected64[regIndex]
		*newInsts = append(*newInsts, NewInst_1(popq, reg64))
	}
}

func CompileToAsm(prog *parser.Prog, verbose bool) string {
	asmGenerator := NewAsmGenerator()

	asmModule := asmGenerator.Visit(prog, nil).(*AsmModule)
	if verbose {
		fmt.Println("在Lower之前：")
		fmt.Println(asmModule.toString())
	}

	lower := NewLower(asmModule)
	lower.lowerModule()

	asm := asmModule.toString()
	if verbose {
		fmt.Println("在Lower之后：")
		fmt.Println(asm)
	}
	return asm
}
