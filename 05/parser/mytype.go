package parser

import (
	"slices"
	"strconv"
	"strings"
)

// ////////////////////////////////////
// 类型体系

// 课程源码本是一个抽象类，有属性和方法，这里使用分离的接口和结构体替代
type IType interface {
	LE(IType) bool           // 当前类型是否小于等于type2
	Accept(ITypeVisitor) any // 访问者模式，可用来生成字节码
	hasVoid() bool           // 类型中是否包含void
	ToString() string
}

type Type struct {
	Name string
}

func NewType(name string) *Type {
	return &Type{Name: name}
}
func (t Type) LE(type2 IType) bool {
	return false
}
func (t Type) Accept(ITypeVisitor) any {
	return nil
}
func (t Type) hasVoid() bool {
	return false
}
func (t Type) ToString() string {
	return t.Name
}

// 计算type1与type2的哪个范围更大
func GetUpperBound(tp1, tp2 IType) IType {
	if tp1 == Any || tp2 == Any {
		return Any
	} else {
		if tp1.LE(tp2) {
			return tp2
		} else if tp2.LE(tp1) {
			return tp1
		} else {
			return NewUnionType([]IType{tp1, tp2}, "")
		}
	}
}

// 判断是否为简单类型
func IsSimpleType(tp IType) bool {
	_, ok := tp.(*SimpleType)
	return ok
}

// 是否为联合类型
func IsUnionType(tp IType) bool {
	_, ok := tp.(*UnionType)
	return ok
}

// 是否为函数类型
func IsFunctionType(tp IType) bool {
	_, ok := tp.(*FunctionType)
	return ok
}

// 简单类型
type SimpleType struct {
	Type
	Name       string
	upperTypes []IType // 多个父类型
}

func NewSimpleType(name string, upperTypes []IType) *SimpleType {
	return &SimpleType{
		Name:       name,
		upperTypes: upperTypes,
	}
}
func (s *SimpleType) hasVoid() bool {
	if s.Name == "void" {
		return true
	}
	// 父类型里有void，也返回true
	for _, t := range s.upperTypes {
		if t.hasVoid() {
			return true
		}
	}
	return false
}
func (s *SimpleType) ToString() string {
	upperTypeNames := strings.Builder{}
	upperTypeNames.WriteString("[")
	for _, ut := range s.upperTypes {
		upperTypeNames.WriteString(ut.(*SimpleType).Name + ", ")
	}
	upperTypeNames.WriteString("]")

	return "SimpleType {name: " + s.Name + ", upperTypes: " + upperTypeNames.String() + "}"
}

// 当前类型是否小于等于type2
func (s *SimpleType) LE(tp2 IType) bool {
	if tp2.(*SimpleType).Name == "any" {
		return true
	} else if s.Name == "any" {
		return false
	} else if s == tp2 {
		return true
	} else if IsSimpleType(tp2) {
		t := tp2.(*SimpleType)
		for _, ut := range s.upperTypes {
			if ut.(*SimpleType).Name == t.Name {
				return true
			}
			if ut.(*SimpleType).LE(tp2) {
				return true
			}
		}
		return false
	} else if IsUnionType(tp2) {
		t := tp2.(*UnionType)
		if slices.IndexFunc(t.types, func(tc IType) bool {
			return s.Name == tc.(*SimpleType).Name
		}) != -1 {
			return true
		} else {
			for _, ut := range t.types {
				if s.LE(ut) {
					return true
				}
			}
			return false
		}
	}
	return false
}

func (s *SimpleType) Accept(visitor ITypeVisitor) any {
	return visitor.visitSimpleType(s)
}

// 函数类型
type FunctionType struct {
	Type
	ParamTypes []IType
	ReturnType IType // 返回值类型
}

// 序号，给函数类型命名
var FunctionTypeIndex int = 0

func NewFunctionType(returnType IType, paramTypes []IType, name string) *FunctionType {
	f := &FunctionType{
		ParamTypes: paramTypes,
		ReturnType: returnType,
	}
	if len(name) == 0 { // 没有传名字，就用function加计数
		f.Name = "@function" + strconv.Itoa(FunctionTypeIndex)
		FunctionTypeIndex++
	} else {
		f.Name = name
	}

	return f
}

func (f *FunctionType) hasVoid() bool {
	return f.ReturnType.hasVoid()
}
func (f *FunctionType) ToString() string {
	paramTypeNames := strings.Builder{}
	paramTypeNames.WriteString("[")
	for _, pt := range f.ParamTypes {
		paramTypeNames.WriteString(pt.(Type).Name + ", ")
	}
	paramTypeNames.WriteString("]")
	return "FunctionType {name: " + f.Name + ", R: " + f.ReturnType.ToString() + ", paramTypes: " + paramTypeNames.String() + ", returnType: " + f.ReturnType.(Type).Name + "}"
}

func (f *FunctionType) LE(tp2 IType) bool {
	if tp2.(*SimpleType).Name == "any" {
		return true
	} else if f.Name == tp2.(*SimpleType).Name {
		return true
	} else if IsUnionType(tp2) {
		t := tp2.(*UnionType)
		if slices.IndexFunc(t.types, func(tc IType) bool {
			return tc.(*SimpleType).Name == f.Name
		}) != -1 {
			return true
		}
	}
	return false
}

func (f *FunctionType) Accept(visitor ITypeVisitor) any {
	return visitor.visitFunctionType(f)
}

// 联合类型
type UnionType struct {
	Type
	Name  string
	types []IType
}

// 序号，用于给UnionType命名
var UnionTypeINdex int = 0

func NewUnionType(types []IType, name string) *UnionType {
	ut := &UnionType{
		types: types,
	}
	if len(name) == 0 {
		ut.Name = "@union" + strconv.Itoa(UnionTypeINdex)
		UnionTypeINdex++
	} else {
		ut.Name = name
	}
	return ut
}
func (ut *UnionType) hasVoid() bool {
	for _, t := range ut.types {
		if t.hasVoid() {
			return true
		}
	}
	return false
}
func (ut *UnionType) ToString() string {
	typeNames := strings.Builder{}
	typeNames.WriteString("[")
	for _, u := range ut.types {
		typeNames.WriteString(u.(Type).Name + ", ")
	}
	typeNames.WriteString("]")
	return "UnionType {name: " + ut.Name + ", types: " + typeNames.String() + "}"
}

func (ut *UnionType) LE(tp2 IType) bool {
	if tp2.(*SimpleType).Name == "any" {
		return true
	} else if IsUnionType(tp2) {
		for _, t1 := range ut.types {
			found := false
			if unionType, ok := tp2.(*UnionType); ok {
				for _, t2 := range unionType.types {
					if t1.LE(t2) {
						found = true
						break
					}
				}
			}
			if !found {
				return false
			}
		}
		return true
	} else {
		return false
	}
}

func (ut *UnionType) Accept(visitor ITypeVisitor) any {
	return visitor.visitUnionType(ut)
}

// 内置类型
type SysTypes struct {
}

// 所有类型的父类型
var Any IType = NewSimpleType("any", []IType{})

// 基础类型
var String IType = NewSimpleType("string", []IType{Any})
var Number IType = NewSimpleType("number", []IType{Any})
var Boolean IType = NewSimpleType("boolean", []IType{Any})

// 所有类型的子类型
var Null IType = NewSimpleType("null", []IType{})
var Undefined IType = NewSimpleType("undefined", []IType{})

// 函数没有任何返回值的情况
var Void IType = NewSimpleType("void", []IType{})

// 两个Number的子类型
var Integer IType = NewSimpleType("integer", []IType{Number})
var Decimal IType = NewSimpleType("decimal", []IType{Number})

// 检查是来吧为内置类型
func IsSysType(t IType) bool {
	return t == Any || t == String || t == Number ||
		t == Boolean || t == Null || t == Undefined ||
		t == Void || t == Integer || t == Decimal
}

// 类型系统的访问者接口
type ITypeVisitor interface {
	visit(IType) any
	visitSimpleType(*SimpleType) any
	visitFunctionType(*FunctionType) any
	visitUnionType(*UnionType) any
}
