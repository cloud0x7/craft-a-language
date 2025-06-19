package parser

import (
	"slices"
	"strconv"
	"strings"
)

type IType interface {
	LE(IType) bool
	Accept(ITypeVisitor) any
	hasVoid() bool
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
func IsSimpleType(tp IType) bool {
	_, ok := tp.(*SimpleType)
	return ok
}
func IsUnionType(tp IType) bool {
	_, ok := tp.(*UnionType)
	return ok
}
func IsFunctionType(tp IType) bool {
	_, ok := tp.(*FunctionType)
	return ok
}

type SimpleType struct {
	Type
	Name       string
	upperTypes []IType
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

type FunctionType struct {
	Type
	ParamTypes []IType
	ReturnType IType
}

var FunctionTypeIndex int = 0

func NewFunctionType(returnType IType, paramTypes []IType, name string) *FunctionType {
	f := &FunctionType{
		ParamTypes: paramTypes,
		ReturnType: returnType,
	}
	if len(name) == 0 {
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

type UnionType struct {
	Type
	Name  string
	types []IType
}

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

type SysTypes struct {
}

var Any IType = NewSimpleType("any", []IType{})

var String IType = NewSimpleType("string", []IType{Any})
var Number IType = NewSimpleType("number", []IType{Any})
var Boolean IType = NewSimpleType("boolean", []IType{Any})

var Null IType = NewSimpleType("null", []IType{})
var Undefined IType = NewSimpleType("undefined", []IType{})

var Void IType = NewSimpleType("void", []IType{})

var Integer IType = NewSimpleType("integer", []IType{Number})
var Decimal IType = NewSimpleType("decimal", []IType{Number})

func IsSysType(t IType) bool {
	return t == Any || t == String || t == Number ||
		t == Boolean || t == Null || t == Undefined ||
		t == Void || t == Integer || t == Decimal
}

type ITypeVisitor interface {
	visit(IType) any
	visitSimpleType(*SimpleType) any
	visitFunctionType(*FunctionType) any
	visitUnionType(*UnionType) any
}
