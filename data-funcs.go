package eval

import (
	"go/constant"
	"go/token"
	"reflect"

	"github.com/bobyang007/helper/goh/constanth"
	"github.com/bobyang007/helper/reflecth"
)

func getBool(inVal constant.Value) bool {
	if inVal == nil {
		return false
	}
	refValue := reflect.ValueOf(inVal)
	if refValue.Kind() == reflect.Ptr {
		refValue = refValue.Elem()
	}
	refType := reflect.TypeOf(inVal)
	if refType.Kind() == reflect.Ptr {
		refType = refType.Elem()
	}
	switch refType.Kind() {
	case reflect.Bool:
		return refValue.Bool()
	case reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uint:
		return refValue.Uint() != 0
	case reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64, reflect.Int:
		return refValue.Int() != 0
	case reflect.Float32, reflect.Float64:
		return refValue.Float() != 0
	case reflect.Complex64, reflect.Complex128:
		return refValue.Complex() != 0
	case reflect.String:
		valueString := refValue.String()
		if valueString == "" ||
			valueString == "0" ||
			valueString == "false" ||
			valueString == "no" ||
			valueString == "{}" ||
			valueString == "[]" {
			return false
		}
		return true
	}

	return false
}

func binaryOp(x Data, op token.Token, y Data) (r Data, err *intError) {
	switch xK, yK := x.Kind(), y.Kind(); {
	case xK == UntypedBool && yK == UntypedConst, xK == UntypedConst && yK == UntypedBool:
		switch {
		case xK == UntypedBool:
			if op == token.LAND {
				r = untypedBoolData(x.UntypedBool() && getBool(y.UntypedConst()))
			} else {
				r = untypedBoolData(x.UntypedBool() || getBool(y.UntypedConst()))
			}
		default:
			if op == token.LAND {
				r = untypedBoolData(y.UntypedBool() && getBool(x.UntypedConst()))
			} else {
				r = untypedBoolData(y.UntypedBool() || getBool(x.UntypedConst()))
			}
		}
	case xK == Nil || yK == Nil || ((xK == UntypedBool || yK == UntypedBool) && xK != yK): // This case needed first to prevent other cases to perform.
		fallthrough
	default:
		err = invBinOpTypesInvalError(x, op, y)
	case xK == UntypedBool && yK == UntypedBool:
		if op == token.LAND {
			r = untypedBoolData(x.UntypedBool() && y.UntypedBool())
		} else {
			r = untypedBoolData(x.UntypedBool() || y.UntypedBool())
		}
	case xK == Regular && yK == Regular, xK == Regular, yK == Regular: // At least one args is regular value
		// Prepare args
		var xV, yV reflect.Value
		switch {
		case xK == Regular && yK == Regular:
			xV = x.Regular()
			yV = y.Regular()
		case xK == Regular:
			xV = x.Regular()
			var ok bool
			yV, ok = y.Assign(xV.Type())
			if !ok {
				return nil, invBinOpTypesMismError(x, op, y)
			}
		case yK == Regular:
			yV = y.Regular()
			var ok bool
			xV, ok = x.Assign(yV.Type())
			if !ok {
				return nil, invBinOpTypesMismError(x, op, y)
			}
		}

		// Calc
		r, err = binaryOpRegular(xV, op, yV)
	case xK == TypedConst && yK == TypedConst, xK == TypedConst && yK == UntypedConst, xK == UntypedConst && yK == TypedConst: // At least one args is typed constant
		// Prepare args
		var xTC, yTC constanth.TypedValue
		switch {
		case xK == TypedConst && yK == TypedConst:
			xTC = x.TypedConst()
			yTC = y.TypedConst()
		case xK == TypedConst:
			xTC = x.TypedConst()
			var ok bool
			yTC, ok = constanth.MakeTypedValue(y.UntypedConst(), xTC.Type())
			if !ok {
				return nil, invBinOpTypesMismError(x, op, y)
			}
		case yK == TypedConst:
			yTC = y.TypedConst()
			var ok bool
			xTC, ok = constanth.MakeTypedValue(x.UntypedConst(), yTC.Type())
			if !ok {
				return nil, invBinOpTypesMismError(x, op, y)
			}
		}

		// Calc
		r, err = binaryOpTypedConst(xTC, op, yTC)
	case xK == UntypedConst && yK == UntypedConst:
		r, err = binaryOpUntypedConst(x.UntypedConst(), op, y.UntypedConst())
	}
	return
}

func compareOpWithNil(x Data, op token.Token) (r Data, err *intError) {
	var equality bool // true if op == "=="
	switch op {
	case token.EQL:
		equality = true
	case token.NEQ:
		equality = false
	default:
		return nil, invBinOpError(x.DeepString(), op.String(), "nil", "operator "+op.String()+" not defined on nil")
	}

	if x.Kind() == Regular {
		switch xV := x.Regular(); xV.Kind() {
		case reflect.Slice, reflect.Map, reflect.Func, reflect.Ptr, reflect.Chan, reflect.Interface:
			return untypedBoolData(xV.IsNil() == equality), nil // TODO IsNill not the same as ==nil
		}
	}
	return nil, cmpWithNilError(x, op)
}

func compareOpWithUntypedBool(x Data, op token.Token, y bool) (r Data, err *intError) {
	var equality bool // true if op == "=="
	switch op {
	case token.EQL:
		equality = true
	case token.NEQ:
		equality = false
	default:
		return nil, invBinOpUnknOpError(x, op, MakeUntypedBool(y))
	}

	var xB bool
	switch x.Kind() {
	case Regular:
		xV := x.Regular()
		if xV.Kind() != reflect.Bool {
			return nil, invBinOpTypesMismError(x, op, MakeUntypedBool(y))
		}
		xB = xV.Bool()
	case TypedConst:
		var ok bool
		xB, ok = constanth.BoolVal(x.TypedConst().Untyped())
		if !ok {
			return nil, invBinOpTypesMismError(x, op, MakeUntypedBool(y))
		}
	case UntypedConst:
		var ok bool
		xB, ok = constanth.BoolVal(x.UntypedConst())
		if !ok {
			return nil, invBinOpTypesMismError(x, op, MakeUntypedBool(y))
		}
	case UntypedBool:
		xB = x.UntypedBool()
	default:
		return nil, invBinOpTypesInvalError(x, op, MakeUntypedBool(y))
	}

	return untypedBoolData((xB == y) == equality), nil
}

func compareOp(x Data, op token.Token, y Data) (r Data, err *intError) {
	var rB bool
	switch xK, yK := x.Kind(), y.Kind(); {
	case xK == Nil:
		return compareOpWithNil(y, op)
	case yK == Nil:
		return compareOpWithNil(x, op)
	case xK == UntypedBool:
		return compareOpWithUntypedBool(y, op, x.UntypedBool())
	case yK == UntypedBool:
		return compareOpWithUntypedBool(x, op, y.UntypedBool())
	case xK == Regular && yK == Regular, xK == Regular, yK == Regular: // At least one args is regular value
		// Prepare args
		var xV, yV reflect.Value
		switch {
		case xK == Regular && yK == Regular:
			xV = x.Regular()
			yV = y.Regular()
		case xK == Regular:
			xV = x.Regular()
			var ok bool
			yV, ok = y.Assign(xV.Type())
			if !ok {
				return nil, invBinOpTypesMismError(x, op, y)
			}
		case yK == Regular:
			yV = y.Regular()
			var ok bool
			xV, ok = x.Assign(yV.Type())
			if !ok {
				return nil, invBinOpTypesMismError(x, op, y)
			}
		}

		// Calc
		rB, err = compareOpRegular(xV, op, yV)
	case xK == TypedConst && yK == TypedConst, xK == TypedConst && yK == UntypedConst, xK == UntypedConst && yK == TypedConst: // At least one args is typed constant
		// Prepare args
		var xTC, yTC constanth.TypedValue
		switch {
		case xK == TypedConst && yK == TypedConst:
			xTC = x.TypedConst()
			yTC = y.TypedConst()
		case xK == TypedConst:
			xTC = x.TypedConst()
			var ok bool
			yTC, ok = constanth.MakeTypedValue(y.UntypedConst(), xTC.Type())
			if !ok {
				return nil, invBinOpTypesMismError(x, op, y)
			}
		case yK == TypedConst:
			yTC = y.TypedConst()
			var ok bool
			xTC, ok = constanth.MakeTypedValue(x.UntypedConst(), yTC.Type())
			if !ok {
				return nil, invBinOpTypesMismError(x, op, y)
			}
		}

		// Calc
		rB, err = compareOpTypedConst(xTC, op, yTC)
	case xK == UntypedConst && yK == UntypedConst:
		rB, err = compareOpUntypedConst(x.UntypedConst(), op, y.UntypedConst())
	default:
		err = invBinOpTypesInvalError(x, op, y) // unreachable?
	}
	if err == nil {
		r = untypedBoolData(rB)
	}
	return
}

func shiftOp(x Data, op token.Token, y Data) (r Data, err *intError) {
	// Calc right operand
	var yUint uint
	switch y.Kind() {
	case Regular:
		yV := y.Regular()
		if !reflecth.IsUint(yV.Kind()) {
			return nil, invBinOpShiftCountError(x, op, y)
		}
		yUint = uint(yV.Uint())
	case TypedConst:
		yTC := y.TypedConst()
		if !reflecth.IsUint(yTC.Type().Kind()) {
			return nil, invBinOpShiftCountError(x, op, y)
		}
		var ok bool
		yUint, ok = constanth.UintVal(yTC.Untyped())
		if !ok {
			return nil, invBinOpShiftCountError(x, op, y) // can be reachable only on 32-bit arch
		}
	case UntypedConst:
		var ok bool
		yUint, ok = constanth.UintVal(y.UntypedConst())
		if !ok {
			return nil, invBinOpShiftCountError(x, op, y)
		}
	default:
		return nil, invBinOpShiftCountError(x, op, y)
	}

	switch x.Kind() {
	case Regular:
		r, err = shiftOpRegular(x.Regular(), op, yUint)
	case TypedConst:
		var rTC constanth.TypedValue
		rTC, err = shiftOpTypedConst(x.TypedConst(), op, yUint)
		if err == nil {
			switch y.IsConst() {
			case true:
				r = typedConstData(rTC)
			case false:
				r = regData(rTC.Value())
			}
		}
	case UntypedConst:
		var rC constant.Value
		rC, err = shiftOpUntypedConst(x.UntypedConst(), op, yUint)
		if err == nil {
			switch y.IsConst() {
			case true:
				r = MakeUntypedConst(rC)
			case false:
				rV, ok := constanth.DefaultValue(rC)
				if !ok {
					err = constOverflowType(rC, constanth.DefaultType(rC))
				} else {
					r = MakeRegular(rV)
				}
			}
		}

	default:
		err = invBinOpShiftArgError(x, op, y)
	}
	return
}

func unaryOp(op token.Token, y Data) (r Data, err *intError) {
	switch y.Kind() {
	case Regular:
		r, err = unaryOpRegular(op, y.Regular())
	case TypedConst:
		r, err = unaryOpTypedConst(op, y.TypedConst(), 0) // TODO prec should be set?
	case UntypedConst:
		r, err = unaryOpUntypedConst(op, y.UntypedConst(), 0) // TODO prec should be set?
	case UntypedBool:
		switch op {
		case token.NOT:
			r = untypedBoolData(!y.UntypedBool())
		default:
			err = invUnaryOp(y, op)
		}
	default:
		err = invUnaryOp(y, op)
	}
	return
}
