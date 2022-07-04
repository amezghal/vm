package vm

import (
	"fmt"
	"math"
	"strconv"
	"strings"
)

/*
same as javascript
variants
	"5"  ADD           "4"            = "54"
	"5"  ADD           int(4)         = "54"
	"5"  ADD           float(12.34)   = "512.34"
	"5" MUL|QUO|SUB   ?              = NAN
	"aa" MUL|QUO|SUB   ?              = NAN
*/

type VMValue struct {
	kind  string
	value any
}

func Arith(op int, n1, n2 VMValue) VMValue {
	if n1.kind == T_UNDEFINED || n2.kind == T_UNDEFINED {
		return VMValue{
			kind:  T_UNDEFINED,
			value: "NAN",
		}
	}

	isInt := func(src any) bool {
		switch src.(type) {
		case int8, int16, int32, int64, int, uint8, uint16, uint32, uint64, uint:
			return true
		}
		return false
	}

	isFloat := func(src any) bool {
		switch src.(type) {
		case float32, float64:
			return true
		}
		return false
	}

	asInt := func(src any) int {
		switch src.(type) {
		case int8:
			return int(src.(int8))
		case int16:
			return int(src.(int16))
		case int32:
			return int(src.(int32))
		case int64:
			return int(src.(int64))
		case int:
			return src.(int)
		case uint8:
			return int(src.(uint8))
		case uint16:
			return int(src.(uint16))
		case uint32:
			return int(src.(uint32))
		case uint64:
			return int(src.(uint64))
		case uint:
			return int(src.(uint))
		}
		return int(0)
	}

	asFloat := func(src any) float64 {
		switch src.(type) {
		case int8:
			return float64(src.(int8))
		case int16:
			return float64(src.(int16))
		case int32:
			return float64(src.(int32))
		case int64:
			return float64(src.(int64))
		case int:
			return float64(src.(int))
		case uint8:
			return float64(src.(uint8))
		case uint16:
			return float64(src.(uint16))
		case uint32:
			return float64(src.(uint32))
		case uint64:
			return float64(src.(uint64))
		case uint:
			return float64(src.(uint))
		case float32:
			return float64(src.(float32))
		case float64:
			return src.(float64)
		}
		return float64(0)
	}

	switch {
	case n1.kind == T_STRING:
		if op == opADD {
			result := strings.Builder{}
			result.WriteString(n1.value.(string))
			switch {
			case n2.kind == T_STRING:
				result.WriteString(n2.value.(string))
			case isInt(n2.value):
				result.Write(strconv.AppendInt([]byte{}, int64(asInt(n2.value)), 10))
			case isFloat(n2.value):
				result.WriteString(fmt.Sprintf("%.2f", asFloat(n2.value)))
			}
			return VMValue{
				kind:  T_STRING,
				value: result.String(),
			}
		} else {
			// other ops: operand1 should be convertible to NUMBER and operand2 should be of type number
			var result any
			if v, err := strconv.ParseFloat(n1.value.(string), 64); err == nil {
				if n2.kind == T_NUMBER {
					result = doArith(op, asFloat(v), asFloat(n2.value))
				} else if v2, err := strconv.ParseFloat(n2.value.(string), 64); err == nil {
					result = doArith(op, asFloat(v), asFloat(v2))
				}
			}

			if result != nil {
				return VMValue{
					kind:  T_NUMBER,
					value: result,
				}
			}
		}
	case isFloat(n1.value):
		var rr any
		switch {
		case isInt(n2.value), isFloat(n2.value):
			rr = doArith(op, asFloat(n1.value), asFloat(n2.value))
		case n2.kind == T_STRING:
			if v, err := strconv.ParseFloat(n2.value.(string), 64); err == nil {
				rr = doArith(op, asFloat(n1.value), asFloat(v))
			} else if op == opADD {
				result := fmt.Sprintf(`%.2f%s`, asFloat(n1.value), n2.value.(string))
				return VMValue{
					kind:  T_STRING,
					value: result,
				}
			}
		}

		if rr != nil {
			return VMValue{
				kind:  T_NUMBER,
				value: rr,
			}
		}
	case isInt(n1.value):
		var rr any
		switch {
		case isInt(n2.value):
			rr = doArith(op, asInt(n1.value), asInt(n2.value))
		case isFloat(n2.value):
			rr = doArith(op, asFloat(n1.value), asFloat(n2.value))
		case n2.kind == T_STRING:
			if v, err := strconv.ParseInt(n2.value.(string), 10, 64); err == nil {
				rr = doArith(op, asInt(n1.value), asInt(v))
			} else if v, err := strconv.ParseFloat(n2.value.(string), 64); err == nil {
				rr = doArith(op, asFloat(n1.value), asFloat(v))
			} else {
				if op == opADD {
					result := strings.Builder{}
					result.Write(strconv.AppendInt([]byte{}, int64(asInt(n1.value)), 10))
					result.WriteString(n2.value.(string))
					return VMValue{
						kind:  T_STRING,
						value: result.String(),
					}
				}
			}
		}

		if rr != nil {
			return VMValue{
				kind:  T_NUMBER,
				value: rr,
			}
		}
	}

	return VMValue{
		kind:  T_UNDEFINED,
		value: "NAN",
	}
}

func doArith(op int, operand1, operand2 any) any {
	// accept only int64, float64, string|+operation
	var (
		op1IsInt, op1IsFloat, op1IsString bool
		op2IsInt, op2IsFloat, op2IsString bool
	)

	switch operand1.(type) {
	case int:
		op1IsInt = true
	case float64:
		op1IsFloat = true
	case string:
		op1IsString = true
	default:
		panic(fmt.Sprintf("unvalid provided type %T", operand1))
	}

	switch operand2.(type) {
	case int:
		op2IsInt = true
	case float64:
		op2IsFloat = true
	case string:
		op2IsString = true
	default:
		panic(fmt.Sprintf("unvalid provided type %T", operand2))
	}

	if (op1IsString && !op2IsString) || (!op1IsString && op2IsString) || ((op1IsString || op2IsString) && op != opADD) {
		panic("both params should be string if one is string and only + is available")
	}

	// only accept + for string
	if (op1IsString && op2IsString) && op == opADD {
		return operand1.(string) + operand2.(string)
	}

	calcInts := func(operator int, op1, op2 int) int {
		switch operator {
		case opADD:
			return op1 + op2
		case opSUB:
			return op1 - op2
		case opMUL:
			return op1 * op2
		case opQUO:
			return op1 / op2
		case opLT:
			if op1 < op2 {
				return 1
			}
			return 0
		case opLTE:
			if op1 <= op2 {
				return 1
			}
			return 0
		case opGT:
			if op1 > op2 {
				return 1
			}
			return 0
		case opGTE:
			if op1 >= op2 {
				return 1
			}
			return 0
		case opEQ:
			if op1 == op2 {
				return 1
			}
			return 0
		case opOR:
			if op1 != 0 || op2 != 0 {
				return 1
			}
			return 0
		case opAND:
			if op1 != 0 && op2 != 0 {
				return 1
			}
			return 0
		case opNOT:
			if op1 != 0 {
				return 0
			}
			return 1
		default:
			return 0
		}
	}

	calcFloats := func(operator int, op1, op2 float64) float64 {
		switch operator {
		case opADD:
			return op1 + op2
		case opSUB:
			return op1 - op2
		case opMUL:
			return op1 * op2
		case opQUO:
			return op1 / op2
		case opMOD:
			return math.Mod(op1, op2)
		case opLT:
			if op1 < op2 {
				return 1
			}
			return 0
		case opLTE:
			if op1 <= op2 {
				return 1
			}
			return 0
		case opGT:
			if op1 > op2 {
				return 1
			}
			return 0
		case opGTE:
			if op1 >= op2 {
				return 1
			}
			return 0
		case opEQ:
			if op1 == op2 {
				return 1
			}
			return 0
		case opOR:
			if op1 != 0 || op2 != 0 {
				return 1
			}
			return 0
		case opAND:
			if op1 != 0 && op2 != 0 {
				return 1
			}
			return 0
		case opNOT:
			if op1 != 0 {
				return 0
			}
			return 1
		default:
			return .0
		}
	}

	if (op1IsInt && !op2IsInt) || (op == opMOD && op1IsInt) {
		operand1 = float64(operand1.(int))
		op1IsFloat = true
	}

	if (!op1IsInt && op2IsInt) || (op == opMOD && op2IsInt) {
		operand2 = float64(operand2.(int))
		op2IsFloat = true
	}

	if op == opMOD || op1IsFloat || op2IsFloat {
		return calcFloats(op, operand1.(float64), operand2.(float64))
	}

	// can be only int
	resultInt := calcInts(op, operand1.(int), operand2.(int))
	if op == opQUO {
		resultFloat := calcFloats(op, float64(operand1.(int64)), float64(operand2.(int64)))
		if float64(resultInt) < resultFloat {
			return resultFloat
		}
	}

	return resultInt
}
