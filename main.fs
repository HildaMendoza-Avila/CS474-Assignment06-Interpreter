type Operator = PLUS | MINUS | TIMES | DIV
type ComparisonType = EQ

type NameType = Name of string

type Expression =
  | IntConstant of int32
  | BooleanConstant of bool
  | BinOp of Operator * Expression * Expression
  | IfExpression of Expression * Expression * Expression
  | ComparisonExpression of ComparisonType * Expression * Expression
  | LetExpression of NameType * Expression * Expression
  | VariableExpression of NameType

type Value =
  | IntValue of int32
  | BooleanValue of bool

let rec eval c =
  match c with
  | IntConstant(value) -> (IntValue value)
  | BooleanConstant(value) -> (BooleanValue value)
  | BinOp(op,left,right) ->
    let (IntValue l) = eval left
    let (IntValue r) = eval right
    match op with
    | MINUS -> IntValue (l - r)
    | PLUS -> IntValue (l + r)
    | TIMES -> IntValue (l * r)
    | DIV -> IntValue (l / r)
  | ComparisonExpression(compType, left, right) ->
    let (IntValue l) = eval left
    let (IntValue r) = eval right
    match compType with
    | EQ -> BooleanValue (l = r)
  | IfExpression(compExpression, thenExpression, elseExpression) ->
    let (BooleanValue condResult) = eval compExpression
    if condResult then eval thenExpression
    else eval elseExpression
  | LetExpression(name, value, body) ->
    // Bind the name to the value of evaluating the initialization expression
  | VariableExpression(varName) ->
    // Check what is the value bound to the name in the variable expression

// 474
let p1 = IntConstant(474)     

// 400 + 74
let p2 = BinOp(PLUS, IntConstant(400), IntConstant(74)) 

// 400 + (70 + 4)
let p3 = BinOp(PLUS, IntConstant(400), BinOp(PLUS, IntConstant(70), IntConstant(4)))

// 400 + true
let p4 = BinOp(PLUS, IntConstant(400), BooleanConstant(true)) 

// if (474 - (400 + 74) == 0) 0 else (1 / (474 - (400 + 74)))
let p5 = IfExpression(
  ComparisonExpression(
    EQ, 
    BinOp(MINUS, IntConstant(474), BinOp(PLUS, IntConstant(400), IntConstant(74))), 
    IntConstant(0)
  ), 
  IntConstant(0), 
  BinOp(DIV, IntConstant(1), BinOp(MINUS, IntConstant(474), BinOp(PLUS, IntConstant(400), IntConstant(74))))
)

// { let operand = (474 - (400 + 74));
//    if (operand == 0) 0 else 1 / operand;
// }
let p6 = LetExpression(
  Name("operand"), 
  BinOp(MINUS, IntConstant(474), BinOp(PLUS, IntConstant(400), IntConstant(74))), 
  IfExpression(
    ComparisonExpression(
      EQ,
      VariableExpression(Name("operand")), 
      IntConstant(0)
    ), 
    IntConstant(0), 
    BinOp(DIV, IntConstant(1), VariableExpression(Name("operand"))
    )
  )
)



printfn "\ncurr result: %A" (eval(p5))
































