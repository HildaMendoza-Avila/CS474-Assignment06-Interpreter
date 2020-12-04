type Operator = PLUS | MINUS | TIMES | DIV
type ComparisonType = EQ

type Value =
  | IntValue of int32
  | BooleanValue of bool

type NameType = Name of string
type BindingType = Binding of NameType * Value
type Environment = 
  | DynamicScopedEnvironment of (BindingType list)
  | LexicalScopedEnvironment of (BindingType list)

let bind (varName, varValue, bindingsList) = 
  let newBinding = Binding(varName, varValue)
  newBinding :: bindingsList

let lookup (variableNameToFind, bindingsToExamine) = 
  let varFoundInBindings (Binding(varName, varValue)) = varName = variableNameToFind
  match List.tryFind varFoundInBindings bindingsToExamine with
  | Some (Binding(varName, varValue)) -> varValue
  | None -> BooleanValue (false)  // returns false if variable was not bound in the past

type Expression =
  | IntConstant of int32
  | BooleanConstant of bool
  | BinOp of Operator * Expression * Expression
  | IfExpression of Expression * Expression * Expression
  | ComparisonExpression of ComparisonType * Expression * Expression
  | LetExpression of NameType * Expression * Expression
  | VariableExpression of NameType

let mutable currDynamicScopeEnv = DynamicScopedEnvironment([]) 

let rec eval (c, e) =
  match c with
  | IntConstant(value) -> (IntValue value)
  | BooleanConstant(value) -> (BooleanValue value)
  | BinOp(op,left,right) ->
    let (IntValue l) = eval (left, e)
    let (IntValue r) = eval (right, e)
    match op with
    | MINUS -> IntValue (l - r)
    | PLUS -> IntValue (l + r)
    | TIMES -> IntValue (l * r)
    | DIV -> IntValue (l / r)
  | ComparisonExpression(compType, left, right) ->
    let (IntValue l) = eval (left, e)
    let (IntValue r) = eval (right, e)
    match compType with
    | EQ -> BooleanValue (l = r)
  | IfExpression(compExpression, thenExpression, elseExpression) ->
    let (BooleanValue condResult) = eval (compExpression, e)
    if condResult then eval (thenExpression, e)
    else eval (elseExpression, e)
  | LetExpression(name, valueExpression, body) ->
    // Bind the name to the value of evaluating the initialization expression
    let value = eval (valueExpression, e)
    match e with
    | DynamicScopedEnvironment(outerScopeBindingsList) ->
      match currDynamicScopeEnv with
      | DynamicScopedEnvironment(bindingsList) ->
        let newBindingsList = bind (name, value, bindingsList)
        currDynamicScopeEnv <- DynamicScopedEnvironment(newBindingsList)
        eval (body, currDynamicScopeEnv)
      | _ -> BooleanValue(false)    // there was an error 
    | LexicalScopedEnvironment(bindingsList) -> 
      let newBindingsList = bind (name, value, bindingsList)
      eval (body, LexicalScopedEnvironment(newBindingsList))

  | VariableExpression(varName) ->
    // Check what is the value bound to the name in the variable expression
    match e with
    | DynamicScopedEnvironment(outerScopeBindingsList) -> 
      match currDynamicScopeEnv with
      | DynamicScopedEnvironment(bindingsList) ->lookup (varName, bindingsList)
    | LexicalScopedEnvironment(bindingsList) -> lookup (varName, bindingsList)
    

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
//    if (operand == 0) 0 else 474 / operand;
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
    BinOp(DIV, IntConstant(474), VariableExpression(Name("operand"))
    )
  )
)

// {
//   let x = 10
//   { let x = 20
//     x -> Always 20
//   }
//   + x -> 10 on lexical scoping, 20 on dynamic scoping
// }
// Lexical scoping = 30
// Dynamic scoping = 40

let p7 = LetExpression(
  Name("x"), 
  IntConstant(10), 
  BinOp(
    PLUS, 
    LetExpression(
      Name("x"), 
      IntConstant(20), 
      VariableExpression(Name("x"))
    ), 
    VariableExpression(Name("x"))
  )
)


let e = DynamicScopedEnvironment([])

printfn "\ncurr result: %A" (eval(p7, e))
































