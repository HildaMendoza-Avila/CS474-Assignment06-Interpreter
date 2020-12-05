type Operator = PLUS | MINUS | TIMES | DIV
type ComparisonType = EQ

// type LambdaType// = ('a -> 'b) -> 'a list -> 'b list
//'a -> 'a   //unit//(('a) list) -> (('b) list)     //'a -> unit -> 'a


type Value =
  | IntValue of int32
  | BooleanValue of bool
  | FunctionValue of ((Value list) -> Value )

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
  | BinaryOp of Operator * Expression * Expression
  | IfExpression of Expression * Expression * Expression
  | ComparisonExpression of ComparisonType * Expression * Expression
  | LetExpression of NameType * Expression * Expression
  | VariableExpression of NameType
  | FunctionCallExpression of Expression * (Expression list)
  | FunctionDeclarationExpression of NameType * (NameType list) * Expression 

type FunctionType = Function of NameType * (NameType list) * Expression 

let rec findFunction (functionName, functionList) = 
  match functionList with
  | [] -> Function(Name("Error: Function not found"), [], BooleanConstant(false))
  | head :: tail -> 
    match head with
    | (Function(varName, p, b)) -> 
      if (varName = functionName) then head
      else findFunction (functionName, tail)

let mutable currDynamicScopeEnv = DynamicScopedEnvironment([])
let mutable actualValues = []

let rec bindArgumentsToValues (argumentNames, argumentValues, e) = 
  match (argumentNames, argumentValues, e) with
  | ([], _, _) -> e
  | (_, [], _) -> e
  | ((argName :: nameRest), (argVal :: valRest), DynamicScopedEnvironment(bindingsList)) -> 
    let newBindings = bind(argName, argVal, bindingsList)
    bindArgumentsToValues(nameRest, valRest, DynamicScopedEnvironment(newBindings))
  | ((argName :: nameRest), (argVal :: valRest), LexicalScopedEnvironment(bindingsList)) -> 
    let newBindings = bind(argName, argVal, bindingsList)
    bindArgumentsToValues(nameRest, valRest, LexicalScopedEnvironment(newBindings))

let rec eval (c, e) =
  match c with
  | IntConstant(value) -> (IntValue value)
  | BooleanConstant(value) -> (BooleanValue value)
  | BinaryOp(op,left,right) ->
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
    
  | FunctionCallExpression(function_, paramsToEvaluate) ->
    let rec getActualValues (actualArguments, evaluatedValues) = 
      match actualArguments with
      | [] -> List.rev evaluatedValues
      | head :: tail -> getActualValues(tail, (eval(head, e))::evaluatedValues)
    
    actualValues <- getActualValues(paramsToEvaluate, [])
    
    let successful = eval (function_, e) // f(actualValues)
    successful
    // let mutable argumentValues = []

    // for paramVal in paramsToEvaluate do
    //   argumentValues <- ((eval(paramVal, e)) :: argumentValues)
    
    // argumentValues <- List.rev argumentValues
    // let evaluatingEnvironment = bindArgumentsToValues (arguments, argumentValues, e)
    


  | FunctionDeclarationExpression (functionName, argumentNames, body) ->
    eval(body, e)


    // let retVal = fun (actualValues)-> body
    // retVal


    // FunctionValue(fun actualArguments ->
    //   let evaluatingEnvironment = bindArgumentsToValues (argumentNames, actualArguments, e)
    //   eval(body, evaluatingEnvironment)
    //   )


// 474
let p1 = IntConstant(474)     

// 400 + 74
let p2 = BinaryOp(PLUS, IntConstant(400), IntConstant(74)) 

// 400 + (70 + 4)
let p3 = BinaryOp(PLUS, IntConstant(400), BinaryOp(PLUS, IntConstant(70), IntConstant(4)))

// 400 + true
let p4 = BinaryOp(PLUS, IntConstant(400), BooleanConstant(true)) 

// if (474 - (400 + 74) == 0) 0 else (1 / (474 - (400 + 74)))
let p5 = IfExpression(
  ComparisonExpression(
    EQ, 
    BinaryOp(MINUS, IntConstant(474), BinaryOp(PLUS, IntConstant(400), IntConstant(74))), 
    IntConstant(0)
  ), 
  IntConstant(0), 
  BinaryOp(DIV, IntConstant(1), BinaryOp(MINUS, IntConstant(474), BinaryOp(PLUS, IntConstant(400), IntConstant(74))))
)

// { let operand = (474 - (400 + 74));
//    if (operand == 0) 0 else 474 / operand;
// }
let p6 = LetExpression(
  Name("operand"), 
  BinaryOp(MINUS, IntConstant(474), BinaryOp(PLUS, IntConstant(400), IntConstant(74))), 
  IfExpression(
    ComparisonExpression(
      EQ,
      VariableExpression(Name("operand")), 
      IntConstant(0)
    ), 
    IntConstant(0), 
    BinaryOp(DIV, IntConstant(474), VariableExpression(Name("operand"))
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
  BinaryOp(
    PLUS, 
    LetExpression(
      Name("x"), 
      IntConstant(20), 
      VariableExpression(Name("x"))
    ), 
    VariableExpression(Name("x"))
  )
)

// let params = 

// safeDivision(top, bot) : if (bot == 0) then 0 else top/bot
let safeDivision = Function(  // NameType * (NameType list) * Expression 
  Name("safeDivision"), 
  [Name("top"); Name("bot")],
  IfExpression(
    ComparisonExpression(
      EQ, 
      VariableExpression(
        Name("bot")
      ), 
      IntConstant(0)
    ), 
    IntConstant(0), 
    BinaryOp(
       DIV, 
       VariableExpression(
         Name("bot")
       ), 
       VariableExpression(
         Name("top")
       )
    )
  )
)

// fact(x) : if (x == 1) then 1 else x * fact(x-1)
let fact = Function(  // NameType * (Expression list)
  Name("fact"),  
  [Name("x")],
  IfExpression(
    ComparisonExpression(
      EQ, 
      VariableExpression(
        Name("x")
      ), 
      IntConstant(1)
    ), 
    IntConstant(1), 
    BinaryOp(
      TIMES, 
      VariableExpression(
        Name("x")
      ), 
      FunctionCallExpression(
        VariableExpression(Name("fact")), 
        [BinaryOp(
          MINUS, 
           VariableExpression(
             Name("x")
            ),
          IntConstant(1)
        )]
      )
    )
  )
)

// safeDivision(474, (474 - (400 + 74)))
let p8 = FunctionCallExpression(  // NameType * (Expression list)
  VariableExpression(Name("safeDivision")), 
  [IntConstant(474); BinaryOp(MINUS,IntConstant(474),BinaryOp(PLUS, IntConstant(400), IntConstant(74)))]
)

// fact(5)
let p9 = FunctionCallExpression(
  VariableExpression(Name("fact")), 
  [IntConstant(5)]
)

// { let safeDivision = function(top, bot){ if (bot == 0) then 0 else top/bot }
//     safeDivision(474, (474 - (400 + 74)))
// }
let p10 = LetExpression(
  Name("safeDivision"), 
  FunctionDeclarationExpression( // here :) Add to Expressions :)
    Name("safeDivision"), 
    [Name("top"); 
    Name("bot")], 
    IfExpression(
    ComparisonExpression(
      EQ, 
      VariableExpression(
        Name("bot")
      ), 
      IntConstant(0)
    ), 
    IntConstant(0), 
    BinaryOp(
       DIV, 
       VariableExpression(
         Name("bot")
       ), 
       VariableExpression(
         Name("top")
       )
    )
  )
  ),
  FunctionCallExpression(
    VariableExpression(Name("safeDivision")), 
    [IntConstant(474); 
    BinaryOp(
      MINUS,
      IntConstant(474),
      BinaryOp(
        PLUS, 
        IntConstant(400), 
        IntConstant(74)
      )
    )]
  ) 
)

// { let safeDivision = function(top, bot){ if (bot == 0) then 0 else top/bot }
//   { let fact = function(x) { if (x ==1) then 1 else x * fact(x-1)}
//     safeDivision(474, (474 - (400 + 74)))
//   }
// }


let e = LexicalScopedEnvironment([])

printfn "\ncurr result: %A" (eval(p10, e))
































