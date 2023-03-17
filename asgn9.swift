


struct NumC {
    let value: Double
}


struct LamC {
    let arg: [String]
    let body: ExprC
}

struct AppC {
    let fun: ExprC
    let args: [ExprC]
}



struct IdC {
    let name: String
}

indirect enum ExprC {
    case num(NumC)
    // case string(StringC)
    case id(IdC)
    case app(AppC)
    case lam(LamC)
    // case ifc(IfC)

}

struct Binding {
    let key: String
    let value: Value
}

struct NumV {
    let value: Double
}

struct CloV {
    let args: [String]
    let body : ExprC
    let env : [Binding]
}

struct OpV {
    let operand: String
}

enum Value {
    case numV(NumV)
    case closure(CloV)
    case operation(OpV)
}

let topEnv: [Binding] = [
    Binding(key: "+", value: .operation(OpV(operand: "+"))),
    Binding(key: "-", value: .operation(OpV(operand: "-"))),
    Binding(key: "/", value: .operation(OpV(operand: "/"))),
    Binding(key: "*", value: .operation(OpV(operand: "*")))
]

func interp(_ expr: ExprC, _ env: [Binding]) -> Value {
    switch expr {
    case .num(let num):
        return .numV(NumV(value: num.value))
        
    case .id(let id):
        for binding in env.reversed() {
            if binding.key == id.name {
                return binding.value
            }
        }
        fatalError("Undefined identifier: \(id.name)")
        
    case .app(let app):
        let fun = interp(app.fun, env)
        
        switch fun {
        case .closure(let clo):
            let args = app.args.map { interp($0, env) }
            let newEnv = clo.env + zip(clo.args, args).map { Binding(key: $0, value: $1) }
            return interp(clo.body, newEnv)
            
        case .operation(let op):
            let args = app.args.map { interp($0, env) }
            switch op.operand {
            case "+":
                let sum = args.reduce(0) { acc, arg in
                    guard case let .numV(num) = arg else {
                        fatalError("not a number + operator")
                    }
                    return acc + Int(num.value)
                }
                return .numV(NumV(value: Double(sum)))
                
            case "-":
                guard args.count == 2 else {
                    fatalError("need two arguments")
                }
                guard case let .numV(num1) = args[0], case let .numV(num2) = args[1] else {
                    fatalError("not a number - operator")
                }
                return .numV(NumV(value: num1.value - num2.value))
                
            case "*":
                let prod = args.reduce(1) { acc, arg in
                    guard case let .numV(num) = arg else {
                        fatalError("not a number * operator")
                    }
                    return acc * Int(num.value)
                }
                return .numV(NumV(value: Double(prod)))
                
            case "/":
                guard args.count == 2 else {
                    fatalError("two args needed for /")
                }
                guard case let .numV(num1) = args[0], case let .numV(num2) = args[1] else {
                    fatalError("not a number / operator")
                }
                guard num2.value != 0 else {
                    fatalError("Division by zero")
                }
                return .numV(NumV(value: num1.value / num2.value))
                
            default:
                fatalError("Unknown operator: \(op.operand)")
            }
            
        default:
            fatalError("Invalid application")
        }
        
    case .lam(let lam):
        return .closure(CloV(args: lam.arg, body: lam.body, env: env))
    }
}


let expr = ExprC.app(
    AppC(
        fun: ExprC.id(IdC(name: "*")),
        args: [
            ExprC.num(NumC(value: 2)),
            ExprC.num(NumC(value: 3))
        ]
    )
)

let result = interp(expr, topEnv)

if case .numV(let num) = result {
    if(num.value != 6.0){
        fatalError("Values are not equal")
    }
    print(num.value)
}


// Define a lambda expression that takes a single argument and adds 1 to it
let addOneLam = ExprC.lam(
    LamC(
        arg: ["x"], 
        body: ExprC.app(AppC(
                            fun: ExprC.id(IdC(name: "+")), 
                            args: [ExprC.num(NumC(value: 1.0)), ExprC.id(IdC(name: "x"))]
                            )
                        )
        )
)

// Define an expression that calls the lambda expression with argument 2
let callAddOneLam = ExprC.app(
    AppC(
        fun: addOneLam, 
        args: [ExprC.num(NumC(value: 2.0))]
        )
)

// Call interp with the lambda expression and the top-level environment
let result2 = interp(callAddOneLam, topEnv)

if case .numV(let num) = result2 {
    if(num.value != 3.0){
        fatalError("Values are not equal")
    }
    print(num.value)
}


let addOneLamandAppC = ExprC.lam(
    LamC(
        arg: ["x"], 
        body: ExprC.app(AppC(
                            fun: ExprC.id(IdC(name: "+")), 
                            args: [ExprC.app(
                                    AppC(
                                        fun: ExprC.id(IdC(name: "*")),
                                        args: [
                                            ExprC.num(NumC(value: 2)),
                                            ExprC.num(NumC(value: 3))
                                        ]
                                    )
                                ), ExprC.id(IdC(name: "x"))]
                            )
                        )
        )
)
let callAddOneLamAppC = ExprC.app(
    AppC(
        fun: addOneLamandAppC, 
        args: [ExprC.num(NumC(value: 2.0))]
        )
)

let result3 = interp(callAddOneLamAppC, topEnv)

if case .numV(let num) = result3 {
    if(num.value != 8.0){
        fatalError("Values are not equal")
    }
    print(num.value)
}


func parseExpr(tokens: inout [String]) -> ExprC? {
    guard let token = tokens.first else {
        return nil
    }
    tokens.removeFirst()
    
    switch token {
    case "(":
        return parseApplication(tokens: &tokens) ?? parseLambda(tokens: &tokens) ?? parseExpr(tokens: &tokens)
    case ")":
        return nil
    default:
        if let num = Double(token) {
            return .num(NumC(value: num))
        } else {
            return .id(IdC(name: token))
        }
    }
}

func parseApplication(tokens: inout [String]) -> ExprC? {
    guard let fun = parseExpr(tokens: &tokens) else {
        return nil
    }
    var args: [ExprC] = []
    while let arg = parseExpr(tokens: &tokens) {
        args.append(arg)
    }
    return .app(AppC(fun: fun, args: args))
}

func parseLambda(tokens: inout [String]) -> ExprC? {
    guard let openParen = tokens.first, openParen == "(",
          let lambdaToken = tokens.dropFirst().first, lambdaToken == "(",
          let argToken = tokens.dropFirst(2).first,
          let colonToken = tokens.dropFirst(3).first, colonToken == ":",
          let body = parseExpr(tokens: &tokens),
          let closeParen = tokens.dropFirst(4).first, closeParen == ")"
    else {
        return nil
    }
    tokens.removeFirst(5)
    let arg = [argToken]
    return .lam(LamC(arg: arg, body: body))
}

func parse(input: [String]) -> ExprC? {
    var tokens = input
    return parseExpr(tokens: &tokens)
}

let input = ["(", "(", "x", ")", ":", "(", "+", "x", "2", ")", ")"]
if let result3 = parse(input: input) {
    print(result3)
} else {
    print("Error: Could not parse input")
}




// func parse(sexp : [String], index : Int) -> ExprC {
//     if sexp[index] == "("{
//         let index = index + 1
//         return parse(sexp : sexp, index : index)
//     }
//     if sexp[index] == ")"{
//         let index = index + 1
//         return parse(sexp : sexp, index : index)
//     }

//     if sexp[index] == "2"{

//         if let doubleValue = Double(sexp[index]) {
//             return .num(NumC(value: doubleValue))
//         } else {
//             return .num(NumC(value: 0))
//         }
         
//     }

//     return .num(NumC(value: 0))
// }

// print(parse(sexp : ["(", "+", "1", "2", ")"], index : 0))