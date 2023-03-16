struct NumC {
    let value: Double
}

struct StringC {
    let value: String
}

struct LamC {
    let arg: [String]
    let body: ExprC
}

struct AppC {
    let fun: ExprC
    let args: [ExprC]
}

struct IfC {
    let ifcase: ExprC
    let thencase : ExprC
    let elsecase : ExprC 
}

struct IdC {
    let name: String
}

indirect enum ExprC {
    case num(NumC)
    case string(StringC)
    case id(IdC)
    case app(AppC)
    case ifc(IfC)
    case lam(LamC)
}

func parse(sexp : [String], index : Int) -> ExprC {
    if sexp[index] == "("{
        let index = index + 1
        return parse(sexp : sexp, index : index)
    }
    if sexp[index] == ")"{
        let index = index + 1
        return parse(sexp : sexp, index : index)
    }

    if sexp[index] == "2"{

        if let doubleValue = Double(sexp[index]) {
            return .num(NumC(value: doubleValue))
        } else {
            return .num(NumC(value: 0))
        }
         
    }

    return .num(NumC(value: 0))
}

print(parse(sexp : ["(", "2", ")"], index : 0))


