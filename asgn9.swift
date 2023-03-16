import Foundation

print("Please enter your input:")
if let userInput = readLine() {
    print("It's true, \(userInput)")
}

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

extension String {
    func ends(with suffix: String) -> Bool {
        guard let range = range(of: suffix, options: [.backwards, .anchored]) else {
            return false
        }
        return range.upperBound == endIndex
    }
}

func parse(sexp: [String], index: Int) -> ExprC {
    if sexp[index] == "(" {
        let index = index + 1
        if sexp[index] == "if" {
            return parseIf(sexp: sexp, index: index)
        } else if sexp[index] == "lambda" {
            return parseLam(sexp: sexp, index: index)
        } else {
            return parseApp(sexp: sexp, index: index)
        }
    }

    if let doubleValue = Double(sexp[index]) {
        return .num(NumC(value: doubleValue))
    } else if sexp[index].starts(with: "\"") && sexp[index].ends(with: "\"") {
        let value = String(sexp[index].dropFirst().dropLast())
        return .string(StringC(value: value))
    } else {
        return .id(IdC(name: sexp[index]))
    }
}

func parseApp(sexp: [String], index: Int) -> ExprC {
    let fun = parse(sexp: sexp, index: index)
    var args: [ExprC] = []
    var currentIndex = index + 1 // add 1 to skip opening parens
    while sexp[currentIndex] != ")" {
        let arg = parse(sexp: sexp, index: currentIndex)
        args.append(arg)
        currentIndex += 1 // iterate thru the args, until we close the list
    }
    return .app(AppC(fun: fun, args: args))
}

func parseLam(sexp: [String], index: Int) -> ExprC {
    let argNames = sexp[index+1].components(separatedBy: " ") // add 1 to skip over "lambda"
    let bodyIndex = index + argNames.count + 2 // add 2 to skip over the "lambda" and argument list
    let body = parse(sexp: sexp, index: bodyIndex)
    let lam = LamC(arg: argNames, body: body)
    return .lam(lam)
}

func parseIf(sexp: [String], index: Int) -> ExprC {
    let ifcaseIndex = index + 1
    let thencaseIndex = ifcaseIndex + 1
    let elsecaseIndex = thencaseIndex + 1
    let ifcase = parse(sexp: sexp, index: ifcaseIndex)
    let thencase = parse(sexp: sexp, index: thencaseIndex)
    let elsecase = parse(sexp: sexp, index: elsecaseIndex)
    let ifexpr = IfC(ifcase: ifcase, thencase: thencase, elsecase: elsecase)
    return .ifc(ifexpr)
}

print(parse(sexp : ["(", "2", ")"], index : 0))

