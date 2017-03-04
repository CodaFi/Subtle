//
//  Lexer.swift
//  PianoForte
//
//  Created by Robert Widmann on 3/4/17.
//  Copyright © 2017 TypeLift. All rights reserved.
//

public let tokenize : Parser<ParsableUnicodeScalar, [Token]> = whitespace *> sepBy1(whitespace, parseToken) <* whitespace

let whitespace : Parser<ParsableUnicodeScalar, ()>
  = many(tokenFilter({ t in Set(" \n\r\t".unicodeScalars).contains(t.unScalar) })) *> pure(())

let parseToken = parseBracket | parseKeyword | ({ w in
  let stringValue = w.reduce("") { (acc, x) in acc + String(x.unScalar) }
  return Token.Identifier(w.first!.location, stringValue)
} <^> parseWord)

let parseKeyword : Parser<ParsableUnicodeScalar, Token> = pFilter({ t in
  let stringValue = t.reduce("") { (acc, x) in acc + String(x.unScalar) }
  return Keyword.allStrings[stringValue].map({ x in Token.Keyword(t.first!.location, x) })
}, parseWord)

let parseBracket : Parser<ParsableUnicodeScalar, Token> = { w in
  return Token.Keyword(w.location, Keyword(withString: String(w.unScalar))!)
} <^> tokenFilter({ t in Set("(){}[]".unicodeScalars).contains(t.unScalar) })

let parseWord : Parser<ParsableUnicodeScalar, [ParsableUnicodeScalar]>
  = some(tokenFilter({ t in !Set(" \n\t(){}[]".unicodeScalars).contains(t.unScalar) }))

public indirect enum Keyword : String {
  case lparen = "("
  case rparen = ")"
  case lbrace = "{"
  case rbrace = "}"
  case lbrack = "["
  case rbrack = "]"

  case arrow = "->"
  case comma = ","
  case semi = ";"
  case dot = "."
  case or = "|"
  case and = "&"
  case colon = ":"
  case subsume = "<:"
  case rec = "rec"
  case equals = "="
  case `let` = "let"
  case `true` = "true"
  case `false` = "false"
  case `if` = "if"
  case then = "then"
  case `else` = "else"
  case def = "def"
  case `do` = "do"
  case end = "end"
  case type = "type"
  case any = "any"
  case nothing = "nothing"

  case doubleEq = "=="
  case lt = "<"
  case gt = ">"
  case lte = "<="
  case gte = ">="
  case plus = "+"
  case minus = "-"
  case under = "_"

  case cons = "::"
  case match = "match"
  case `case` = "case"

  init?(withString str : String) {
    guard let c = Keyword.allStrings[str] else {
      return nil
    }
    self = c
  }

  static var all : [Keyword] {
    return [
      .lparen,
      .rparen,
      .lbrace,
      .rbrace,
      .lbrack,
      .rbrack,

      .arrow,
      .comma,
      .semi,
      .dot,
      .or,
      .and,
      .colon,
      .subsume,
      .rec,
      .equals,
      .`let`,
      .`true`,
      .`false`,
      .`if`,
      .then,
      .`else`,
      .def,
      .`do`,
      .end,
      .type,
      .any,
      .nothing,

      .doubleEq,
      .lt,
      .gt,
      .lte,
      .gte,
      .plus,
      .minus,
      .under,
      
      .cons,
      .match,
      .`case`,
    ]
  }

  static var allStrings : Dictionary<String, Keyword> {
    return Dictionary<String, Keyword>(zip(Keyword.all.map({ $0.rawValue }), Keyword.all).map(id))
  }

  var parse : Parser<Token, ()> {
    return parseTokenIf(Token.Keyword(SourceLocation(), self))
  }
}

public indirect enum Token : Locatable, Equatable, CustomStringConvertible, CustomDebugStringConvertible {
  case Identifier(SourceLocation, String)
  case Keyword(SourceLocation, Keyword)

  public init(location : SourceLocation, symbol : String) {
    guard let k = PianoForte.Keyword(withString: symbol) else {
      self = Token.Identifier(location, symbol)
      return
    }
    self = Token.Keyword(location, k)
  }

  public var location : SourceLocation {
    switch self {
    case let .Identifier(s, _): return s
    case let .Keyword(s, _): return s
    }
  }

  public var nextLocation : SourceLocation {
    let sl = self.location
    return SourceLocation(line: sl.line, col: sl.col + 1)
  }

  public var length : Int {
    switch self {
    case let .Identifier(_, s): return s.unicodeScalars.count
    case let .Keyword(_, s): return s.rawValue.unicodeScalars.count
    }
  }

  public var contents : String {
    switch self {
    case let .Identifier(_, s): return s
    case let .Keyword(_, s): return s.rawValue
    }
  }

  public func withLocation(_ newLoc : SourceLocation) -> Token {
    switch self {
    case let .Identifier(_, s):
      return .Identifier(newLoc, s)
    case let .Keyword(_, k):
      return .Keyword(newLoc, k)
    }
  }

  public var debugDescription : String {
    return self.description
  }

  public var isIdentifier : Bool {
    switch self {
    case .Identifier(_, _):
      return true
    default:
      return false
    }
  }

  public var description : String {
    switch self {
    case let .Identifier(loc, s):
      return s + "@\(loc)"
    case let .Keyword(loc, s):
      return s.rawValue + "@\(loc)"
    }
  }

  public static func == (l : Token, r : Token) -> Bool {
    switch (l, r) {
    case let (.Identifier(_, ls), .Identifier(_, rs)):
      return ls == rs
    case let (.Keyword(_, lk), .Keyword(_, rk)):
      return lk == rk
    default:
      return false
    }
  }
}
