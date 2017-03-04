//
//  Surface.swift
//  PianoForte
//
//  Created by Robert Widmann on 3/4/17.
//  Copyright © 2017 TypeLift. All rights reserved.
//

let parseName : Parser<Token, Name> = { t in
  return Name(name: t.contents, location: t.location)
} <^> tokenFilter({ t in t.isIdentifier })
let parseNameList : Parser<Token, [Name]> = many(parseName)

public enum RawDecl {
  case type(Name, [(Variance, Name)], TypeTerm)
  case opaqueType(Name, [(Variance, Name)])
  case def(Name, [(Name, TypeTerm)], Expression)
  case `let`(Name, Expression)
}

public let parseDeclList : Parser<Token, [RawDecl]> = sepBy(Keyword.semi.parse, parseDecl)
let parseDecl : Parser<Token, RawDecl> =
    (Keyword.`let`.parse *> parseName >>- { n in
      return Keyword.equals.parse *> parseLambdaExp() >>- { e in
        return pure(RawDecl.`let`(n, e))
      }
    })
  | (Keyword.def.parse *> parseName >>- { f in
      return Keyword.lparen.parse *> parseParams >>- { p in
        return Keyword.rparen.parse *> parseFunBody() >>- { e in
          return pure(RawDecl.def(f, p, e))
        }
      }
    })
  | (Keyword.type.parse *> parseName >>- { n in
      return optional(Keyword.lbrack.parse *> sepBy1(Keyword.comma.parse, zip(parseVariance, parseName)) <* Keyword.rbrack.parse) >>- { args in
        return Keyword.equals.parse *> parseTypeTerm() >>- { t in
          return pure(RawDecl.type(n, args ?? [(Variance, Name)](), t))
        }
      }
    })
  | (Keyword.type.parse *> parseName >>- { n in
      return optional(Keyword.lbrack.parse *> sepBy1(Keyword.comma.parse, zip(parseVariance, parseName)) <* Keyword.rbrack.parse) >>- { args in
        return pure(RawDecl.opaqueType(n, args ?? [(Variance, Name)]()))
      }
    })

func parseFunBodyCode() -> Parser<Token, Expression> {
  return
    (Keyword.equals.parse *> parseLambdaExp())
  | (Keyword.`do`.parse *> parseBlock() <* Keyword.end.parse)
}

func parseFunBody() -> Parser<Token, Expression> {
  return
    parseFunBodyCode()
  | (Keyword.colon.parse *> zip(parseTypeTerm(), parseFunBodyCode()) >>- { (t, e) in
      return pure(Expression.typed(e, t))
    })
}

let parseVariance : Parser<Token, Variance> =
      (Keyword.plus.parse *> pure(Variance.positive))
    | (Keyword.minus.parse *> pure(Variance.negative))
    | (Keyword.plus.parse *> Keyword.minus.parse *> pure(Variance.mixed))
    | (Keyword.minus.parse *> Keyword.plus.parse *> pure(Variance.mixed))

let parseTypeParam : Parser<Token, (Variance, Name)> = zip(parseVariance, parseName)

func parseBlock() -> Parser<Token, Expression> {
  return
    (Keyword.let.parse *> parseName >>- { v in Keyword.equals.parse *> parseLambdaExp() >>- { e1 in parseBlock() >>- { e2 in pure(Expression.let(v, e1, e2)) } } })
  | (zip(parseLambdaExp(), parseLambdaExp()) >>- { (e1, e2) in
      return pure(Expression.seq(e1, e2))
    })
  | (zip(parseLambdaExp(), Keyword.semi.parse) >>- { (e1, _) in
      return pure(Expression.seq(e1, .unit))
    })
  | parseLambdaExp()
}


func parseTerm() -> Parser<Token, Expression> {
  return
    (Keyword.`if`.parse *> parseSimpleExpr() >>- { cond in
      return Keyword.then.parse *> parseBlock() >>- { tcase in
        return Keyword.else.parse *> parseBlock() >>- { fcase in
          return pure(Expression.if(cond, tcase, fcase))
        }
      }
    })
//  | (Keyword.match.parse *> sepBy1(Keyword.comma.parse, parseSimpleExpr()) >>- { e in
//      return
//    })
// | MATCH; e = separated_nonempty_list(COMMA, simple_exp); snl; c = nonempty_list(case); END
// { Match (e, c) }
  | (parseName >>- { v in pure(Expression.variable(v)) })
  | (Keyword.lparen.parse *> parseLambdaExp() <* Keyword.rparen.parse)
  | (Keyword.lparen.parse *> parseLambdaExp() >>- { e in
      return Keyword.colon.parse *> parseTypeTerm() >>- { t in
        return pure(Expression.typed(e, t))
      }
    } <* Keyword.rparen.parse)
  | (parseTerm() >>- { f in
      return Keyword.lparen.parse *> sepBy(Keyword.comma.parse, parseLambdaExp()) >>- { x in
        return pure(Expression.app(f, x))
      } <* Keyword.rparen.parse
    })
  | (Keyword.lparen.parse *> Keyword.rparen.parse *> pure(Expression.unit))
  | (Keyword.lbrack.parse *> Keyword.rbrack.parse *> pure(Expression.`nil`))
//  | LBRACK; e = nonemptylist_r; RBRACK
//    { e }
  | (parseTerm() >>- { e in
      return Keyword.dot.parse *> parseName >>- { f in pure(Expression.project(e, f)) }
    })
//  | i = INT { Int i }
//  | t = tag { Object (Some t, []) }
//  | t = tag; LBRACE; o = separated_list(COMMA, objfield(parseLambdaExp)); RBRACE
//      { Object (Some t, o) }
//  | LBRACE; o = separated_list(COMMA, objfield(parseLambdaExp)); RBRACE
//      { Object (None, o) }
  | (Keyword.`true`.parse *> pure(Expression.bool(true)))
  | (Keyword.`false`.parse *> pure(Expression.bool(false)))
}

let parseParams : Parser<Token, [(Name, TypeTerm)]> = sepBy(Keyword.comma.parse, parseParamType)
let parseParamType : Parser<Token, (Name, TypeTerm)> = parseName >>- { n in
  return Keyword.colon.parse *> parseTypeTerm() >>- { tt in
    return pure((n, tt))
  }
}

let parseTypearg : Parser<Token, TyArgTerm<TypeTerm>> =
   (Keyword.plus.parse *> parseTypeTerm() >>- { t in pure(.varSpec(TyArg<TypeTerm>.positive(t))) })
 | (Keyword.minus.parse *> parseTypeTerm() >>- { t in pure(.varSpec(TyArg<TypeTerm>.negative(t))) })
 | (Keyword.minus.parse *> parseTypeTerm() >>- { s in
    return Keyword.plus.parse *> parseTypeTerm() >>- { t in
      return pure(.varSpec(TyArg<TypeTerm>.mixed(s, t)))
    }
 })
 | (Keyword.plus.parse *> parseTypeTerm() >>- { t in
    return Keyword.minus.parse *> parseTypeTerm() >>- { s in
      return pure(.varSpec(TyArg<TypeTerm>.mixed(s, t)))
    }
 })
 | (parseTypeTerm() >>- { t in pure(.varUnspec(t)) } )

func parseFunType() -> Parser<Token, [(Optional<Name>, TypeTerm)]> {
  return
    (Keyword.rparen.parse *> pure([]))
  | (parseTypeTerm() >>- { t in
      return Keyword.comma.parse *> parseFunType() >>- { ts in
        return pure([(.none, t)] + ts)
      }
    })
  | (parseTypeTerm() >>- { t in
      return Keyword.rparen.parse *> pure([(.none, t)])
    })
  | (parseName >>- { v in
      return Keyword.colon.parse *> parseTypeTerm() >>- { t in
        return Keyword.comma.parse *> parseFunType() >>- { ts in
          return pure([(.some(v), t)] + ts)
        }
      }
    })
  | (parseName >>- { v in
      return Keyword.colon.parse *> parseTypeTerm() >>- { t in
        return Keyword.rparen.parse *> pure([(.some(v), t)])
      }
    })
}

let parseObjType : Parser<Token, (Name, () -> TypeTerm)> = parseName >>- { v in
  return Keyword.colon.parse *> parseTypeTerm() >>- { t in
    return pure((v, { _ in t }))
  }
}

let parseMeetJoin : Parser<Token, Polarity> =
    (Keyword.and.parse *> pure(Polarity.negative))
  | (Keyword.or.parse *> pure(Polarity.positive))

func parseTypeTerm() -> Parser<Token, TypeTerm> {
  return
    (parseName >>- { v in
      return Keyword.lbrack.parse *> parseName >>- { v in
        return sepBy1(Keyword.comma.parse, parseTypearg) >>- { ps in
          return pure(TypeTerm.named(v, ps))
        } <* Keyword.rbrack.parse
      }
    })
  | (parseName >>- { v in pure(TypeTerm.named(v, [])) })
      // | ()
  | (Keyword.any.parse *> pure(TypeTerm.zero(.negative)))
  | (Keyword.nothing.parse *> pure(TypeTerm.zero(.positive)))
//  | LBRACE; o = separated_list(COMMA, objtype); RBRACE  { TCons (ty_obj_l o (L.pos ($startpos, $endpos))) }
//  | (Keyword.lbrace.parse *> sepBy(Keyword.comma.parse, parseObjType) >>- { o in
//      return
//    } <* Keyword.rbrace.parse)
  | (zip(parseTypeTerm(), zip(parseMeetJoin, parseTypeTerm())) >>- { (t1, t) in
        let (p, t2) = t
        return pure(TypeTerm.add(p, t1, t2))
    })
  | (Keyword.rec.parse *> parseName >>- { v in
      return Keyword.equals.parse *> parseTypeTerm() >>- { t in
        return pure(TypeTerm.rec(v, t))
      }
    })
  | (Keyword.under.parse *> pure(TypeTerm.wildcard))
  | (Keyword.lparen.parse *> parseTypeTerm() <* Keyword.rparen.parse)
}
//(* funtype includes its closing paren as a hack to avoid a conflict *)
//| LPAR; ts = funtype; ARROW; tr = typeterm 
//    { TCons (ty_fun
//        (ts |> List.filter (function (None, _) -> true | _ -> false) |> List.map (fun (_, t) -> fun _ -> t))
//        (ts |> List.fold_left (fun s (v, t) -> match v with Some v -> Typector.SMap.add v ((fun _ -> t), true) s | None -> s) Typector.SMap.empty)
//        (fun _ -> tr)
//        (L.pos ($startpos, $endpos))) }

let parseBinop : Parser<Token, String> =
    (Keyword.doubleEq.parse *> pure("(==)"))
  | (Keyword.lt.parse *> pure("(<)"))
  | (Keyword.gt.parse *> pure("(>)"))
  | (Keyword.lte.parse *> pure("(<=)"))
  | (Keyword.gte.parse *> pure("(>=)"))
  | (Keyword.plus.parse *> pure("(+)"))
  | (Keyword.minus.parse *> pure("(-)"))

func parseSimpleExpr() -> Parser<Token, Expression> {
  return
    (zip(parseSimpleExpr(), zip(parseBinop, parseSimpleExpr())) >>- { (e1, t) in
      let (op, e2) = t
      return pure(Expression.app(.variable(Name(name: op)), [ e1, e2 ]))
    })
  | (parseTerm() >>- { x in Keyword.cons.parse *> parseSimpleExpr() >>- { xs in pure(Expression.cons(x, xs)) } })
  | parseTerm()
}

func parseLambdaExp() -> Parser<Token, Expression> {
  return
    (Keyword.lt.parse *> parseParams >>- { ps in Keyword.gt.parse *> parseLambdaExp() >>- { e in pure(Expression.lambda(ps, e)) } })
  | (Keyword.`do`.parse *> parseBlock())
  | parseSimpleExpr()
}
