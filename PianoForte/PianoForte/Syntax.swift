//
//  Syntax.swift
//  PianoForte
//
//  Created by Robert Widmann on 3/4/17.
//  Copyright © 2017 TypeLift. All rights reserved.
//

public enum TyArg<A> {
  case none
  case positive(A)
  case negative(A)
  case mixed(A, A)
}

public enum TyArgTerm<A> {
  case varSpec(TyArg<A>)
  case varUnspec(A)
}

public indirect enum TypeTerm {
  case zero(Polarity)
  case named(Name, [TyArgTerm<TypeTerm>])
  //| TCons of typeterm Components.t
  case add(Polarity, TypeTerm, TypeTerm)
  case rec(Name, TypeTerm)
  case wildcard
}

public indirect enum Pattern {
  case wildcard
  case variable(Name)
  case object(Name, [(Name, Pattern)])
  case int(Int64)
  case alt(Pattern, Pattern)
}

public indirect enum Expression {
  case variable(Name)
  case lambda([(Name, TypeTerm)], Expression)
  case `let`(Name, Expression, Expression)
  case rec(Name, Expression)
  case app(Expression, [Expression])
  case seq(Expression, Expression)
  case typed(Expression, TypeTerm)

  case match([Expression], ([Pattern], Expression))
  case object(Name, [(Name, Expression)])
  case project(Expression, Name)

  case unit
  case int(Int64)
  case bool(Bool)

  case `if`(Expression, Expression, Expression)

  case `nil`
  case cons(Expression, Expression)
}
