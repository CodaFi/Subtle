//
//  Parser.swift
//  Khatru
//
//  Created by Robert Widmann on 6/29/16.
//  Copyright © 2016 Robert Widmann. All rights reserved.
//

//public typealias ParseFailure<X> = ([X], ParseError<X>)

public indirect enum ParseError<T> {
  case abort
  case endOfStream
  case endOfParser([T])
  case expect(T)
  case fail(String)
}

public func describeParseFailure<A>(_ pf : ([A], ParseError<A>), _ f : ([A]) -> String) -> String {
  let postMark = pf.0.isEmpty ? "" : "\nSuccessfully parsed: \(f(pf.0))"
  switch pf.1 {
  case .abort:
    return "parser aborted." + postMark
  case .endOfStream:
    return "end of stream." + postMark
  case let .endOfParser(ts):
    return "end of parser; unconsumed: \(ts)." + postMark
  case let .expect(t):
    return "expected \(f([t]))." + postMark
  case let .fail(s):
    return s
  }
}

//public typealias Parser<T, X> = ([T]) -> Either<([T], ParseError<T>), ([T], X, [T])>

/// A parser for things, once removed.
public struct Parser<T, X> {
  let unParser : ([T]) -> Either<([T], ParseError<T>), ([T], X, [T])>

  init(_ unParser : @escaping ([T]) -> Either<([T], ParseError<T>), ([T], X, [T])>) {
    self.unParser = unParser
  }
}

public func parse<X>(_ p : Parser<Character, X>, _ ts : String) -> Either<([Character], ParseError<Character>), X> {
  return parse(p, ts.characters.map { $0 })
}

public func parse<T, X>(_ p : Parser<T, X>, _ ts : [T]) -> Either<([T], ParseError<T>), X> {
  switch p.unParser(ts) {
  case let .Right((_, x, xs)) where xs.isEmpty:
    return .Right(x)
  case let .Right((ts, _, cts)):
    return Either<([T], ParseError<T>), X>.Left((ts, ParseError<T>.endOfParser(cts)))
  case let .Left(e):
    return .Left(e)
  }
}

public func zip<T, X, Y>(_ p : Parser<T, X>, _ q : Parser<T, Y>) -> Parser<T, (X, Y)> {
  return map(p, { ps in { qs in (ps, qs) } }) <*> q
}

public func map<T, X, Y>(_ p : Parser<T, X>, _ f : @escaping (X) -> Y) -> Parser<T, Y> {
  return pure(f) <*> p
}

public func pure<T, X>(_ x : X) -> Parser<T, X> {
  return Parser { (ts : [T]) in
    return .Right(([T](), x, ts))
  }
}

public func fail<T, X>(_ s : String) -> Parser<T, X> {
  return Parser { _ in Either<([T], ParseError<T>), ([T], X, [T])>.Left(([T](), ParseError<T>.fail(s))) }
}

public func empty<T, X>() -> Parser<T, X> {
  return Parser { _ in .Left(([T](), .abort)) }
}

public func <^> <T, X, Y>(_ f : @escaping (X) -> Y, _ p : Parser<T, X>) -> Parser<T, Y> {
  return pure(f) <*> p
}

public func <^ <T, X, Y>(x : X, f : Parser<T, Y>) -> Parser<T, X> {
  return map(f, { const(x, $0) })
}

public func id<T>(_ x : T) -> T {
  return x
}

public func *> <T, X, Y>(p : Parser<T, X>, q : Parser<T, Y>) -> Parser<T, Y> {
  return (id <^ p) <*> q
}

public func <* <T, X, Y>(a : Parser<T, Y>, b : Parser<T, X>) -> Parser<T, Y> {
  return ({ a in { _ in a } } <^> a) <*> b
}

public func <*> <T, X, Y>(p : Parser<T, (X) -> Y>, q : @autoclosure @escaping () -> Parser<T, X>) -> Parser<T, Y> {
  return p >>- { f in
    return q() >>- { x in
      return pure(f(x))
    }
  }
}

public func >>- <T, X, Y>(p : Parser<T, X>, f : @escaping (X) -> Parser<T, Y>) -> Parser<T, Y> {
  return Parser { ts in
    switch p.unParser(ts) {
    case let .Left(l):
      return .Left(l)
    case let .Right((sts, s2, ts2)):
      switch f(s2).unParser(ts2) {
      case let .Left(l):
        return .Left(l)
      case let .Right((tts, t2, ts3)):
        let tt : [T] = sts + tts
        return .Right((tt, t2, ts3))
      }
    }
  }
}

// One or more
public func some<T, X>(_ p : Parser<T, X>) -> Parser<T, [X]> {
  return Parser { ts in
    switch p.unParser(ts) {
    case let .Left(l):
      return .Left(l)
    case let .Right((sts, x, ts2)):
      var stv : [T] = sts
      var tsv : [T] = ts2
      var xs : [X] = [x]
      while true {
        switch p.unParser(tsv) {
        case let .Right((tts, x, ts3)) where !tts.isEmpty:
          stv += tts
          xs.append(x)
          tsv = ts3
        default:
          let vv : ([T], [X], [T]) = (stv, xs, tsv)
          let e = Either<([T], ParseError<T>), ([T], [X], [T])>.Right(vv)
          return e
        }
      }
      //      switch (many(p) | pure([X]()))(ts2) {
      //      case let .Left(l):
      //        return .Left(l)
      //      case let .Right((tts, xs, ts3)):
      //        let tt : [T] = sts + tts
      //        let xx : [X] = [x] + xs
      //        return .Right((tt, xx, ts3))
      //      }
    }
  }
}

// Zero or more.
public func many<T, X>(_ v : Parser<T, X>) -> Parser<T, [X]> {
  return some(v) | pure([X]())
}

public func optional<A, E>(_ s : Parser<E, A>) -> Parser<E, Optional<A>> {
  return map(s, { x in
    print(x)
    return .some(x)
  }) | pure(nil)
}

public func pGuard<T>(_ b : Bool) -> Parser<T, ()> {
  if b {
    return pure(())
  } else {
    return fail("")
  }
}

public func | <T, X>(p : Parser<T, X>, q : @autoclosure @escaping () -> Parser<T, X>) -> Parser<T, X> {
  return Parser { (ts : [T]) -> Either<([T], ParseError<T>), ([T], X, [T])> in
    switch p.unParser(ts) {
    case let .Right(r):
      return .Right(r)
    default:
      return q().unParser(ts)
    }
  }
}

public func choice<T, X>(_ ps : [Parser<T, X>]) -> Parser<T, X> {
  guard ps.count > 1 else {
    fatalError("Use | instead.")
  }

  return ps.dropFirst().reduce(ps.first!) { (p, ps) in
    return p | ps
  }
}

public func nextToken<T>() -> Parser<T, T> {
  return Parser { ts in
    switch ts.match {
    case .Nil:
      return .Left(([], .endOfStream))
    case let .Cons(t, ts):
      return .Right(([t], t, ts))
    }
  }
}

public func pRest<T>() -> Parser<T, [T]> {
  return Parser { ts in
    return .Right((ts, ts, [T]()))
  }
}

public func peekToken<T>() -> Parser<T, [T]> {
  return Parser { ts in
    return .Right(([T](), ts, ts))
  }
}

public func pFilter<T, A, B>(_ f : @escaping (A) -> B?, _ p : Parser<T, A>) -> Parser<T, B> {
  return p >>- { a in
    switch f(a) {
    case let .some(b):
      return pure(b)
    default:
      return empty()
    }
  }
}

public func parseTokenIf<T : Equatable>(_ t : T) -> Parser<T, ()> {
  return nextToken() >>- { x in
    if t == x {
      return pure(())
    }
    return empty()
  }
}

public func tokenFilter<T>(_ p : @escaping (T) -> Bool) -> Parser<T, T> {
  return nextToken() >>- { x in
    if p(x) {
      return pure(x)
    }
    return empty()
  }
}

public func sepBy<T, S, X>(_ sep : Parser<T, S>, _ p : Parser<T, X>) -> Parser<T, [X]> {
  return sepBy1(sep, p) | pure([])
}

public func sepBy1<T, S, X>(_ sep : Parser<T, S>, _ p : Parser<T, X>) -> Parser<T, [X]> {
  return zip(p, many(sep *> p)) >>- { (p_, ps_) in
    return pure([p_] + ps_)
  }
}

