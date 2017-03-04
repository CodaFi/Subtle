//
//  Detritus.swift
//  Siberia
//
//  Created by Robert Widmann on 12/20/16.
//  Copyright © 2016 TypeLift. All rights reserved.
//

public indirect enum ArrayMatcher<A> {
  case Nil
  case Cons(A, [A])
}

public func const<A, B>(_ x : A, _ : B) -> A {
  return x
}

public func curry<A, B, C>(_ f : @escaping (A, B) -> C) -> (A) -> (B) -> C {
  return { (a : A) -> (B) -> C in
    return { (b : B) -> C in
      return f(a, b)
    }
  }
}

extension Array {
  public func foldr<B>(_ k : (Element) -> (B) -> B, _ i : B) -> B {
    switch self.match {
    case .Nil:
      return i
    case .Cons(let x, let xs):
      return k(x)(xs.foldr(k, i))
    }
  }

  func traverse<L, B>(_ f : @escaping (Element) -> Either<L, B>) -> Either<L, [B]> {
    return self.reduce(.Right([])) { (b, a) in
      return f(a).map { x in { xs in [x] + xs } } <*> b
    }
  }

  /// Destructures a list into its constituent parts.
  ///
  /// If the given list is empty, this function returns .Nil.  If the list is non-empty, this
  /// function returns .Cons(head, tail)
  public var match : ArrayMatcher<Element> {
    if self.count == 0 {
      return .Nil
    } else if self.count == 1 {
      return .Cons(self[0], [])
    }
    let hd = self[0]
    let tl = Array(self[1..<self.count])
    return .Cons(hd, tl)
  }

  public var matchBackward : ArrayMatcher<Element> {
    if self.count == 0 {
      return .Nil
    } else if self.count == 1 {
      return .Cons(self[0], [])
    }
    let lst = self.last!
    let ini = Array(self[0..<self.index(before: self.endIndex)])
    return .Cons(lst, ini)
  }
}

extension Dictionary {
  public init<S : Sequence>(_ seq : S) where S.Iterator.Element == Element {
    self.init()
    for (k, v) in seq {
      self[k] = v
    }
  }
}

