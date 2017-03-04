//
//  Either.swift
//  Khatru
//
//  Created by Robert Widmann on 6/23/16.
//  Copyright © 2016 Robert Widmann. All rights reserved.
//

public indirect enum Either<L, R> {
	case Left(L)
	case Right(R)

	public func either<C>(_ fl : (L) -> C, _ fr : (R) -> C) -> C {
		switch self {
		case let .Left(l):
			return fl(l)
		case let .Right(r):
			return fr(r)
		}
	}

	public func map<S>(_ f : (R) -> S) -> Either<L, S> {
		switch self {
		case let .Left(l):
			return .Left(l)
		case let .Right(r):
			return .Right(f(r))
		}
	}

	public func mapLeft<S>(_ f : (L) -> S) -> Either<S, R> {
		switch self {
		case let .Left(l):
			return .Left(f(l))
		case let .Right(r):
			return .Right(r)
		}
	}

	public func bind<S>(_ f : (R) -> Either<L, S>) -> Either<L, S> {
		switch self {
		case let .Left(l):
			return .Left(l)
		case let .Right(r):
			return f(r)
		}
	}

  public func then<S>(_ k : Either<L, S>) -> Either<L, S> {
    return self.bind { _ in k }
  }
}

extension Either {
  public static func pure(_ r : R) -> Either<L, R> {
    return .Right(r)
  }
}

public func lift<T>(_ f : @autoclosure () throws -> T) -> Either<Error, T> {
  do {
    return Either.Right(try f())
  } catch let e {
    return Either.Left(e)
  }
}

public func <^> <L, RA, RB>(f : (RA) -> RB, e : Either<L, RA>) -> Either<L, RB> {
  switch e {
  case let .Left(l):
    return .Left(l)
  case let .Right(r):
    return .Right(f(r))
  }
}

func <*> <L, X, Y>(p : Either<L, (X) -> Y>, q : Either<L, X>) -> Either<L, Y> {
	switch p {
	case let .Left(l):
		return .Left(l)
	case let .Right(f):
		return q.map(f)
	}
}

public func sequence<L, R>(_ ms : [Either<L, R>]) -> Either<L, [R]> {
  return ms.reduce(Either<L, [R]>.pure([]), { n, m in
    return n.bind { xs in
      return m.bind { x in
        return Either<L, [R]>.pure(xs + [x])
      }
    }
  })
}
