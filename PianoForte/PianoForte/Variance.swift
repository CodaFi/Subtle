//
//  Variance.swift
//  PianoForte
//
//  Created by Robert Widmann on 3/4/17.
//  Copyright © 2017 TypeLift. All rights reserved.
//

public enum Polarity {
  case positive
  case negative

  public var invert : Polarity {
    switch self {
    case .positive:
      return .negative
    case .negative:
      return .positive
    }
  }

  public var variance : Variance {
    switch self {
    case .positive:
      return .positive
    case .negative:
      return .negative
    }
  }
}

public enum Variance : CustomStringConvertible {
  case none
  case positive
  case negative
  case mixed

  public static func join(_ v1 : Variance, _ v2 : Variance) -> Variance {
    switch (v1, v2) {
    case let (.none, x):
      return x
    case let (x, .none):
      return x
    case (.positive, .positive):
      return .positive
    case (.negative, .negative):
      return .negative
    case (.negative, .positive):
      return .mixed
    case (.positive, .negative):
      return .mixed
    case (.mixed, _):
      return .mixed
    case (_, .mixed):
      return .mixed
    default:
      fatalError("Non-exhaustive join?")
    }
  }

  public var description : String {
    switch self {
    case .none:
      return ""
    case .positive:
      return "+"
    case .negative:
      return "-"
    case .mixed:
      return "-+"
    }
  }
}
