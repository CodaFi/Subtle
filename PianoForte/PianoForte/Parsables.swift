//
//  Parsables.swift
//  PianoForte
//
//  Created by Robert Widmann on 3/4/17.
//  Copyright © 2017 TypeLift. All rights reserved.
//

extension String.UnicodeScalarView {
  public var parsableCharacters : [ParsableUnicodeScalar] {
    var currentState = SourceLocation()
    return self.map { c in
      let c2 = ParsableUnicodeScalar(c, currentState)
      if "\n".unicodeScalars.contains(c) {
        currentState = currentState.newline()
      } else {
        currentState = currentState.advanced()
      }
      return c2
    }
  }
}

public struct ParsableUnicodeScalar : Locatable, CustomStringConvertible {
  public let unScalar : UnicodeScalar
  public let location : SourceLocation

  public init(_ scalar : UnicodeScalar, _ loc : SourceLocation) {
    self.unScalar = scalar
    self.location = loc
  }

  public var description : String {
    return self.unScalar.description
  }
}
