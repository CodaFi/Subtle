//
//  Symbol.swift
//  PianoForte
//
//  Created by Robert Widmann on 3/4/17.
//  Copyright © 2017 TypeLift. All rights reserved.
//

public struct SourceLocation : Locatable, CustomStringConvertible, CustomDebugStringConvertible {
  public let line : Int
  public let col : Int

  public init() {
    self.line = 1
    self.col = 1
  }

  public init(line : Int, col : Int) {
    self.line = line
    self.col = col
  }

  public var location : SourceLocation {
    return self
  }

  public func advanced(by offset : Int = 1) -> SourceLocation {
    return SourceLocation(line: self.line, col: self.col + offset)
  }

  public func newline(by offset : Int = 1) -> SourceLocation {
    return SourceLocation(line: self.line + offset, col: 1)
  }

  public var description : String {
    return "[\(self.line):\(self.col)]"
  }

  public var debugDescription : String {
    return self.description
  }
}

public protocol Locatable {
  var location : SourceLocation { get }
}

public struct Name : Equatable, Comparable, Hashable, Locatable, CustomStringConvertible {
  public let location : SourceLocation
  let string : String

  public init() {
    self.location = SourceLocation()
    self.string = "" // FIXME:
  }

  public init(name : String) {
    self.location = SourceLocation()
    self.string = name
  }

  public init(name : String, location : SourceLocation) {
    self.location = location
    self.string = name
  }

  public static func ==(l : Name, r : Name) -> Bool {
    return l.string == r.string
  }

  public static func <(l : Name, r : Name) -> Bool {
    return l.string < r.string
  }

  public var hashValue : Int {
    return self.string.hashValue
  }

  public var description : String {
    return self.string
  }
}


public struct QualifiedName : Locatable, Equatable, Hashable, CustomStringConvertible {
  let name : Name
  let module : [Name]

  public init() {
    self.name = Name()
    self.module = []
  }

  public init(name : String) {
    self.name = Name(name: name)
    self.module = []
  }

  public init(name : Name) {
    self.name = name
    self.module = []
  }

  public init(cons name : Name, _ ns : QualifiedName) {
    self.name = name
    self.module = [ns.name] + ns.module
  }

  public init(cons name : Name, _ module : [Name]) {
    self.name = name
    self.module = module
  }

  public var location : SourceLocation {
    return self.name.location
  }

  public var hashValue : Int {
    return self.module.reduce(self.name.hashValue) { (acc, x) in
      return acc ^ x.hashValue
    }
  }

  public static func ==(l : QualifiedName, r : QualifiedName) -> Bool {
    guard l.module.count == r.module.count else {
      return false
    }

    return zip(l.module, r.module).reduce(l.name == r.name) { (acc, x) in
      return acc && (x.0 == x.1)
    }
  }

  public var description : String {
    return self.module.reduce("") { (acc, x) in
      return acc + x.description + "."
      } + self.name.description
  }

  public var string : String {
    return self.description
  }
}
