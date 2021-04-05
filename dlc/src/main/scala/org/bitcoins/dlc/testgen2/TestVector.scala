package org.bitcoins.dlc.testgen2

import ujson.Value

trait TestVector {
  def toJson: Value

  def toJsonStr: String = {
    toJson.render(indent = 2)
  }
}

trait TestVectorParser[T <: TestVector] {
  def fromJson(json: Value): T

  def fromString(str: String): T = {
    fromJson(ujson.read(str))
  }
}
