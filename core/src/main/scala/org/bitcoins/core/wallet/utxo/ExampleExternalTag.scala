package org.bitcoins.core.wallet.utxo

case object ExampleTagType extends ExternalAddressTagType {
  override val typeName: String = "ExampleType"
}

sealed trait ExampleExternalTag extends ExternalAddressTag {
  override val tagType: ExternalAddressTagType = ExampleTagType
}

object ExampleExternalTag extends AddressTagFactory[ExampleExternalTag] {
  override val tagType: ExternalAddressTagType = ExampleTagType

  override val tagNames: Vector[ExternalAddressTagName] =
    Vector(ExampleNameA, ExampleNameB, ExampleNameC)

  override val all: Vector[ExampleExternalTag] =
    Vector(ExampleTagA, ExampleTagB, ExampleTagC)

  case object ExampleNameA extends ExternalAddressTagName {
    override val name: String = "A"
  }

  case object ExampleNameB extends ExternalAddressTagName {
    override val name: String = "B"
  }

  case object ExampleNameC extends ExternalAddressTagName {
    override val name: String = "C"
  }

  case object ExampleTagA extends ExampleExternalTag {
    override val tagName: ExternalAddressTagName = ExampleNameA
  }

  case object ExampleTagB extends ExampleExternalTag {
    override val tagName: ExternalAddressTagName = ExampleNameB
  }

  case object ExampleTagC extends ExampleExternalTag {
    override val tagName: ExternalAddressTagName = ExampleNameC
  }
}

object Example {

  val redundancyWithinTypeExternalAddressTag: Boolean = // Returns True
    ExternalAddressTagWrapper("A", "ExampleType") == ExampleExternalTag.ExampleTagA
}
