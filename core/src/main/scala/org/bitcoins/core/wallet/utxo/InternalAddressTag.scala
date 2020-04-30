package org.bitcoins.core.wallet.utxo

/**
  * An AddressTagNames that is native to Bitcoin-S.
  * InternalAddressTagNames are still usable when using Bitcoin-S
  * as a dependency
  */
sealed trait InternalAddressTagName extends AddressTagName

/**
  * An AddressTagType that is native to Bitcoin-S.
  * InternalAddressTagTypes are still usable when using Bitcoin-S
  * as a dependency
  */
sealed trait InternalAddressTagType extends AddressTagType

/**
  * An AddressTag that is native to Bitcoin-S.
  * InternalAddressTags are still usable when using Bitcoin-S
  * as a dependency
  */
sealed trait InternalAddressTag extends AddressTag

/** An unknown address tag name, most likely an internal representation of an [[ExternalAddressTagName]] */
case class UnknownAddressTagName(name: String) extends InternalAddressTagName {
  require(InternalAddressTagName.fromStringOpt(name).isEmpty,
          s"This tag name is already defined, got $name")
}

/** An unknown address tag type, most likely an internal representation of an [[ExternalAddressTagType]] */
case class UnknownAddressTagType(typeName: String)
    extends InternalAddressTagType {
  require(InternalAddressTagType.fromStringOpt(typeName).isEmpty,
          s"This tag type is already defined, got $typeName")
}

/** An address tag without an unknown type; an internal representation of an [[ExternalAddressTag]] */
case class ExternalAddressTagWrapper(
    tagName: InternalAddressTagName,
    tagType: UnknownAddressTagType)
    extends InternalAddressTag {
  require(InternalAddressTagType.fromStringOpt(tagType.typeName).isEmpty,
          s"This tag type is already defined, got ${tagType.typeName}")

  def toExternal[T <: ExternalAddressTag](
      constructor: (String, String) => T): T = {
    constructor(tagName.name, tagType.typeName)
  }
}

object ExternalAddressTagWrapper {

  def apply(
      tagName: AddressTagName,
      tagType: AddressTagType): ExternalAddressTagWrapper = {
    ExternalAddressTagWrapper(tagName.name, tagType.typeName)
  }

  def apply(tagName: String, tagType: String): ExternalAddressTagWrapper =
    ExternalAddressTagWrapper(UnknownAddressTagName(tagName),
                              UnknownAddressTagType(tagType))

  def apply(
      tagName: String,
      tagType: AddressTagType): ExternalAddressTagWrapper =
    ExternalAddressTagWrapper(UnknownAddressTagName(tagName), tagType)

  def apply(
      tagName: AddressTagName,
      tagType: String): ExternalAddressTagWrapper =
    ExternalAddressTagWrapper(tagName, UnknownAddressTagType(tagType))
}

object InternalAddressTagName {

  val all: Seq[InternalAddressTagName] = StorageLocationTag.tagNames

  def fromStringOpt(string: String): Option[InternalAddressTagName] =
    all.find(_.name.toLowerCase == string.toLowerCase)

  def fromString(string: String): InternalAddressTagName =
    fromStringOpt(string).getOrElse(UnknownAddressTagName(string))
}

object InternalAddressTagType {
  val all: Seq[InternalAddressTagType] = Vector(StorageLocationTagType)

  def fromStringOpt(string: String): Option[InternalAddressTagType] =
    all.find(_.typeName.toLowerCase == string.toLowerCase)

  def fromString(string: String): InternalAddressTagType =
    fromStringOpt(string).getOrElse(UnknownAddressTagType(string))
}

object InternalAddressTag {

  def fromString(tagNameStr: String, tagTypeStr: String): InternalAddressTag = {
    val tagName = InternalAddressTagName.fromString(tagNameStr)
    val tagType = InternalAddressTagType.fromString(tagTypeStr)

    tagType match {
      case unknownType: UnknownAddressTagType =>
        ExternalAddressTagWrapper(tagName, unknownType)
      case StorageLocationTagType =>
        tagName match {
          case StorageLocationTag.HotStorageName =>
            StorageLocationTag.HotStorage
          case StorageLocationTag.ColdStorageName =>
            StorageLocationTag.ColdStorage
          case StorageLocationTag.DeepColdStorageName =>
            StorageLocationTag.DeepColdStorage
          case unknownName: UnknownAddressTagName =>
            throw new IllegalArgumentException(
              s"$unknownName is not a valid StorageLocationTag")
        }
    }
  }

  def apply(
      tagName: AddressTagName,
      tagType: AddressTagType): InternalAddressTag = {
    fromString(tagName.name, tagType.typeName)
  }
}

object StorageLocationTagType extends InternalAddressTagType {
  override val typeName: String = "StorageLocationTag"
}

/** Storage Location of the private keys associated with the address */
sealed trait StorageLocationTag extends InternalAddressTag {
  override val tagType: AddressTagType = StorageLocationTagType
}

object StorageLocationTag extends AddressTagFactory[StorageLocationTag] {

  override val tagType: InternalAddressTagType = StorageLocationTagType

  override val tagNames =
    Vector(HotStorageName, ColdStorageName, DeepColdStorageName)

  // Tag Names
  case object HotStorageName extends InternalAddressTagName {
    override def name: String = "HotStorage"
  }

  case object ColdStorageName extends InternalAddressTagName {
    override def name: String = "ColdStorage"
  }

  case object DeepColdStorageName extends InternalAddressTagName {
    override def name: String = "DeepColdStorage"
  }

  /** Keys stored on a computer connected to the internet */
  case object HotStorage extends StorageLocationTag {
    override val tagName: AddressTagName = HotStorageName
  }

  /** Keys stored on a hardware wallet or other offline device */
  case object ColdStorage extends StorageLocationTag {
    override val tagName: AddressTagName = ColdStorageName
  }

  /** Keys stored on a hardware wallet or other offline device locked in a safe in a distant location */
  case object DeepColdStorage extends StorageLocationTag {
    override val tagName: AddressTagName = DeepColdStorageName
  }

  override val all: Vector[StorageLocationTag] =
    Vector(HotStorage, ColdStorage, DeepColdStorage)
}
