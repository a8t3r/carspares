package org.eordie.lib

import net.liftweb.proto.Crudify
import net.liftweb.record.Field
import xml.NodeSeq
import net.liftweb.mongodb.record.{MongoMetaRecord, MongoRecord}
import net.liftweb.common.Box
import net.liftmodules.mongoauth.Locs
import net.liftweb.http.S

/**
 *
 * @author Alexandr Kolosov
 * @since 10/6/12
 */
trait  MongoCRUDify[T <: MongoRecord[T]] extends Crudify {
  self: T with MongoMetaRecord[T]  =>

  type TheCrudType = T

  type FieldPointerType = Field[_, TheCrudType]

  override def calcPrefix = getClass.getSimpleName.toLowerCase :: Nil

  override def fieldsForDisplay: List[FieldPointerType] = metaFields().filter(_.shouldDisplay_?)

  override def computeFieldFromPointer(instance: TheCrudType, pointer: FieldPointerType): Box[FieldPointerType] = instance.fieldByName(pointer.name)

  override protected def addlMenuLocParams = List(Locs.HasRole("manager"))

  override def editMenuName = S ? "Edit"
  override def viewMenuName = S ? "View"
  override def showAllMenuName = S ? "List"

  override def viewClass = showAllClass
  override def createClass = showAllClass
  override def deleteClass = showAllClass
  override def showAllClass = "table table-condensed table-hover"

  override def findForParam(in: String): Box[TheCrudType] = {
    find(in)
  }

  override def findForList(start: Long, count: Int) = {
    findAll
  }

  override def create = createRecord

  override def buildBridge(in: TheCrudType) = new SquerylBridge(in)

  protected class SquerylBridge(in: TheCrudType) extends CrudBridge {

    def delete_! =  { delete("_id", in.id.toString); true}

    def save = {
      in.save
      true
    }

    def validate = in.validate

    def primaryKeyFieldAsString = in.id.toString
  }

  def buildFieldBridge(from: FieldPointerType): FieldPointerBridge = new SquerylFieldBridge(from)

  protected class SquerylFieldBridge(in: FieldPointerType) extends FieldPointerBridge {
    def displayHtml: NodeSeq = in.displayHtml
  }
}