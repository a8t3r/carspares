package org.eordie.model

import net.liftweb.record.field.NumericTypedField
import net.liftweb.http.S
import net.liftweb.common.{Full, Box}
import net.liftweb.util.Helpers
import xml.{Elem, NodeSeq}
import Helpers._

/**
 *
 * @author Alexandr Kolosov
 * @since 11/11/12
 */
trait NumericView[Number] extends NumericTypedField[Number] {

  def elem: Elem = S.fmapFunc((s: List[String]) => setFromAny(s)) {
    funcName => view(funcName)
  }

  def view(funcName: String) = <input type="number" name={funcName} value={valueBox.map(_.toString) openOr ""} tabindex={tabIndex.toString}/>

  override abstract def toForm: Box[NodeSeq] =
    uniqueFieldId match {
      case Full(id) => Full(elem % ("id" -> id))
      case _ => Full(elem)
    }
}
