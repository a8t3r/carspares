package org.eordie.snippet

import org.eordie.lib._
import org.eordie.model.car.{OfferType, Offer}
import net.liftweb.record.Field
import net.liftweb.http.S
import net.liftweb.util.Bindable
import xml.NodeSeq
import net.liftweb.util.Helpers._

/**
 *
 * @author Alexandr Kolosov
 * @since 10/7/12
 */
class DemoTable {

  def table = {

    // Тип отображаемых записей
    val qType: String = S.attr("type") openOr "open"

    val all: List[Offer] = OfferType.fetch(OfferType.withName(qType))
    val fields: List[Field[_, Offer]] = Offer.fieldsForList
    val cols: List[String] = for (field <- fields) yield field.displayName
    val count: Int = all.size
    val data: List[List[String]] = for (offer <- all) yield
      for (field <- fields) yield offer.fieldByName(field.name).openTheBox.asInstanceOf[Bindable].asHtml.toString()

    // this function returns data to populate table based on parameters passed in
    val fun = (params: DataTableParams) => {
      new DataTableArraySource(count, count, data)
    }

    DataTable(
      cols, fun,
      "my-table",
      List(("bJQueryUI", "true")),
      ("class", "table table-striped table-bordered")) // set css class for table
  }
}