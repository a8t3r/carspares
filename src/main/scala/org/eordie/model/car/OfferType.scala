package org.eordie.model.car

import org.eordie.model.user.{SystemUser, User}

import net.liftweb.json.JsonDSL._
import net.liftweb.json.JsonAST
import net.liftweb.json.JsonAST.{JObject, JArray}
import net.liftweb.mongodb.Limit
import java.io.EOFException
import net.liftweb.common.Loggable

/**
 *
 * @author Alexandr Kolosov
 * @since 10/20/12
 */
object OfferType extends Enumeration with Loggable {

  type OfferType = Value

  val myOffers, myOrders, open, completed = Value

  def fetch(offerType: Value): List[Offer] = {
    Offer.findAll(query(offerType))
  }

  // trying to avoid mongodb communication problems - need more exploration
  def managerOffers: List[Offer] = {
    try {
      val query: JObject = managerOffersQuery
      Offer.findAll(query, ("updatedAt" -> -1), Limit(10))
    } catch {
      case ioe: EOFException => {
        logger.info("Manager offers error", ioe)
        List[Offer]()
      }
    }
  }

  def query(offerType: OfferType.Value): JsonAST.JObject = {
    val userId: String = User.currentUser.openTheBox.userIdAsString

    offerType match {
      // Тип объявления и создатель, соответствующий текущему пользователю
      case OfferType.myOffers => ("status" -> ("$ne" -> 2)) ~ ("creator" -> userId)
      case OfferType.myOrders => ("status" -> ("$ne" -> 2)) ~ ("purchaser" -> userId)

      // Для менеджера отобразить все объявления клиентов
      case OfferType.open if User.hasRole("manager") => ("status" -> ("$ne" -> 2)) ~ ("creator" -> ("$ne" -> userId))

      // Для клиентов отобразить все объявления менеджера
      case OfferType.open => managerOffersQuery

      // Завершенные сделки - закрытые обяъвления, в которых принимал участие (был продавцом или покупателем) текущий пользователь
      case OfferType.completed => ("status" -> 2) ~ ("$or" -> JArray(List(("creator" -> userId), ("purchaser") -> userId)))
    }
  }

  // Открытые предложения менеджера, у которых нет покупателя
  private def managerOffersQuery = ("status" -> 0) ~ ("creator" -> SystemUser.user.userIdAsString) ~ ("purchaser" -> ("$exists" -> false))
}
