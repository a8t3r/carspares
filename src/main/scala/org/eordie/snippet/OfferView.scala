package org.eordie.snippet

import net.liftweb.http.{SHtml, S}
import net.liftweb.http.SHtml._
import org.eordie.model.car.{OfferStatus, Offer}
import net.liftweb.util.Helpers._
import org.eordie.model.user.{SystemUser, User}
import net.liftweb.common.{Loggable, Full, Empty, Box}
import xml.{Elem, Text, NodeSeq}
import net.liftweb.http.js.JsCmds.{RedirectTo, SetHtml}
import net.liftweb.http.js.{JsCommands, JsCmd}
import org.eordie.config.Site

/**
 *
 * @author Alexandr Kolosov
 * @since 10/21/12
 */
class OfferView extends Loggable {

  var id = S.param("id") openOr ""

  lazy val offer: Offer = Offer.find(id).openOr(S.redirectTo("/404"))

  def header = <h2>{offer.title}</h2>

  // Отклик
  def createRespond: JsCmd = {
    val currentUser: User = User.currentUser.openTheBox
    offer.purchaser(currentUser.id.is)
    offer.save
    offer.performRespond()

    S.notice(S ? "subscribe_feedback")
    SetHtml("id_editlink", Text(S ? "cancel_subscribe"))
  }

  // Отказ
  def cancelRespond: JsCmd = {
    offer.purchaser.clear
    offer.save
    S.notice(S ? "cancel_subscribe_feedback")
    SetHtml("id_editlink", Text(S ? "subscribe"))
  }

  private def createButton(subscribe: Boolean): Elem = {
    val textMessage = if (subscribe) S ? "subscribe" else S ? "cancel_subscribe"

    ajaxButton(Text(textMessage), {() =>
      val purchaserBox: Box[User] = User.find(offer.purchaser.get)
      if (purchaserBox.isEmpty) createRespond else cancelRespond
    }, "class" -> "btn btn-primary btn-large", "id" -> "id_editlink")
  }

  def respond_button: NodeSeq = {
    val purchaserBox: Box[User] = User.find(offer.purchaser.get)

    purchaserBox match {
      // Если покупатель отсутствует, то ссылка на отклик
      case Empty => createButton(subscribe = true)

      // Если покупатель - текущий пользователь, то ссылка на отписку
      case Full(user) if (user.userIdAsString == User.currentUser.openTheBox.userIdAsString) => createButton(subscribe = false)

      // В прочих случаях ничего не делать
      case _ => <b>{S ? "no_subscribe"}</b>
    }
  }

  def changeStatus(status: OfferStatus.Value) = {
    offer.status(status)
    offer.save

    S.redirectTo(
      if (status == OfferStatus.completed) "/offers/completed" else "/offers/myoffers",
      () => S.notice(S ? ("offer_updated_status", S ? status.toString))
    )
  }

  def editLink: NodeSeq =
    if (offer.isEditable) {
      val href = "/offers/edit/" + offer.id
      <a href={href}>{S ? "offer_do_edition"}</a>
    } else <a href="#" class="disabled">Нет доступных операций</a>

  def closeLink: NodeSeq = {
    if (offer.hasPurchaser) {
      SHtml.a(() => changeStatus(OfferStatus.completed), Text(S ? "offer_completed_status"))
    } else {
      if (offer.isCreator) {
        offer.status.is match {
          case OfferStatus.open => SHtml.a(() => changeStatus(OfferStatus.closed), Text(S ? "offer_close_status"))
          case OfferStatus.closed => SHtml.a(() => changeStatus(OfferStatus.open), Text(S ? "offer_open_status"))
          case _ => NodeSeq.Empty
        }
      } else NodeSeq.Empty
    }
  }

  def view = {

    val buttonsGroup = {
      if (User.currentUser.isEmpty) <a class="btn btn-primary btn-large" href="/login">Для операций требуется вход</a>
      else {
        if (offer.isCompleted) {
          <span class="btn btn-primary btn-large disabled">{S ? "offer_completed"}</span>
        } else {
          wrap()
        }
      }
    }
    val images: NodeSeq = OfferSnips.imageNodes(offer.images)

    "#id_dealType" #> offer.dealType &
    "#id_name *" #> offer.title.asHtml &
    "#id_description" #> offer.description.asHtml &
    "#id_amount" #> offer.amount &
    "#id_price" #> offer.estimatedPrice.viewString &
    "#id_creator" #> offer.creator.asHtml &
    "#id_status" #> offer.status.asHtml &
    "#id_created" #> offer.createdAt &
    "#id_updated" #> offer.updatedAt &
    "#id_buttonsGroup" #> buttonsGroup &
    "#gallery" #> <div>{images}</div>
  }

  private def wrap(): NodeSeq = {
    <div class="btn-group">
      {if (offer.isCreator) <button class="btn btn-primary btn-large">{S ? "offer_operations"}</button> else respond_button}
      <button class="btn btn-primary btn-large dropdown-toggle" data-toggle="dropdown"><span class="caret"></span></button>
      <ul class="dropdown-menu">
        <li>{closeLink}</li>
        <li>{editLink}</li>
      </ul>
    </div>
  }
}
