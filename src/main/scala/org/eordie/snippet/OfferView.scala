package org.eordie.snippet

import net.liftweb.http.{SHtml, S}
import net.liftweb.http.SHtml._
import org.eordie.model.car.{OfferStatus, Offer}
import net.liftweb.util.Helpers._
import org.eordie.model.user.{SystemUser, User}
import net.liftweb.common.{Loggable, Full, Empty, Box}
import xml.{MetaData, Elem, Text, NodeSeq}
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

  def title = <title>Carspares: {offer.title.is}</title>

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
    }, largePrimary, "id" -> "id_editlink")
  }

  private val largePrimary = {
    ("class" -> "btn btn-primary btn-large")
  }

  // Откликнуться
  def respond_button: Elem = {
    val purchaserBox: Box[User] = User.find(offer.purchaser.get)

    purchaserBox match {
      // Если покупатель отсутствует, то ссылка на отклик
      case Empty => createButton(subscribe = true)

      // Если покупатель - текущий пользователь, то ссылка на отписку
      case Full(user) if (SystemUser.isSystem(user)) => createButton(subscribe = false)

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

  def changeStatusLink: NodeSeq = {
    offer.status.is match {
      case OfferStatus.open => SHtml.a(() => changeStatus(OfferStatus.closed), Text(S ? "offer_close_status"))
      case OfferStatus.closed => SHtml.a(() => changeStatus(OfferStatus.open), Text(S ? "offer_open_status"))
      case _ => NodeSeq.Empty
    }
  }

  // Редактировать
  def editLink: Elem = {
    val href = "/offers/edit/" + offer.id
    <a href={href}>{S ? "offer_do_edition"}</a>
  }

  def view = {

    val buttonsGroup = User.currentUser match {
      case Empty => <a href="/login">{S ? "login_required"}</a> % largePrimary
      case Full(x) => {
        if (offer.isCompleted) <span>{S ? "offer_completed"}</span> % largePrimary
        else {
          if (offer.isCreator(x)) {
            if (offer.hasPurchaser) {
              wrap(
                SHtml.a(() => changeStatus(OfferStatus.completed), Text(S ? "offer_completed_status")),
                SHtml.a(() => {
                  offer.purchaser.clear
                  offer.save
                  S.redirectTo("/offers/myoffers", () => S.notice(S ? ("offer_cancel_status_done", offer.title)))
                }, Text(S ? "offer_cancel_status"))
              )
            } else {
              wrap(editLink, changeStatusLink)
            }
          } else {
            if (SystemUser.isSystem(x)) wrap(respond_button, editLink,
              SHtml.a(() => {
                val copy: Offer = Offer.createRecord
                copy.title(offer.title.is + " (копия)")
                copy.description(offer.description.is)
                copy.amount(offer.amount.is)
                copy.realPrice(offer.realPrice.is)
                copy.images(offer.images.is)
                copy.save

                S.redirectTo("/offers/myoffers", () => S.notice(S ? ("create_offer_copy_done", copy.title)))
              }, Text(S ? "create_offer_copy"))
            )
            else respond_button
          }
        }
      }

      case _ => NodeSeq.Empty
    }

    val images: NodeSeq = OfferSnips.imageNodes(offer.images)

    "#id_dealType" #> offer.dealType &
    "#id_name *" #> offer.title.asHtml &
    "#id_description" #> offer.description.asHtml &
    "#id_amount" #> offer.amount &
    "#id_price" #> offer.realPrice.viewString &
    "#id_creator" #> offer.creator.asHtml &
    "#id_status" #> offer.status.asHtml &
    "#id_created" #> offer.createdAt &
    "#id_updated" #> offer.updatedAt &
    "#id_buttonsGroup" #> buttonsGroup &
    "#gallery" #> <div>{images}</div>
  }

  private def wrap(first: Elem, second: NodeSeq, third: NodeSeq = NodeSeq.Empty): NodeSeq = {
    <div class="btn-group">
      {first % largePrimary}
      <button class="btn btn-primary btn-large dropdown-toggle" data-toggle="dropdown"><span class="caret"></span></button>
      <ul class="dropdown-menu">
        <li>{second}</li>
        <li>{third}</li>
      </ul>
    </div>
  }
}
