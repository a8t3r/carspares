package org.eordie.snippet

import net.liftweb._

import common.Full
import http.{SHtml, S}
import mongodb.record.field.MongoListField
import record.Field
import scala.xml._
import org.eordie.model.car.{DealType, Offer}
import util.BaseField
import util.Helpers._
import xml.Text

/**
 * Сниппет для создания предложения
 *
 * @author Alexandr Kolosov
 * @since 11/11/12
 */
object OfferSnips {

  def crudDoForm(item: Offer, noticeMsg: String)(in: NodeSeq): NodeSeq = {

    def loop(html:NodeSeq): NodeSeq = {
      def error(field: BaseField): NodeSeq =
        field.uniqueFieldId match {
          case fid @ Full(id) => S.getNotices.filter(_._3 == fid).flatMap(err =>
            <li class="field-error">{err._2}</li>)

          case _ => NodeSeq.Empty
        }

      def build(field: Field[_, Offer]): NodeSeq =
        if (field.shouldDisplay_?) <label class="control-label">{field.displayName}</label> % ("for" -> field.uniqueFieldId)
        else NodeSeq.Empty

      def doFields(html: NodeSeq): NodeSeq =
        for {
          pointer <- (Offer.fieldsForEditing ::: List(Offer.images))
          field <- item.fieldByName(pointer.name).toList
          if field.show_?
          form <- field.toForm.toList
          node <- bind("crud", html,
            "name" -> build(field),
            "error" -> error(field),
            "form" -> form)
        } yield node

      def doDeleteSubmit() = {
        S.notice(S ? ("offer_deleted", item.title.is))
        item.delete_!
        S.redirectTo("/offers/myoffers")
      }

      def doSubmit() {
        item.validate match {
          case Nil =>
            S.notice(noticeMsg)
            item.save

            val ref = if (item.dealType.is == DealType.Offer) "myoffers" else "myorders"
            S.redirectTo("/offers/" + ref)

          case xs =>
            S.error(xs)
            S.currentSnippet.foreach(S.mapSnippet(_, loop))
        }
      }

      bind("crud", html,
        "field" -> doFields _,
        "submit" -> ((text: NodeSeq) => button(text, doSubmit _, "btn btn-primary")),
        "delete" -> ((text: NodeSeq) => button(text, doDeleteSubmit _, "btn")))
    }

    loop(in)
  }

  private def button(text: NodeSeq, func: () => Any, cssClass: String) = SHtml.button(text.text, func, SHtml.BasicElemAttr("class", cssClass))

  def create(html : NodeSeq): NodeSeq = {
    crudDoForm(Offer.create, S ? "offer_created")(nodes(opType = false))
  }

  def imageNodes(images: MongoListField[Offer, String]): NodeSeq = {
    images.is.flatMap(o =>
      <a id={"image_" + o} href={"/download/" + o} rel="gallery">
        <img src={"/serving/" + o + "_small"} height='100' width='100' />
      </a>)
  }

  def edit(html : NodeSeq): NodeSeq = {
    var id = S.param("id") openOr ""
    val offer: Offer = Offer.find(id).openTheBox
    val form = crudDoForm(offer, S ? ("offer_updated", offer.title.is))(nodes(opType = true))
    bind(Map("offerImages" -> imageNodes(offer.images)), form)
  }

  val imageButton: NodeSeq = <div class="btn btn-danger" onclick="$('input[id=lefile]').click();"><i class=" icon-picture"></i> Добавить изображение</div>

  private def nodes(opType: Boolean) = {
    val buttonText = if (opType) "Сохранить" else "Создать"

    <div>
      <div class="form-horizontal span5">
        <table>
          <crud:field>
            <div class="control-group">
                <crud:name/>
                <div class="controls">
                  <crud:form/>
                  <span class="notices-container">
                    <ul>
                      <crud:error/>
                    </ul>
                  </span>
                </div>
            </div>
          </crud:field>
        </table>

        <div class="form-actions span9">
          <crud:submit>{buttonText}</crud:submit>
          {if (opType) <crud:delete>Удалить</crud:delete> else NodeSeq.Empty}
          {imageButton}
        </div>
      </div>

      <div class="span4">
        <label>Загруженные изображения:</label>
        <div id="gallery" data-toggle="modal-gallery" data-target="#modal-gallery">
          {if (opType) <lift:bind name="offerImages"/> else NodeSeq.Empty}
        </div>
      </div>
    </div>
  }
}
