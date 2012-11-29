package org.eordie.model.car

import net.liftweb.mongodb.record.{MongoMetaRecord, MongoRecord}
import net.liftweb.record.field._
import net.liftweb.mongodb.record.field._
import org.eordie.lib.{UploadManager, MongoCRUDify}
import org.eordie.model.{NumericView, MoneyField, Localization}
import org.eordie.model.user.{SystemUser, User}
import net.liftweb.record.{LifecycleCallbacks, Field}
import net.liftweb.http.S
import scala._
import net.liftweb.common.{Loggable, Empty, Box}
import java.util.{Calendar, Date}
import xml.{Elem, Text}
import java.text.SimpleDateFormat
import net.liftweb.util.FieldError
import net.liftweb.http.S._
import net.liftweb.common.Full
import net.liftweb.util.Helpers._
import org.joda.time.format.DateTimeFormat

/**
 *
 * @author Alexandr Kolosov
 * @since 10/6/12
 */
class Offer extends MongoRecord[Offer] with ObjectIdPk[Offer] with Loggable {
  def meta = Offer

  lazy val df = DateTimeFormat.forPattern("dd.MM.yyyy HH:mm")
  def now: Date = Calendar.getInstance().getTime

  // Тип предложения
  object dealType extends EnumField(this, DealType) with Localization {
    override def buildDisplayList = enum.values.map(a => (Full(a), S ? a.toString)).toList

    override def asHtml = {
      Text(S ? get.toString)
    }
  }

  // Название детали
  object title extends StringField(this, 255) with Localization {

    override def required_? = true

    override def validations = valMinLen(3, "Слишком короткое название") _ :: super.validations

    override def asHtml = <a href={viewHref}>{get}</a>

    override def toForm = Full(S.fmapFunc(SFuncHolder(this.setFromAny(_))) {
      funcName => <input type="text" maxlength={maxLength.toString}
                    id={uniqueFieldId.openTheBox}
                    name={funcName}
                    value={valueBox openOr ""}
                    placeholder="Обязательное поле"
                    tabindex={tabIndex.toString}/>
    })
  }

  def viewHref: String = S.hostAndPath + "/offers/view/" + id + "#disqus_thread"

  // Описание детали
  object description extends TextareaField(this, 512) with Localization

  // Колличество деталей
  object amount extends IntField(this) with Localization with NumericView[Int] {

    private def positive(value: Int): List[FieldError] = {
      if (value < 0) List(FieldError(this, "Значение должно быть больше нуля"))
      else Nil
    }

    override def validations = List((f: ValueType) => positive(f))
  }

  // Действительная цена продажи
  object realPrice extends MoneyField(this) with Localization

  // Создатель по-умолчанию - либо текущий пользователь, либо системный пользователь
  object creator extends ObjectIdRefField(this, User) with Localization {
    override def defaultValue = User.currentUser.openOr(SystemUser.user).id.is

    override def asHtml = {
      val userBox: Box[User] = User.find(get)
      val userName = userBox.get.username.value
      val href = S.hostAndPath + "/user/" + userName
      <a href={href}>{userName}</a>
    }
  }

  // Покупатель
  object purchaser extends ObjectIdRefField(this, User) with Localization {

    override def optional_? = true

    override def asHtml = {
      val userBox: Box[User] = User.find(get)
      if (userBox.isEmpty) {
        <b>{S ? "no_purchaser"}</b>
      } else {
        val userName = userBox.get.username.value
        val href = "/user/" + userName
        <a href={href}>{userName}</a>
      }
    }
  }

  // Текущий  статус сделки
  object status extends  EnumField(this, OfferStatus) with Localization {
    override def buildDisplayList = enum.values.map(a => (Full(a), S ? a.toString)).toList

    override def asHtml = {
      Text(S ? get.toString)
    }
  }

  // Дата создания
  object createdAt extends DateField(this) with Localization with LifecycleCallbacks {
    override def asHtml = {
      Text(df.print(get.getTime))
    }

    override def beforeCreate {
      super.beforeCreate
      this.set(now)
    }
  }

  // Дата обновления
  object updatedAt extends DateField(this) with Localization with LifecycleCallbacks {
    override def asHtml = {
      Text(df.print(get.getTime))
    }

    override def beforeSave {
      super.beforeSave
      this.set(now)
    }
  }

  object images extends MongoListField[Offer, String](this) with Localization with LifecycleCallbacks {
    override def defaultValue = List()

    override def afterDelete {
      UploadManager.clear(is)
    }

    override def shouldDisplay_? = false

    private val func = LFuncHolder(list => set(list.filter(_ != "empty")))

    override def toForm = {
      val fileIds: List[String] = is

      fmapFunc(func)(funcName => Full(
        <div>
          <select multiple="true" id="fileIds" name={funcName} style="display:none">
            <option value="empty" selected="true"/>
            {fileIds.flatMap(o => (<option value={o} selected="true" id={"hidden_" + o} />))}
          </select>
        </div>))
    }
  }

  def performRespond() {
    val subject: String = S ? ("offer_respond_mail_subject", title.is)
    val creatorUser = User.find(creator.is).openTheBox
    val purchaserUser = User.find(purchaser.is).openTheBox

    creatorUser.mail(subject,
      <p>
        <p>На ваше объявление {title.asHtml} откликнулся пользователь {purchaser.asHtml}.</p>
        <p>{footer(creatorUser, purchaserUser)}</p>
      </p>)

    purchaserUser.mail(subject,
      <p>
        <p>Вы откликнулись на объявление {title.asHtml}, созданное пользователем {creator.asHtml}.</p>
        <p>{footer(purchaserUser, creatorUser)}</p>
      </p>
    )
  }

  private def footer(a: User, b: User): String = {
    if (SystemUser.isSystem(a)) "Свяжитесь с данным пользователем по телефону " + b.mobilePhone
    else "Ожидайте звонка от Веселова Павла"
  }

  def isCompleted = OfferStatus.completed == status.is

  def isCreator(user: User) = User.find(creator.is)
    .map(_.userIdAsString == user.userIdAsString)
    .openOr(false)

  def isEditable: Boolean = {
    User.find(creator.is)
      .map(user => User.currentUser.map(c => (c.userIdAsString == user.userIdAsString || c.userIdAsString == SystemUser.user.userIdAsString)).openOr(false))
      .openOr(false)
  }

  def hasPurchaser: Boolean = {
    status.is == OfferStatus.open && User.currentUser.map(c => SystemUser.isSystem(c)).openOr(false) && User.find(purchaser.is).map(c => true).openOr(false)
  }
}

object OfferStatus extends Enumeration {
  type OfferStatus = Value
  val open, closed, completed = Value
}

object DealType extends Enumeration {
  type DealType = Value
  val Offer, Order = Value
}

object Offer extends Offer with MongoMetaRecord[Offer] with MongoCRUDify[Offer] {

  override def createMenuLoc = Empty

  override def fieldsForEditing = List(dealType, title, description, amount, realPrice, creator, purchaser, images)

  override def fieldsForList = List(Offer.dealType, Offer.title, Offer.amount, Offer.realPrice, Offer.creator, Offer.purchaser, Offer.status)

  override def fieldsForDisplay = fieldsForEditing ::: List[Field[_, Offer]](createdAt, updatedAt)
}