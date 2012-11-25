package org.eordie.model
package user

import org.bson.types.ObjectId
import org.joda.time.DateTime

import net.liftweb._
import common._
import common.Full
import http.{StringField => _, BooleanField => _, _}
import mongodb.record.field._
import record.field._
import util.Mailer.PlainMailBodyType
import util.{Props, FieldContainer}

import net.liftmodules.mongoauth._
import net.liftmodules.mongoauth.model._
import xml.NodeSeq
import net.liftweb.util.Mailer._
import org.eordie.lib.MongoCRUDify
import org.eordie.config.MenuGroups

class User private () extends LocalizedProtoAuthUser[User] with ObjectIdPk[User] {
  def meta = User

  def userIdAsString: String = id.toString()

  object location extends StringField(this, 64) with Localization {
    override def validations =
      valMaxLen(64, "Местонахождение должно быть короче 64 символов") _ ::
      super.validations
  }

  object bio extends TextareaField(this, 160) with Localization {
    override def validations =
      valMaxLen(160, "Информация должна быть короче 160 символов") _ ::
      super.validations
  }

  object mobilePhone extends LongField(this) with Localization {

    override def setFromString(s: String) = super.setFromString(s.filter(_.isDigit))

  }

  /*
   * FieldContainers for various LiftScreeens.
   */
  def accountScreenFields = new FieldContainer {
    def allFields = List(username, email)
  }

  def profileScreenFields = new FieldContainer {
    def allFields = List(username, mobilePhone, location, bio)
  }

  def registerScreenFields = new FieldContainer {
    def allFields = List(username, email, mobilePhone)
  }

  def whenCreated: DateTime = new DateTime(id.is.getTime)

  def mail(subject: String, msgTxt: NodeSeq) {
    mail(subject, XHTMLMailBodyType(msgTxt))
  }

  def mail(subject: String, msgTxt: String) {
    mail(subject, PlainMailBodyType(msgTxt))
  }

  def mail(subject: String, body: MailBodyType) {
    sendMail(
      From(MongoAuth.systemFancyEmail, Full(MongoAuth.systemUsername.vend)),
      Subject(subject),
      To(fancyEmail, Full(username.is)),
      body
    )
  }
}

object User extends User with ProtoAuthUserMeta[User] with Loggable with MongoCRUDify[User] {
  import mongodb.BsonDSL._

  override def collectionName = "user.users"

  // ovverided methods for crudify
  override def fieldsForList = List(username, email, mobilePhone)
  override def fieldsForDisplay = fieldsForEditing
  override def fieldsForEditing = List(username, email, mobilePhone, verified, location, bio)

  override def showAllMenuLocParams = List(MenuGroups.TopBarGroup)

  override def showAllMenuName = S ? "registered_users"
  // end of crudify

  val manager = Role.createRecord.id("manager").permissions(List(Permission.all)).save
  val client = Role.createRecord.id("client").permissions(List(Permission("details", "view"))).save

  ensureIndex((email.name -> 1), true)
  ensureIndex((username.name -> 1), true)

  def findByEmail(in: String): Box[User] = find(email.name, in)
  def findByUsername(in: String): Box[User] = find(username.name, in)

  def isCurrentUser(user: Box[User]): Boolean = {
    val userBox: Box[User] = User.currentUser
    user.isDefined && userBox.isDefined && userBox.openTheBox.userIdAsString == user.openTheBox.userIdAsString
  }

  def findByStringId(id: String): Box[User] =
    if (ObjectId.isValid(id)) find(new ObjectId(id))
    else Empty

  override def onLogIn: List[User => Unit] = List(user => User.loginCredentials.remove())
  override def onLogOut: List[Box[User] => Unit] = List(
    x => logger.debug("User.onLogOut called."),
    boxedUser => boxedUser.foreach { u =>
      ExtSession.deleteExtCookie()
    }
  )

  /*
   * MongoAuth vars
   */
  private lazy val siteName = MongoAuth.siteName.vend
  private lazy val sysUsername = MongoAuth.systemUsername.vend
  private lazy val indexUrl = MongoAuth.indexUrl.vend
  private lazy val registerUrl = MongoAuth.registerUrl.vend
  private lazy val loginTokenAfterUrl = MongoAuth.loginTokenAfterUrl.vend

  /*
   * LoginToken
   */
  override def handleLoginToken(): Box[LiftResponse] = {
    val resp = S.param("token").flatMap(LoginToken.findByStringId) match {
      case Full(at) if (at.expires.isExpired) => {
        at.delete_!
        RedirectWithState(indexUrl, RedirectState(() => { S.error("Login token has expired") }))
      }
      case Full(at) => find(at.userId.is).map(user => {
        if (user.validate.length == 0) {
          user.verified(true)
          user.roles(List("client"))
          user.save
          logUserIn(user)
          at.delete_!
          RedirectResponse(loginTokenAfterUrl)
        }
        else {
          at.delete_!
          regUser(user)
          RedirectWithState(registerUrl, RedirectState(() => { S.notice("Please complete the registration form") }))
        }
      }).openOr(RedirectWithState(indexUrl, RedirectState(() => { S.error("User not found") })))
      case _ => RedirectWithState(indexUrl, RedirectState(() => { S.warning("Login token not provided") }))
    }

    Full(resp)
  }

  // send an email to the user with a link for logging in
  def sendLoginToken(user: User) {
    val token = LoginToken.createForUserId(user.id.is)
    val msgTxt = S ? ("email_body", siteName, token.url, sysUsername)

    user.mail(S ? ("confirm_password", siteName), msgTxt)
  }

  /*
   * ExtSession
   */
  def createExtSession(uid: ObjectId) = ExtSession.createExtSession(uid)

  /*
  * Test for active ExtSession.
  */
  def testForExtSession: Box[Req] => Unit = {
    ignoredReq => {
      if (currentUserId.isEmpty) {
        ExtSession.handleExtSession match {
          case Full(es) => find(es.userId.is).foreach { user => logUserIn(user, false) }
          case Failure(msg, _, _) =>
            logger.warn("Error logging user in with ExtSession: %s".format(msg))
          case Empty =>
        }
      }
    }
  }

  // used during login process
  object loginCredentials extends SessionVar[LoginCredentials](LoginCredentials(""))
  object regUser extends SessionVar[User](createRecord.email(loginCredentials.is.email))
}

case class LoginCredentials(email: String, isRememberMe: Boolean = false)

object SystemUser {
  private val username = "Веселов Павел"
  private val email = Props.get("mail.smtp.user")

  lazy val user: User = User.find("username", username) openOr {
    User.createRecord
      .location("Сочи, Кудепста")
      .mobilePhone(79101046338l)
      .username(username)
      .roles(List("manager"))
      .email(email)
      .verified(true)
      .password(Props.get("mail.smtp.pass").openTheBox, isPlain = true)
      .save
  }

  def isSystem(thatUser: User): Boolean = user.userIdAsString == thatUser.userIdAsString
}
