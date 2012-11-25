package org.eordie
package snippet

import config.Site
import lib.{Gravatar, AppHelpers}
import model.user.{User, LoginCredentials}

import scala.xml._

import net.liftweb._
import common._
import http.{S, SHtml}
import http.js.JsCmd
import http.js.JsCmds._
import util._
import Helpers._

import net.liftmodules.mongoauth.{MongoAuth, LoginRedirect}
import net.liftmodules.mongoauth.model.ExtSession

sealed trait UserSnippet extends AppHelpers with Loggable {

  protected def user: Box[User]

  protected def serve(snip: User => NodeSeq): NodeSeq =
    (for {
      u <- user ?~ "User not found"
    } yield {
      snip(u)
    }): NodeSeq

  protected def serve(html: NodeSeq)(snip: User => CssSel): NodeSeq =
    (for {
      u <- user ?~ "User not found"
    } yield {
      snip(u)(html)
    }): NodeSeq

  def header(xhtml: NodeSeq): NodeSeq = serve { user =>
    <div id="user-header">
      {gravatar(xhtml)}
      <h3>{username(xhtml)}</h3>
    </div>
  }

  def gravatar(xhtml: NodeSeq): NodeSeq = {
    val size = S.attr("size").map(toInt) openOr Gravatar.defaultSize.vend

    serve { user =>
      Gravatar.imgTag(user.email.is, size)
    }
  }

  def username(xhtml: NodeSeq): NodeSeq = serve { user =>
    Text(user.username.is)
  }

  def title(xhtml: NodeSeq): NodeSeq = serve { user =>
    <lift:head>
      <title lift="Menu.title">{"CarSpares: %*% - "+user.username.is}</title>
    </lift:head>
  }
}

object CurrentUser extends UserSnippet {
  protected def user = User.currentUser
}

object ProfileLocUser extends UserSnippet {

  protected def user = Site.profileLoc.currentValue

  import java.text.SimpleDateFormat

  val df = new SimpleDateFormat("dd.MM.yyyy")

  def profile(html: NodeSeq): NodeSeq = serve(html) { user =>
    val editLink: NodeSeq =
      if (User.currentUser.filter(_.id.is == user.id.is).isDefined)
        <a href={Site.editProfile.url} class="btn btn-info"><i class="icon-edit icon-white"></i> { S ?? "edit.profile" } </a>
      else
        NodeSeq.Empty

    "#id_avatar *" #> Gravatar.imgTag(user.email.is) &
    "#id_name *" #> <h3>{user.username.is}</h3> &
    "#id_mobilePhone *" #> formatPhone(user.mobilePhone.is) &
    "#id_email *" #> user.email.is &
    "#id_location *" #> user.location.is &
    "#id_whencreated" #> df.format(user.whenCreated.toDate) &
    "#id_bio *" #> user.bio.is &
    "#id_editlink *" #> editLink
  }

  // phone number pretty print
  private def formatPhone(phone: Long): String = {
    val phoneStr: String = phone.toString
    if (phoneStr.length != 11) phoneStr
    else {
      val builder = StringBuilder.newBuilder
      val prefix: String = if (phoneStr.startsWith("7")) "+ 7 " else "8 "
      builder.append(prefix)
      builder.append("(").append(phoneStr.substring(1, 4)).append(") ")
      builder.append(phoneStr.substring(4, 7)).append(" ")
      builder.append(phoneStr.substring(7, 9)).append(" ")
      builder.append(phoneStr.substring(9, 11)).append(" ")

      builder.toString()
    }
  }
}

object UserLogin extends Loggable {

  def render = {
    // form vars
    var password = ""
    var hasPassword = false
    var remember = User.loginCredentials.is.isRememberMe

    val radios = SHtml.radioElem[Boolean](
      Seq(false, true),
      Full(hasPassword)
    )(it => it.foreach(hasPassword = _))

    def doSubmit(): JsCmd = {
      S.param("email").map(e => {
        val email = e.toLowerCase.trim
        // save the email and remember entered in the session var
        User.loginCredentials(LoginCredentials(email, remember))

        if (hasPassword && email.length > 0 && password.length > 0) {
          User.findByEmail(email) match {
            case Full(user) if (user.password.isMatch(password)) =>
              logger.debug("pwd matched")
              logUserIn(user, remember)
              RedirectTo(LoginRedirect.openOr(Site.home.url))

            case _ =>
              S.error(S ? "invalid_credentials")
              Noop
          }
        }
        else if (hasPassword && email.length <= 0 && password.length > 0) {
          S.error("id_email_err", S ? "id_email_err")
          Noop
        }
        else if (hasPassword && password.length <= 0 && email.length > 0) {
          S.error("id_password_err", S ? "id_password_err")
          Noop
        }
        else if (hasPassword) {
          S.error("id_email_err", S ? "id_email_err")
          S.error("id_password_err", S ? "id_password_err")
          Noop
        }
        else if (email.length > 0) {
          // see if email exists in the database
          User.findByEmail(email) match {
            case Full(user) =>
              User.sendLoginToken(user)
              logger.debug("confirm_email_sended: " + user.email)
              User.loginCredentials.remove()
              S.notice(S ? "confirm_email_sended")
              Noop
            case _ =>
              RedirectTo(Site.register.url)
          }
        }
        else {
          S.error("id_email_err", S ? "id_email_err")
          Noop
        }
      }) openOr {
        S.error("id_email_err", S ? "id_email_err")
        Noop
      }
    }

    def cancel() = S.seeOther(Site.home.url); Noop

    "#id_email [value]" #> User.loginCredentials.is.email &
    "#id_password" #> SHtml.password(password, password = _) &
    "#no_password" #> radios(0) &
    "#yes_password" #> radios(1) &
    "name=remember" #> SHtml.checkbox(remember, remember = _) &
    "#id_submit" #> SHtml.hidden(doSubmit)
  }

  def logUserIn(user: User, remember: Boolean) = {
    User.logUserIn(user, true)
    if (remember) User.createExtSession(user.id.is)
    else ExtSession.deleteExtCookie()
  }
}

object UserTopbar {
  def render = {
    User.currentUser match {
      case Full(user) =>
        <ul class="nav pull-right" id="user">
          <li class="dropdown" data-dropdown="dropdown">
            <a href="#" class="dropdown-toggle" data-toggle="dropdown">
              {Gravatar.imgTag(user.email.is, 20)}
              <span>{user.username.is}</span>
              <b class="caret"></b>
            </a>
            <ul class="dropdown-menu">
              <li><a href={Site.profileLoc.calcHref(user)}><i class="icon-user"></i> { S ? "Profile" } </a></li>
              <li><lift:Menu.item name="Account" donthide="true" linktoself="true"><i class="icon-cog"></i> { S ? "Settings" } </lift:Menu.item></li>
              <li class="divider"></li>
              <li><lift:Menu.item name="Logout" donthide="true"><i class="icon-off"></i> { S ?? "log.out" } </lift:Menu.item></li>
            </ul>
          </li>
        </ul>
      case _ if (S.request.flatMap(_.location).map(_.name).filterNot(it => List("Login", "Register").contains(it)).isDefined) =>
        <ul class="nav pull-right">
          <li><a href="/login">Войти</a></li>
        </ul>
      case _ => NodeSeq.Empty
    }
  }
}
