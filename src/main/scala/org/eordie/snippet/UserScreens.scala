package org.eordie
package snippet

import config.Site
import lib.Gravatar
import model.user._

import scala.xml._

import net.liftweb._
import http.{LiftScreen, S}
import util.FieldError
import util.Helpers._

/*
 * Use for editing the currently logged in user only.
 */
sealed trait BaseCurrentUserScreen extends BaseScreen {
  object userVar extends ScreenVar(User.currentUser.open_!)

  override def localSetup {
    Referer(Site.account.url)
  }
}

object AccountScreen extends BaseCurrentUserScreen {
  addFields(() => userVar.is.accountScreenFields)

  def finish() {
    userVar.is.save
    S.notice("Настройки сохранены")
  }
}

sealed trait BasePasswordScreen {
  this: LiftScreen =>

  def pwdName: String = "Пароль"
  def pwdMinLength: Int = 6
  def pwdMaxLength: Int = 32

  val passwordField = password(pwdName, "", trim,
    valMinLen(pwdMinLength, "Длина пароля должна превышать " + pwdMinLength + " символов"),
    valMaxLen(pwdMaxLength, "Длина пароля должна быть меньше " + pwdMaxLength + " символом"),
    ("tabindex" -> "1")
  )
  val confirmPasswordField = password("Подтверждение пароля", "", trim, ("tabindex" -> "1"))

  def passwordsMustMatch(): Errors = {
    if (passwordField.is != confirmPasswordField.is)
      List(FieldError(confirmPasswordField, "Пароли должны совпадать"))
    else Nil
  }
}


object PasswordScreen extends BaseCurrentUserScreen with BasePasswordScreen {
  override def pwdName = "Новый пароль"
  override def validations = passwordsMustMatch _ :: super.validations

  def finish() {
    userVar.is.password(passwordField.is)
    userVar.is.password.hashIt
    userVar.is.save
    S.notice("Пароль изменен")
  }
}

/*
 * Use for editing the currently logged in user only.
 */
object ProfileScreen extends BaseCurrentUserScreen {
  def gravatarHtml = {
    val userEmail = userVar.is.email.is

    <span>
      <div class="gravatar">
        {Gravatar.imgTag(userEmail, 60)}
      </div>
      <div class="gravatar">
        <h4>Измените свою фотографию на сайте <a href= { Gravatar.signupUrl(userEmail) } target="_blank">Gravatar.com</a></h4>
        <p>
          Для получения фотографии используется почта {userEmail}.
        </p>
      </div>
    </span>
  }

  val gravatar = displayOnly(S ? "Picture", gravatarHtml)

  addFields(() => userVar.is.profileScreenFields)

  def finish() {
    userVar.is.save
    S.notice("Настройки профиля сохранены")
  }
}

// this is needed to keep these fields and the password fields in the proper order
trait BaseRegisterScreen extends BaseScreen {
  object userVar extends ScreenVar(User.regUser.is)

  addFields(() => userVar.is.registerScreenFields)
}

/*
 * Use for creating a new user.
 */
object RegisterScreen extends BaseRegisterScreen with BasePasswordScreen {
  override def validations = passwordsMustMatch _ :: super.validations

  val rememberMe = builder("", User.loginCredentials.is.isRememberMe, ("tabindex" -> "1"))
    .help(Text("Запомнить меня"))
    .make

  override def localSetup {
    Referer(Site.home.url)
  }

  def finish() {
    val user = userVar.is
    user.password(passwordField.is)
    user.password.hashIt
    user.save
    User.logUserIn(user, true)
    if (rememberMe) User.createExtSession(user.id.is)
    S.notice("Спасибо за вход на сайт!")
  }
}

