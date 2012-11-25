package org.eordie
package config

import model.user.User

import net.liftweb._
import common.Full
import http.js.JsCmds.RedirectTo
import http.{RedirectResponse, S}
import sitemap._
import sitemap.Loc._

import net.liftmodules.mongoauth.Locs
import model.car.{OfferType, Offer}

object MenuGroups {
  val SettingsGroup = LocGroup("settings")
  val TopBarGroup = LocGroup("topbar")
  val DetailsGroup = LocGroup("details")
}

/*
 * Wrapper for Menu locations
 */
case class MenuLoc(menu: Menu) {
  lazy val url: String = S.contextPath+menu.loc.calcDefaultHref
  lazy val fullUrl: String = S.hostAndPath+menu.loc.calcDefaultHref
}

object Site extends Locs {
  import MenuGroups._

  // locations (menu entries)
  val home = MenuLoc(Menu.i("Home") / "index" >>  If(() => !User.isLoggedIn, () => RedirectResponse("offers/open")))

  val loginToken = MenuLoc(buildLoginTokenMenu)
  val logout = MenuLoc(buildLogoutMenu)
  val loginMenu = MenuLoc(Menu.i("Login") / "login" >> RequireNotLoggedIn)

  private val profileParamMenu = Menu.param[User]("User", S ? "Profile",
    User.findByUsername _,
    _.username.is
  ) / "user" >> Loc.CalcValue(() => User.currentUser) >> RequireLoggedIn
  lazy val profileLoc = profileParamMenu.toLoc

  val password = MenuLoc(Menu.i("Password") / "settings" / "password" >> RequireLoggedIn >> SettingsGroup)
  val account = MenuLoc(Menu.i("Account") / "settings" / "account" >> SettingsGroup >> RequireLoggedIn)
  val editProfile = MenuLoc(Menu("EditProfile", S ? "Profile") / "settings" / "profile" >> SettingsGroup >> RequireLoggedIn)
  val register = MenuLoc(Menu.i("Register") / "register" >> RequireNotLoggedIn)

  def detailsTemplate(qType: OfferType.Value) = Template({ () =>
    <div lift="surround?with=detail-wrap;at=content">
      <div class={"lift:DemoTable.table?type=" + qType.toString}></div>
    </div>
    })

  val myOffers = Menu.i("MyOffers") / "offers" / "myoffers" >> detailsTemplate(OfferType.myOffers) >> RequireLoggedIn >> DetailsGroup
  val myOrders = Menu.i("MyOrders") / "offers" / "myorders" >> detailsTemplate(OfferType.myOrders) >> RequireLoggedIn >> DetailsGroup

  val openOffers = Menu.i("AllOffers") / "offers" / "open" >> detailsTemplate(OfferType.open) >> RequireLoggedIn >> DetailsGroup
  val completedOffers = Menu.i("CompletedOffers") / "offers" / "completed" >> detailsTemplate(OfferType.completed) >> RequireLoggedIn >> DetailsGroup

  private def menus = List(
    home.menu,
    loginMenu.menu,
    register.menu,
    loginToken.menu,
    logout.menu,
    profileParamMenu,
    account.menu,
    password.menu,
    editProfile.menu,

    openOffers,
    myOffers,
    myOrders,
    completedOffers,

    Menu.i("offer_viewing") / "offers" / "view",
    Menu.i("offer_edition") / "offers" / "edit" >> RequireLoggedIn,
    Menu.i("offer_creation") / "offers" / "create" >> RequireLoggedIn,

    Menu.i("About") / "about" >> TopBarGroup,
    Menu.i("Contact") / "contact" >> TopBarGroup,
    Menu.i("Throw") / "throw" >> Hidden,
    Menu.i("Error") / "error" >> Hidden,
    Menu.i("NotFound") / "404" >> Hidden
  ) ::: Offer.menus ::: User.menus

  /*
   * Return a SiteMap needed for Lift
   */
  def siteMap: SiteMap = SiteMap(menus:_*)
}
