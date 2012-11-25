package org.eordie.model

import net.liftweb.http.S
import net.liftweb.util.ReadableField

/**
 *
 * @author Alexandr Kolosov
 * @since 10/6/12
 */
trait Localization extends ReadableField {

  abstract override def displayName = S ? name

}
