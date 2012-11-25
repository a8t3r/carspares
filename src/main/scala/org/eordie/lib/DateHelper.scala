package org.eordie.lib

import java.util.{Calendar, Locale, Date}
import org.joda.time.{DateMidnight, Days, DateTime}
import net.liftweb.http.S
import org.joda.time.format.DateTimeFormat

/**
 * Помощник для работы с датами
 *
 * @author Alexandr Kolosov
 * @since 10/20/12
 */
object DateHelper {

  def yesterdayOrToday(date: Date): String = {

    def toTime(day: String, date: Date) = S ? (day, new DateTime(date).toString("kk:mm"))

    val dateTime = new DateTime(date).toLocalDate
    val between: Int = Days.daysBetween(dateTime.toDateMidnight, new DateMidnight()).getDays

    between match {
      case 0 => toTime("today", date)
      case 1 => toTime("yesterday", date)
      case _ => dateTime.toString("dd.MM.yyyy")
    }
  }
}
