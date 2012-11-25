package org.eordie.model

import net.liftweb.record.field.DecimalField
import net.liftweb.record.Record
import org.apache.commons.validator.routines.BigDecimalValidator
import net.liftweb.common.Box
import net.liftweb.util.FieldError
import java.util.Locale

/**
 *
 * @author Alexandr Kolosov
 * @since 11/11/12
 */
class MoneyField[OwnerType <: Record[OwnerType]](rec: OwnerType) extends DecimalField(rec, 0) with NumericView[BigDecimal] {

  private lazy val validator = BigDecimalValidator.getInstance()
  private val locale = new Locale("en")

  override def setFromString(s: String) = {
    setBox(Box.legacyNullTest(validator.validate(s, locale)))
  }

  private def positive(f: ValueType): List[FieldError] = {
    if (f.bigDecimal == null || f < 0) List(FieldError(this, "Некорректное значение"))
    else Nil
  }

  def viewString: String = (this.is * 1000).toBigInt() + " рублей"

  override def validations = List((f: ValueType) => positive(f))

  override def view(funcName: String) =
    <div class="input-prepend input-append">
      <input id="appendedPrependedInput" type="number" step="any" name={funcName} value={valueBox.map(_.toString()) openOr ""} tabindex={tabIndex.toString} class="input-small span2"/>
      <span class="add-on">тыс. рублей</span>
    </div>
}
