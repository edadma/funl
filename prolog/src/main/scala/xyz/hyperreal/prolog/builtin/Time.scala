package xyz.hyperreal.prolog.builtin

import java.time.format.DateTimeFormatter
import java.time.temporal.TemporalAccessor
import java.time.{Instant, ZonedDateTime}

import xyz.hyperreal.char_reader.CharReader
import xyz.hyperreal.prolog.{VM, domainError, typeError}

object Time {

  def timestamp(vm: VM, pos: IndexedSeq[CharReader], now: Any): Boolean = vm.unify(now, ZonedDateTime.now)

  def instant(vm: VM, pos: IndexedSeq[CharReader], now: Any): Boolean = vm.unify(now, Instant.now)

  def timeFormatter(vm: VM, pos: IndexedSeq[CharReader], format: Any, formatter: Any): Boolean =
    format match {
      case s: String =>
        try {
          val f = DateTimeFormatter.ofPattern(s)

          vm.unify(f, formatter)
        } catch {
          case e: IllegalArgumentException => domainError(pos(0), e.getMessage, "datetime_format", format, "timeFormatter", 1)
        }
      case _ => typeError(pos(0), "expected Scala date time format string", "string", format, "timeFormatter", 1)
    }

  def formatTime(vm: VM, pos: IndexedSeq[CharReader], time: Any, formatter: Any, formatted: Any): Boolean =
    (time, formatter) match {
      case (t: TemporalAccessor, f: DateTimeFormatter) => vm.unify(f format t, formatted)
      case _                                           => typeError(pos(0), "expected Java time and date time formatter", "string", formatter, "formatTime", 2)
    }

}
