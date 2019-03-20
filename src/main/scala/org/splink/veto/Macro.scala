package org.splink.veto

import scala.language.experimental.macros
import scala.reflect.macros._

object Macro {

  implicit def body(f: Any): (String, String) = macro bodyImpl

  def bodyImpl(c: blackbox.Context)(f: c.Tree) = {
    import c.universe._

    val code = showCode(f, true, true)
    val tree = showRaw(f)


    q"""($code -> $tree)"""
  }
}
