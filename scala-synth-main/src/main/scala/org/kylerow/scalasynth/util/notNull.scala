package org.kylerow.scalasynth.util

object notNull {
	def apply[T <:AnyRef](obj :T) :T= if(obj!=null) obj else None.asInstanceOf[T]
}