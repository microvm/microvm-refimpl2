package uvm.refimpl.nat

/**
 * Constants specific to the platform. Currently only supports x86_64.
 */
object PlatformConstants {
  type Word = Long

  val WORD_SIZE_LOG: Word = 6L
  val WORD_SIZE_BITS: Word = 1L << WORD_SIZE_LOG
  val WORD_SIZE_BYTES: Word = 1L << (WORD_SIZE_LOG - 3L)
}