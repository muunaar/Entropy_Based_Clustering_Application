package util

object Exceptions {

  /**
    * Exception for handling similarity threshold!
    * Thrown when the value inserted does not fit in the range.
    */
  case object ThresholdNotInRange extends Exception {
    //you could override the getMessage method instead of toString
    override def toString: String =
      "Similarity threshold Not in range : It should be < 0.9 and > 0.5"
  }

  /**
    * Exception for handling data file formatting
    * Thrown when the File is not correctly formatted according to (R  A,A,A)
    * Where R is the resource performing the task and A the related tasks
    */
  case object FileNotCorrectlyFormated extends Exception {
    //you could override the getMessage method instead of toString
    override def toString: String =
      "Error Parsing : File formatting is not respected "
  }

}
