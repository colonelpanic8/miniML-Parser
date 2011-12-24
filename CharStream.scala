/*
 * Math 384: Programming Language Design and Implementation
 * 
 * http://www.reed.edu/~jimfix/384
 * afs:/reed.edu/courses/math/math384
 * 
 */
package math384.util

/** This case class describes location of a particular character in a CharStream.
 *  It provides convenience routines for associating program tokens with locations
 *  in the program source code, and reporting errors in parsing the source, or
 *  during its execution and/or evaluation.
 * 
 * @author Jim Fix
 * @version Fall2010.1
 */ 
case class CharStreamPosition(line:Int, column:Int) {
  def this(position:(Int,Int)) { this(position._1,position._2) }  

  /** Converts a position intto text useful for reporting an error.
   * 
   * @param      none
   * @return     a String saying "line <code>line</code>, column <code>column</code>"
   */
  override def toString:String = "line "+line+", column "+column

  /** Constructs a <code>CharStreamRegion</code> representing the range of
   *  source code position from the receiver up to and including the provided
   *  second position.
   * 
   *  @param      that - another <code>CharSreamPosition</code> that ends the region
   *  @return     a String saying "line <code>line</code>, column <code>column</code>"
   */
  def to(that:CharStreamPosition):CharStreamRegion = new CharStreamRegion(this,that)
}

/** This case class describes a range of characters from a CharStream.
 *  It provides convenience routines for associating program tokens with portions
 *  of the program source code.
 *
 * @author Jim Fix
 * @version Fall2010.1
 */ 
case class CharStreamRegion(start:CharStreamPosition, end:CharStreamPosition) {
  override def toString:String = "from "+start+" to "+end
} 

/** This class provides a simple representation for a stream of characters,
 *  typically either program source from a file, or lines entered into
 *  an interactive shell.
 *
 * @author Jim Fix
 * @version Fall2010.1
 */ 

abstract class CharStream {
  var line:Int = 1
  var column:Int = 1
  var lastpos:(Int,Int) = (0,0)
  def lastPosition(u:Unit):CharStreamPosition = new CharStreamPosition(lastpos)
  def position(u:Unit):CharStreamPosition = new CharStreamPosition(line,column)
  def advance(u:Unit):Unit
  def current(u:Unit):Char
  def advanceToNonWhiteSpace:Unit = while (isAtWhiteSpace) advance()
  def isAtWhiteSpace:Boolean = " \t\n" contains (current())
  def isAtEOF:Boolean = current() == '\000'
  def isAtAlpha:Boolean = (('a' to 'z') contains (current())) || (('A' to 'Z') contains (current)())
  def isAtDigit:Boolean = ('0' to '9') contains (current())
  def isAtAlphaNumeric:Boolean = isAtDigit || isAtAlpha || current() == '_' 
  def isAtOpChar:Boolean = ".~!@#%^&|+-*/%:;<>=" contains (current())
}

class CharStreamFull(source:String) extends CharStream {
  private var text = (source+"\000") toList
  def current(u:Unit):Char = text.head
  def advance(u:Unit):Unit = {

    lastpos = (line,column)

    if (current () == '\n') {
      this line_= (line + 1)
      column_= (1)
    } else if (current () == '\t') {
      column_= (column + 4)
    } else {
      column_= (column + 1)
    }     

    text = text.tail
  }  
}
