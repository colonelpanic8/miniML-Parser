package math384.util

import scala.collection.immutable.Map 
import scala.collection.immutable.HashMap 

class TokenStream(stream:CharStream) {
  abstract class Token 
  case object EofToken extends Token
  case class IntToken(value:Int) extends Token
  case class IdToken(identifier:String) extends Token {
    def this() = { this("") }
  }
  case object LParenToken extends Token
  case object RParenToken extends Token
  case object LBraceToken extends Token
  case object RBraceToken extends Token
  case object LBrackToken extends Token
  case object RBrackToken extends Token
  case object DQuoteToken extends Token
  abstract class OpToken extends Token
  
  val opSet = HashMap[String,Token]()
  val keySet = HashMap[String,Token]()
  val specialSet = HashMap[Char,Token]('('->LParenToken,
				       ')'->RParenToken,
				       '{'->LBraceToken,
				       '}'->RBraceToken,
				       '['->LBrackToken,
				       ']'->RBrackToken,
				       '"'->DQuoteToken)

  private var token:Token = null
  private var start:CharStreamPosition = null
  private var end:CharStreamPosition = null
  private def initIfNecessary:Unit = if (token == null) advance

  def init = initIfNecessary
  def currentTokenStart:CharStreamPosition = { initIfNecessary; start }
  def currentTokenEnd:CharStreamPosition = { initIfNecessary; end }
  def currentTokenRegion:CharStreamRegion = { initIfNecessary; start to end }
  def currentToken:Token = { initIfNecessary; token }

  def advance:Unit = {
    stream advanceToNonWhiteSpace;
    start = stream position ();
    token = produceNextToken;
    end = stream lastPosition ()
  }
  def eatIdToken:String = {
    token match {
      case IdToken(x) => advance; x
      case _ => error("Failed to match a token "+(currentTokenStart toString)+".")
    }
  }
  def eat(t:Token):Unit = {
    if (t.hashCode == currentToken.hashCode) {
      advance
    } else {
      error("Failed to match a token "+(currentTokenStart toString)+".  Found "+token+".  Expected "+t+".")
    }
  }
  private def produceNextToken:Token = {   
    if (stream isAtEOF) {
      EofToken
    } else if (stream isAtDigit) {
      produceIntToken
    } else if (stream isAtAlpha) {
      produceWordToken
    } else if (stream isAtOpChar) {
      produceOpToken
    } else if (specialSet contains (stream current ())) {
      val c = stream current ();
      stream advance ();
      specialSet(c)
    } else {
      error("Unexpected token "+(currentTokenStart toString)+".")
    }
  }
  private def produceIntToken:Token = {
    var value = 0
    while (stream isAtDigit) {
      value = (value*10) + ((stream current ())-'0');
      stream advance ()
    }
    IntToken(value)
  }
  private def produceWordToken:Token = {
    var word = ""
    while (stream isAtAlphaNumeric) {
      word = word + (stream current ());
      stream advance ()
    }
    if (keySet contains (word)) {
      keySet(word)
    } else {
      IdToken(word)
    }
  }
  private def produceOpToken:Token = {
    var word = ""
    while (stream isAtOpChar) {
      word = word + (stream current ());
      stream advance ()
    }
    if (opSet contains (word)) {
      opSet(word)
    } else {
      error("Unexpected operator token "+word+" "+(currentTokenStart toString)+".")
    }
  }
}
