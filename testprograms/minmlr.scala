//
// MATH 384: Programming Language Design & Implementation
// Spring 2010
//
// HW 7: A Parser and Reducing Engine for MinML
//
// This code defines a module
//
//    math384.hw7.minml.normalize
//
// whose "main" method parses and executes source code of
// an extreme subset of the language Standard ML, called
// MinML.  It is essentially Church's lambda caclulus
// augmented with a convenient "let..." construct. 
//
// Briefly, here is the grammar for MinML:
//
// <trm> ::= let <defs> in <trm> end
// <def> ::= val <name> = <trm>
// <trm> ::= fn <name> => <trm>
// <trm> ::= <trm> <trm>
// <trm> ::= <name> 
// <trm> ::= ( <trm> )
// <name> ::= <any identifier that's not fn,let,val,&c>
//    
// Thus it only contains function abstraction and 
// application.
//
// By investigations of Church and others, this language is 
// found to be powerful enough to express computations (it 
// is Turing-complete) including all the recursive functions 
// on the natural numbers.  It has a well-defined semantics
// often defined as rewrite rules, in the form of beta
// reduction-- the rule for replacing all occurrences of
// a formal parameter by an actual parameter (term) within an
// abstraction's body upon its application-- and all
// programs where this rewriting can halt have a determined
// irreducible form.  In addition, this form can be reached
// via normal order reduction, where the leftmost, outermost
// reducible term is acted upon.
//
// Below, we include code for parsing MinML "programs" and
// top-level code, and also code for a normalizing engine.
// When this project is completed, you will be able to
// execute
//
//    scala math384.hw7.minml.normalize test.mml
//
// yielding a series of outputs of terms, each one resulting
// from the prior due to a normal order reduction step, halting
// with the normal form of the program in "test.mml", should it
// have one.
//
// Your assignment, then, is to write three methods, one version 
// for each abstract syntax term (Lam, App, and Var):
//
// 1. output: print the given abstract syntax term.  You are 
//      welcome to use parentheses liberally.  The output should
//      parse back to the term that produced it.  BONUS: try to
//      have it use the fewest parentheses but still parse
//      correctly, using our precedence conventions.
// 2. sub(a,x): replace all free occurrences of the variable
//      x with the term a.  
// 3. reduce: returns a pair (r,t) that reflects possible normal 
//      order reduction applied to the term.  Note that
//    * r should be false if the term was not reducible, in which
//      case t should just be the unchanged term;
//    * if the term is reducible, then r should be true and t 
//      should be the result of applying a normal order reduction
//      to the term.
// One has to take some caution in this code.  First, note that 
// normal order reduction is well-defined-- your reduce methods will 
// want to hunt through a terms subterm structure to find the leftmost
// outermost reducible subterm (which might be itself).  The code
// is simple, but takes some forethought to write.  
//
// Second, the substitution needs to avoid name conflicts when it 
// hits an abstraction (i.e. Lam) term during its traversal of a term's 
// tree.  It should want to rename a function's formal parameter in 
// order to prevent accidental binding of free names in the term that's 
// replacing the variable.  
//
// To enable this activity, I've provided a Vars module that provides
// an endless supply of "fresh" variables by adding version numbers 
// to names of existing variables.  For example, 
//    Vars.fresh("y")
// may yield a string like "y$1234" and
//    Vars.fresh("y$1234")
// may yield a string like "y$1235".  The code guaranteesn that 
// the suffix integer that's being appended will be unique over 
// all prefix names (i.e. there can't be an "x$1234" and also a
// "y$1234" within the same normalize execution).
// 
// Your code can blindly keep requesting fresh variables as
// it sees fit.
// 
// Hand in your hw7 folder (compressed) in the usual place 
// in AFS.  As a last requirement, please include ten test 
// programs in that folder so that I can evaluate your code's 
// correctness.
//

package math384.hw7.minml

import math384.util.CharStreamPosition
import scala.collection.immutable.Map 
import scala.collection.immutable.HashMap 
import math384.util.CharStream
import math384.util.CharStreamFull
import math384.util.TokenStream
import java.io.{File, ByteArrayOutputStream, FileInputStream}

object Vars {
   var freshest = 0;
   def fresh(name:String):String = {
     freshest = freshest + 1;
     val base = if (name.contains("$")) {
                  name.subSequence(0,name.indexOf('$'))
                } else {
                  name
                }
     return base+"$"+freshest;  
   }
}

//
// abstract syntax terms
//
abstract class Term {
   def sub(a:Term,x:String):Term;
   def reduce:(Boolean,Term);
   def output:Unit;
}

case class Lam(formal:String,body:Term) extends Term {
	def sub(a:Term, x:String):Term = {
		val nformal = Vars.fresh(this.formal);
		var term:Term = body.sub(Var(nformal),formal);
		Lam(nformal, term.sub(a, x));
	}
	def output:Unit = {
		var str:String = "Lam(" + formal.output + "," + body.output + ")";
		print(str);
	}
	def reduce:(Boolean, Term) = {
		body.reduce;
	}
	
}

case class App(fn:Term,arg:Term) extends Term {
	def output:Unit = {
		print("(");
		fn.output;
		print(", ");
		
		arg.output;
		print(")");
	}
	
	def sub(a:Term, x:String):Term = {
		val fns:Term = this.fn.sub(a,x);
		val args = this.arg.sub(a,x);
		return new App(fns,args);
	}
	
	def reduce:(Boolean,Term) = {
		val (lBool, lTerm) = this.fn.reduce;
		if(lBool) (true, App(lTerm,this.arg));
		else {
			val (rBool, rTerm) = this.arg.reduce;
			if(rBool) (true, App(this.fn, rTerm));
			else{
				val bRed:Term = fn match{
					case Lam(f,b) => b.sub(this.arg, f)
					case Var(string) => this.arg;
					case _ => error("abe");
				}
				
				if(bRed!=this) return (true,bRed);
				else return (false,this);
			}
		}
	}
}

case class Var(name:String) extends Term {
	def sub(a:Term, x:String):Term = {
		if (this.name==x){
			return a;
		}
		else return this;
	}
	def output:Unit = {
		print(name);
	}
	def reduce:(Boolean,Term) = {
		return(false,this);
	}
}

class Parser(src:CharStream) extends TokenStream(src) {
  case object LetToken extends Token
  case object ValToken extends Token
  case object InToken extends Token
  case object EndToken extends Token
  case object FnToken extends Token
  case object EqualToken extends OpToken
  case object ToToken extends OpToken
  override val opSet = HashMap[String,Token]("="->EqualToken,
					     "=>"->ToToken);
  override val keySet = HashMap[String,Token]("let"->LetToken,
					      "val"->ValToken,
					      "in"->InToken,
					      "end"->EndToken,
					      "fn"->FnToken);
  def parse:Term = parseTerm
  def parseTerm:Term = {
    currentToken match {
      case FnToken => {
	return parseFn
      }
      case _ => parseApp
    }
  }
  def parseFn:Term = {
    eat(FnToken);
    val x = currentToken match {
      case IdToken(n) => { advance; n }
      case _ => error("Expected formal parameter name.");
    }
    eat(ToToken);
    val b = parseTerm;
    return Lam(x,b);
  }
  def parseApp:Term = {
    currentToken match {
      case _ => {
	var e:Term = parseAtom;
	while (!(currentToken == RParenToken || currentToken == EofToken 
	       || currentToken == EndToken || currentToken == ValToken || currentToken == InToken
	       || currentToken == EqualToken)) {
	  e = App(e,parseAtom);
	}
	return e;
      }
    }
  }
  def parseAtom:Term = {
    currentToken match {
      case IdToken(x) => { advance; return Var(x); }
      case LParenToken => { 
	eat(LParenToken);
	val e = parseTerm;
	eat(RParenToken);
	return e;
      }
      case LetToken => parseLet
      case _ => error("Unexpected token: "+currentToken);
    }
  }
  def parseLet:Term = {
    eat(LetToken);
    eat(ValToken);
    val x = currentToken match {
	      case IdToken(n) => { advance; n }
	      case _ => error("Expected an identifier after LET VAL.");
	    }
    eat(EqualToken);
    val d = parseTerm;
    App(Lam(x,parseLetNest),d)
  }
  def parseLetNest:Term = {
    if (currentToken == InToken) {
      eat(InToken);    
      val b = parseTerm;
      eat(EndToken);
      return b;
    } else {
      eat(ValToken);
      val x = currentToken match {
	      case IdToken(n) => { advance; n }
	      case _ => error("Expected an identifier after VAL.");
	    }
      eat(EqualToken);
      val d = parseTerm;
      App(Lam(x,parseLetNest),d)
    }
  }
}

object normalize extends Application {
  //
  // from http://blog.lostlake.org/index.php?/archives/61-A-spell-checker-in-Scala.html
  //
  def readFileToString(f: File):String = {
    val bos = new ByteArrayOutputStream;
    val ba = new Array[Byte](2048);
    val is = new FileInputStream(f);
    def read {
      is.read(ba) match {
	case n if n < 0 =>
	  case 0 => read
	case n => bos.write(ba, 0, n); read
      }
    }
    read
    bos.toString("UTF-8")
  }

  //                                                                                                                                                       
  // Reads MinML source code from a file, or from standard input
  //
  // Example command line:
  //
  //    scala math384.hw7.minml.normalize test.mml
  //
  override def main(args:Array[String]):Unit = {
    val src:String =
      if (args.length > 0) {
	println("MinML: Processing "+args(0)+"...");
	readFileToString(new File(args(0)));
      } else {
	println("MinML: Processing standard input.  Enter your program then hit ctrl-d.");
	(scala.io.Source.fromInputStream(System.in)) mkString "";
      }
    val parser:Parser = new Parser(new CharStreamFull(src))
    var term:Term = parser parse;
    var reduced:Boolean = false;
    do {
       term.output;
       println();
       val (r,t) = term.reduce;
       reduced = r;
       term = t;
    } while (reduced); 
  }
}
