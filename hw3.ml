(* CSE305 Spring 2014
 * A possible solution to HW2, expressed in Standard ML
 * Author: Carl Alphonce
 *)

(****************************************************************************
 TYPE DEFINITIONS 
 ****************************************************************************)

(* The type of expressions and special forms *)
datatype exp =
    Boolean of bool
  | Number of int
  | String of string
  | Error | Quit | Add | Sub | Mul | Div | Rem | Pop | Exc | Neg;

(****************************************************************************
 UTILITY FUNCTIONS 
 ****************************************************************************)

(* Utility function to build a textual form of an expression
   or special form, used for printing.
 *)
fun expression2string(Boolean(true)) = ":true:"
  | expression2string(Boolean(false)) = ":false:"
  | expression2string(Number(X)) = if (X<0) then "-"^Int.toString(~X) else Int.toString(X)
  | expression2string(Error) = ":error:"
  | expression2string(String(X)) = "\""^X^"\""
  | expression2string(_) = "?? should not happen ??"
  ;

(* Utility function to print stack *)
fun printStack([]) = ()
  | printStack(x::xs) = ( TextIO.print(expression2string(x)^"\n") ; printStack(xs) );

(* Computes quotient according to rules of the language *)
fun quotient(a2,a1) =
    if (a1<0)
    then if (a2 div a1) * a1 > a2 then (a2 div a1)+1 else (a2 div a1)
    else (a2 div a1);

(* Computes remainder according to rules of the language *)
fun remainder(a2,a1) = a2 - a1 * quotient(a2,a1);


(****************************************************************************
 MAIN EVALUATOR FUNCTIONS 
 ****************************************************************************)

(* Applies a primitive function to its two arguments *)

fun 
  (* 0-ARY OPERATORS *)
    apply(_,[])  = (Error,[])

  (* UNARY OPERATORS *)
  | apply(Neg,Number(a)::rest) = (Number(~a),rest)
  | apply(_,[x]) = (Error,[x])

  (* BINARY OPERATORS *)
  | apply(Add,Number(a1)::Number(a2)::rest) = (Number(a2+a1),rest)
  | apply(Sub,Number(a1)::Number(a2)::rest) = (Number(a2-a1),rest)
  | apply(Mul,Number(a1)::Number(a2)::rest) = (Number(a2*a1),rest)
  | apply(Div,Number(a1)::Number(a2)::rest) = (Number(quotient(a2,a1)),rest)
  | apply(Rem,Number(a1)::Number(a2)::rest) = (Number(remainder(a2,a1)),rest)
  | apply(_,stack) = (Error,stack)
  ;

(* stack operations *)

fun stackOps(Pop, x::stack) = stack
  | stackOps(Exc, x::y::stack) = y::x::stack
  | stackOps(_,stack) = Error::stack
  ;

(* Evaluates an expression *)

fun eval(Boolean(x), stack) = Boolean(x)::stack
  | eval(Number(x), stack)  = Number(x)::stack
  | eval(String(x), stack)  = String(x)::stack
  | eval(Quit, stack)       = Quit::stack
  | eval(Pop, x::stack)     = stack
  | eval(Exc, x::y::stack)     = y::x::stack
  | eval(Error, stack)      = Error::stack
  | eval(expr, stack) = let
        val (v,s) = apply(expr, stack)
    in
        v::s
    end;

(* Functions to parse a number *)

fun parseNumber(x,inStr) = 
    case (TextIO.input1(inStr)) of
	NONE => SOME(Error)
      | SOME(ch) => 
	    if (Char.isDigit(ch)) then parseNumber(x*10+(ord(ch)-ord(#"0")),inStr)
       else if (Char.isSpace(ch)) then SOME(Number(x))
       else SOME(Error)
      ;

fun parseNegativeNumber(inStr) = 
    case ( parseNumber(0,inStr) ) of
        NONE => SOME(Error)
      | SOME(Number(num)) =>  SOME(Number(~1 * num))
      | SOME(_) => SOME(Error) 
      ;

(* Function to parse a boolean  *)

fun parseBoolean(x, inStr) = 
    case (TextIO.input1(inStr)) of
	NONE => SOME(Error)
      | SOME(ch) => 
	    if (Char.isAlpha(ch) orelse ch = #":") then parseBoolean(x^Char.toString(ch),inStr)
       else if (Char.isSpace(ch))
	       then if (x = ":true:")  then SOME(Boolean(true))
	       else if (x = ":false:") then SOME(Boolean(false))
	       else SOME(Error)
       else SOME(Error);

(* Function to parse a string  *)

fun parseString(x, inStr) = 
    case (TextIO.input1(inStr)) of
	NONE => SOME(Error)
      | SOME(ch) => 
       if (ch = #"\"") then parseString(x, inStr)
       else if (Char.isAlpha(ch)) then parseString(x^Char.toString(ch), inStr)
       else if (Char.isSpace(ch)) then SOME(String(x))
       else SOME(Error);

(* Function to parse an error *)

fun parseError(x, inStr) = 
    case (TextIO.input1(inStr)) of
	NONE => SOME(Error)
      | SOME(ch) => 
	    if (Char.isAlpha(ch) orelse ch = #":") then parseError(x^Char.toString(ch),inStr)
       else if (Char.isSpace(ch))
	       then if (x = ":error:")  then SOME(Error)
	       else SOME(Error)
       else SOME(Error);

(***** EDITED TO HERE ********************************************************************)

(* Function to parse a boolean or error  *)

fun parseBooleanOrError(x, inStr) = 
    case (TextIO.input1(inStr)) of
	NONE => SOME(Error)
      | SOME(ch) => 
	    if (ch = #"e")                  then parseError(x^Char.toString(ch),inStr)
       else if (ch = #"t" orelse ch = #"f") then parseBoolean(x^Char.toString(ch),inStr)
       else SOME(Error);

(* Function to parse a primitive  *)

fun parsePrimitive(x, inStr) = 
    case (TextIO.input1(inStr)) of
	NONE => SOME(Error)
      | SOME(ch) => 
	    if (Char.isAlpha(ch))  then parsePrimitive(x^Char.toString(ch),inStr)
       else if (Char.isSpace(ch))
	       then if (x = "add") then SOME(Add)
	       else if (x = "sub") then SOME(Sub)
	       else if (x = "mul") then SOME(Mul)
	       else if (x = "div") then SOME(Div)
	       else if (x = "rem") then SOME(Rem)
	       else if (x = "pop") then SOME(Pop)
	       else if (x = "exc") then SOME(Exc)
	       else if (x = "neg") then SOME(Neg)
               else if (x = "quit") then SOME(Quit)
	       else SOME(Error)
       else SOME(Error);

(* A recursive helper function for the parse function, which reads
   a character from the input stream and determines what more
   specific parsing function to call.
 *)      
fun parseHelper(NONE, inStr) = NONE
  | parseHelper(SOME(ch), inStr) =
      if (Char.isDigit(ch)) then parseNumber(ord(ch)-ord(#"0"),inStr)
 else if (ch = #"-")        then parseNegativeNumber(inStr)
 else if (ch = #":")        then parseBooleanOrError(":", inStr)
 else if (ch = #"\"")        then parseString("", inStr)
 else if (Char.isAlpha(ch)) then parsePrimitive(Char.toString(ch), inStr)
 else if (Char.isSpace(ch)) then parseHelper(TextIO.input1(inStr), inStr)
 else NONE;

fun maybePrintPrompt(stack, SOME(0)) = ( printStack(stack); TextIO.print("repl> ") )
  | maybePrintPrompt(stack, SOME(_)) = ();

(* Function to parse the next expression on the input stream. *)      
fun parse(inStr) = parseHelper(TextIO.input1(inStr), inStr);

fun replHelper(inStr, stack) =
(
    maybePrintPrompt(stack, TextIO.canInput(inStr,1));
    case (parse(inStr)) of 
       NONE => replHelper(inStr, stack)
     | SOME(Quit) => () 
     | SOME(expression) => replHelper(inStr,eval(expression, stack)) 
);


fun repl() = replHelper(TextIO.stdIn, []);
