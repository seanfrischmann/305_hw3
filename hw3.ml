(* CSE305 Spring 2014
 * A possible solution to HW2, expressed in Standard ML
 * Author: Carl Alphonce
 * Updated by: Sean Frischmann
 *)

(****************************************************************************
 TYPE DEFINITIONS 
 ****************************************************************************)

(* The type of expressions and special forms *)
datatype exp =
    Boolean of bool
  | Number of int
  | String of string
  | Name of string
  | Error | Quit | Add | Sub | Mul | Div | Rem | Pop | Exc | Neg
  | And | Or | Not | Equal | LessThan | If | Bind;

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
  | expression2string(Name(X)) = X
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

(* Computes primitive boolean functions*)
fun booleanFunctions(And, a2, a1) = if (a2 = a1) then true else false
   | booleanFunctions(Or, a2, a1) = if (a2 = a1) then false else true
   | booleanFunctions(Not, a2, a1) = if (a1 = true) then false else true;

(* Computes primitive relational operators*)
fun relationalOperators(Equal, a2, a1) = if (a2 = a1) then true else false
   | relationalOperators(LessThan, a2, a1) = if (a2 < a1) then true else false

(* Computes primitive if function*)
fun ifFunction(a1,a2,a3) = 
    if (a1 = true) then a3
    else a2;

(****************************************************************************
 MAIN EVALUATOR FUNCTIONS 
 ****************************************************************************)

(* Applies a primitive function to its two arguments *)

fun 
  (* 0-ARY OPERATORS *)
    apply(_,[])  = (Error,[])

  (* UNARY OPERATORS *)
  | apply(Neg,Number(a)::rest) = (Number(~a),rest)
  | apply(Not,Boolean(a1)::rest) = (Boolean(booleanFunctions(Not,true,a1)),rest)
  | apply(_,[x]) = (Error, [x])

  (* BINARY OPERATORS *)
  | apply(Add,Number(a1)::Number(a2)::rest) = (Number(a2+a1),rest)
  | apply(Sub,Number(a1)::Number(a2)::rest) = (Number(a2-a1),rest)
  | apply(Mul,Number(a1)::Number(a2)::rest) = (Number(a2*a1), rest)
  | apply(Div,Number(a1)::Number(a2)::rest) = (Number(quotient(a2,a1)),rest)
  | apply(Rem,Number(a1)::Number(a2)::rest) = (Number(remainder(a2,a1)),rest)
  | apply(And,Boolean(a1)::Boolean(a2)::rest) = (Boolean(booleanFunctions(And,a2,a1)),rest)
  | apply(Or,Boolean(a1)::Boolean(a2)::rest) = (Boolean(booleanFunctions(Or,a2,a1)),rest)
  | apply(Equal,Number(a1)::Number(a2)::rest) = (Boolean(relationalOperators(Equal,a2,a1)),rest)
  | apply(LessThan,Number(a1)::Number(a2)::rest) = (Boolean(relationalOperators(LessThan,a2,a1)),rest)
  | apply(If,Boolean(a1)::a2::a3::rest) = (ifFunction(a1,a2,a3),rest)
  | apply(_,stack) = (Error,stack)
  ;

(* stack operations *)

fun stackOps(Pop, x::stack) = stack
  | stackOps(Exc, x::y::stack) = y::x::stack
  | stackOps(_,stack) = Error::stack
  ;

(* Help Evaluates an expression *)
fun evalHelper(Boolean(x),stack) = Boolean(x)::stack
  | evalHelper(Number(x),stack) = Number(x)::stack
  | evalHelper(String(x),stack) = String(x)::stack
  | evalHelper(Name(x),stack) = Name(x)::stack
  | evalHelper(Quit,stack) = Quit::stack
  | evalHelper(Pop,x::stack) = stack
  | evalHelper(Exc,x::y::stack) = y::x::stack
  | evalHelper(Error,stack) = Error::stack
  | evalHelper(expr, stack) = let
            val (v,s) = apply(expr, stack)
      in
            v::s
      end;

(*Helps sort out stack after bind*)
fun bindStackHelper(a1, stack) = a1::stack;

(*Helps sort out bindings*)
fun bindBindingsHelper(a2, bindings) = a2::bindings;

fun bindValuesHelper(a1, values) = a1::values;

fun existCheck(a2, []) = false
  | existCheck(Name(a2), Name(x)::bindings) = if (Name(a2) = Name(x)) then true else existCheck(Name(a2), bindings)
  | existCheck(a2, x::bindings) = if (a2 = x) then true else existCheck(a2, bindings)
;

fun nameCheck(a1::Name(a2)::stack) = true
  | nameCheck(_) = false
;

fun checkIfName(Name(a1)) = true;

fun findValue(Name(a2), Name(x)::bindings, y::values) = 
    if (Name(a2) = Name(x)) then y
    else findValue(Name(a2), bindings, values)
;

fun bindFailure(a1::Name(a2)::stack) = a1::stack
  | bindFailure(Name(a1)::stack) = stack
  | bindFailure(stack) = stack
;

(* Evaluates Bind*)
fun bindFunction(a1::a2::stack, bindings::values::rest) = 
     if existCheck(a2, bindings)
         then ((evalHelper(Error, stack))::bindings::values::rest)
     else ((bindStackHelper(a1,stack))::(bindBindingsHelper(a2,bindings)::bindValuesHelper(a1, values)::rest))
  | bindFunction(a1::stack,bindings::rest) = ((evalHelper(Error, stack))::bindings::rest)
  | bindFunction(stack,bindings::rest) = ((evalHelper(Error, stack))::bindings::rest)
;

(* Evaluates an expression *)

fun eval(Boolean(x), environment) = ((evalHelper(Boolean(x),hd environment))::(tl environment))
  | eval(Number(x), environment)  = ((evalHelper(Number(x),hd environment))::(tl environment))
  | eval(String(x), environment)  = ((evalHelper(String(x),hd environment))::(tl environment))
  | eval(Name(x), stack::bindings::values::rest)  = 
      if existCheck(Name(x),bindings) then (
          (findValue(Name(x),bindings,values)::stack)::(bindings::values::rest))
      else ((evalHelper(Name(x), stack))::(bindings::values::rest))
  | eval(Quit, environment)       = ((evalHelper(Quit,hd environment))::(tl environment))
  | eval(Pop, environment)     = ((evalHelper(Pop,hd environment))::(tl environment))
  | eval(Exc, environment )     = ((evalHelper(Exc,hd environment))::(tl environment))
  | eval(Error, environment)      = ((evalHelper(Error,hd environment))::(tl environment))
  | eval(Bind, stack::bindings)      = 
      if nameCheck(stack) then (bindFunction(stack, bindings))
      else ((evalHelper(Error, bindFailure(stack)))::(bindings))
  | eval(expr, environment ) = ((evalHelper(expr,hd environment))::(tl environment))
  ;

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

fun parseString(x, inStr, y) = 
    case (TextIO.input1(inStr)) of
	NONE => SOME(Error)
      | SOME(ch) => 
       if (ch = #"\"") then parseString(x, inStr, (y+1))
       else if (Char.isAlpha(ch)) then parseString(x^Char.toString(ch), inStr, y)
       else if (Char.isSpace(ch)) then
            if (y < 1) then parseString(x^Char.toString(ch), inStr, y)
            else SOME(String(x))
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
	       else if (x = "and") then SOME(And)
	       else if (x = "or") then SOME(Or)
	       else if (x = "not") then SOME(Not)
	       else if (x = "equal") then SOME(Equal)
	       else if (x = "lessThan") then SOME(LessThan)
	       else if (x = "if") then SOME(If)
	       else if (x = "bind") then SOME(Bind)
               else if (x = "quit") then SOME(Quit)
	       else SOME(Name(x))
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
 else if (ch = #"\"")        then parseString("", inStr, 0)
 else if (Char.isAlpha(ch)) then parsePrimitive(Char.toString(ch), inStr)
 else if (Char.isSpace(ch)) then parseHelper(TextIO.input1(inStr), inStr)
 else NONE;

fun maybePrintPrompt(stack::bindings::values::rest, SOME(0)) = ( printStack(stack); TextIO.print("repl> ") )
  | maybePrintPrompt(stack, SOME(_)) = ();

(* Function to parse the next expression on the input stream. *)      
fun parse(inStr) = parseHelper(TextIO.input1(inStr), inStr);

fun replHelper(inStr, environment) =
(
    maybePrintPrompt(environment, TextIO.canInput(inStr,1));
    case (parse(inStr)) of 
       NONE => replHelper(inStr, environment)
     | SOME(Quit) => () 
     | SOME(expression) => replHelper(inStr,eval(expression, environment)) 
);


fun repl() = replHelper(TextIO.stdIn, [[],[],[]]);
