


//
// Parser for simple C programs.  This component checks 
// the input program to see if it meets the syntax rules
// of simple C.  The parser returns a string denoting
// success or failure. 
//
// Returns: the string "success" if the input program is
// legal, otherwise the string "syntax_error: ..." is
// returned denoting an invalid simple C program.
//
// <<Kenan Alghythee>>
   // Writing a simple parsar of a subset of C lanagauge using recursion 
   // approach and mulitally recrusive 
   // U. of Illions, Chicago 
   // CS 341, Spring 2022
   // 

//
// Original author:
//   Prof. Joe Hummel
//   U. of Illinois, Chicago
//   CS 341, Spring 2022
//

namespace compiler

module parser =
  //
  // NOTE: all functions in the module must be indented.
  //

  //
  // matchToken
  //
  let private matchToken expected_token tokens =
    //
    // if the next token matches the expected token,  
    // keep parsing by returning the rest of the tokens.
    // Otherwise throw an exception because there's a 
    // syntax error, effectively stopping compilation
    // at the first error.
    //
    let next_token = List.head tokens

    if expected_token = next_token then  
      List.tail tokens
    else
      failwith ("expecting " + expected_token + ", but found " + next_token)


 
 // This function is used to handle the cases of
 // the expro_op and the BNF  is *)

  let private expr_op tokens = 
   let T1:string = List.head tokens 
   if T1 = "+" then
     let plus_token = matchToken "+" tokens 
     plus_token
   elif T1 = "*" then 
     let T2 = matchToken "*" tokens 
     T2
   elif T1 = "-" then 
     let T2 = matchToken "-" tokens 
     T2
   elif T1 = "/" then 
     let T2 = matchToken "/" tokens 
     T2
   elif T1 = "^" then 
      let T2 = matchToken "^" tokens 
      T2
   elif T1 = ">" then 
      let T2 = matchToken ">" tokens 
      T2
   elif T1 = "<" then 
      let T2 = matchToken "<" tokens 
      T2 
   elif T1 = ">=" then 
      let T2 = matchToken ">=" tokens 
      T2
   elif T1 = "<=" then 
      let T2 = matchToken "<=" tokens 
      T2
   elif T1 = "==" then 
      let T2 = matchToken "==" tokens 
      T2
   elif T1 = "!=" then 
      let T2 = matchToken "!=" tokens 
      T2
   else
     failwith ("expecting expression operator, but found " + T1)



// This is used to handle the (expr_value in the BNF) or the values
// that is input or output diffrent cases 
(* <expr-value> ->identifier|int_literal
|str_literal
|true
| false *)

  let private expr_value tokens  = 
    let T1:string = List.head tokens 
    if T1 = "true" then 
      let true_token = matchToken "true" tokens 
      true_token
    elif T1 = "false" then 
      let false_token = matchToken "false" tokens
      false_token
    elif T1.StartsWith("str_literal:") then
      let str_token = matchToken T1 tokens 
      str_token
    elif T1.StartsWith("int_literal:") then
      let int_token = matchToken T1 tokens 
      int_token
    elif T1.StartsWith("identifier:") then
      let T2 = matchToken T1 tokens 
      T2
    else
      failwith ("expecting identifier or literal, but found " + T1) 
      
(* This function is used to used to hanlde the diffrent possible logical expressions and has the following BNF : <expr>-> <expr-value> <expr-op> <expr-value>| <expr-value*)

  let private expr tokens =  
    let T1 = expr_value tokens
    let T2:string = List.head T1
    if T2 = "+" || T2 = "-" || T2 = "*" || T2 = "/" || T2 = "^" || T2 = "<" || T2 =">" || T2 = "<=" || T2 = ">=" || T2 = "==" || T2 = "!=" then
      let T3 = expr_op T1
      let T4 = expr_value T3
      T4
    else
      T1

(*This function is used to output_values rule in the BNF of which handle the cases showing the output values in the output rule 
<output-value> -> <expr-value>
| endl*)    

  let private output_values tokens =
    let T1:string = List.head tokens
    if T1.StartsWith("endl") then 
      let T2 = matchToken "endl" tokens 
      T2
    else 
      let T4 = expr_value tokens 
      T4
(* this function is used to hanld the empty cases rule of BNF empty 
<empty> -> ;
*)
  let private empty tokens = 
    let T1 = List.head tokens
    let T2 = matchToken ";" tokens
    T2
(*
This function is used to handle the defiatnion of a simple variable of type int and
the BNF  is <vardecl> ->int identifier;
*)
  let  private vardecl tokens =
    let T1 = List.head tokens 
    let T2 = matchToken "int" tokens
    let T3:string = List.head T2
    if T3.StartsWith("identifier:") then 
      let T4 = matchToken T3 T2
      let T5 = matchToken ";" T4
      T5
     
    else 
      failwith ("expecting identifier or literal, but found " + T3)
    
    
(*
 This function is used to handle the output rules and based on BNF
 <output>-> cout << <output-value> ;
*)
  let private output tokens = 
    
    let T2 = matchToken "cout" tokens 
    let T3 = matchToken "<<"  T2
    let T4 = output_values T3
    let T5 = matchToken ";" T4
    T5
(*
 This function is used to handle the input rules and based on BNF
 <input> -> cin >> identifier
*)
  let rec private input tokens = 
    let T1 = matchToken "cin" tokens 
    let T2 = matchToken ">>"  T1
    let T3 = List.head T2
    if T3.StartsWith("identifier:") then 
      let T4 = matchToken T3 T2
      let T5 = matchToken ";" T4
      T5
    else 
      failwith ("expecting identifier, but found " + T3)
(*
 This function is used to handle the assignment rules and based on BNF
<assignment> -> identifier = <expr> ;
*)   
    
  let private assignment tokens = 
    let T1:string = List.head tokens 
    if T1.StartsWith("identifier:") then 
      let T2 = matchToken T1 tokens
      let T3 = matchToken "=" T2 
      let T4 = expr T3
      let T5 = matchToken ";" T4
      T5
    else 
      failwith ("expecting identifier or literal, but found " + T1)

// this is used to handle the conidation rule 
// the BNF is : <condition> -> <expr>

  let private condition tokens = 
    let T1 = expr tokens 
    T1
(*
 This function is used to handle the stmt rules and based on BNF
<stmt> -> <empty>
 | <vardecl>
 | <input>
 | <output>
 | <assignment>
 | <ifstmt>
*)   
   
  let rec private stmt tokens = 
    let T1 = List.head tokens
    if T1 = ";"  then 
      empty tokens
    elif T1 = "int" then 
      vardecl tokens 
    elif T1 = "cout" then 
      output tokens 
    elif T1 = "cin" then 
      input tokens 
    elif T1 = "if" then 
      if_stmt tokens 
    elif T1.Contains("identifier:") then 
      assignment tokens 
    else 
      failwith ("expecting statement, but found " + T1)
(*
 This function is used to handle the if_stmt rules and based on BNF
<ifstmt> -> if ( <condition> ) <then-part> <else-part>
*)  
  and private if_stmt tokens = 
    let T1 = matchToken "if" tokens 
    let T2 = matchToken "("  T1
    let T3 = condition T2
    let T4 = matchToken ")" T3
    let T5 = then_part T4
    let T6 = List.head T5
    if T6 = "else" then 
      let T7 = else_part T5 
      T7
    else
      T5
(*
 This function is used to handle the then_part rules and based on BNF
<then-part> -> <stmt>
*) 
  and private then_part tokens = 
    let T1 = stmt tokens 
    T1
(*
 This function is used to handle the then_part rules and based on BNF
<else-part> -> else <stmt>
 | EMPTY
*)
  and private else_part tokens = 
    let check:string = List.head tokens 
    if check = "else" then 
      let T1 = matchToken "else" tokens 
      let T2 = stmt T1 
      T2
    else 
       tokens 
(*
 This function is used to handle the then_part rules and based on BNF
<morestmts> -> <stmt> <morestmts>
 | EMPTY
*)
  let rec private more_stmts tokens = 
    let T1:string = List.head tokens 
    if T1 = "}" then 
      tokens
    else 
      let T2 = stmt tokens 
      let T3 = more_stmts T2
      T3 
(*
 This function is used to handle the then_part rules and based on BNF
<stmts> -> <stmt> <morestmts>
*)
 
  let private stmts tokens =
    
    let T1 = stmt tokens 
    let T2  = more_stmts T1
    T2
(*
 This function is used to handle the then_part rules and based on BNF
<simpleC> -> void main ( ) { <stmts> } $
this is the main part where all other parts of the program 
*)
  let private simpleC tokens = 
    let T2 = matchToken "void" tokens
    let T3 = matchToken "main" T2
    let T4 = matchToken "(" T3
    let T5 = matchToken ")" T4
    let T6 = matchToken "{" T5

    let token_stmts = stmts T6

    let T7 = matchToken "}" token_stmts
    let T8 = matchToken "$" T7
    T8

  //
  // parse tokens
  //
  // Given a list of tokens, parses the list and determines
  // if the list represents a valid simple C program.  Returns
  // the string "success" if valid, otherwise returns a 
  // string of the form "syntax_error:...".
  //
  let parse tokens = 
    try
      let result = simpleC tokens
      "success"
    with 
      | ex -> "syntax_error: " + ex.Message
