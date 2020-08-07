type constant = Int of int | Bool of string | Error of string | String of string | Name of string | Unit of string | Closure of ((constant * command list) * environment list * bool)

and  command = Push of constant | Add | Sub | Mul | Div | Rem | Neg | Swap | Pop | ToString | PrintLn | Quit | Cat | And | Or | Not | Equal | LessThan | Bind | If | Let | End | FUN of constant * constant | INOUTFUN of constant * constant | FUNEND | RETURN| CALL
and  environment = Binding of constant * constant


let errorState : constant = Error ":error:"
let constantError : constant = Error ":constantError:"
let unitState : constant = Unit ":unit:"
let digit = '0'::'1'::'2'::'3'::'4'::'5'::'6'::'7'::'8'::'9'::[]
let alphabet = 'a'::'b'::'c'::'d'::'e'::'f'::'g'::'h'::'i'::'j'::'k'::'l'::'m'::'n'::'o'::'p'::'q'::'r'::'s'::'t'::'u'::'v'::'w'::'x'::'y'::'z'::[]
let notDigit = 'a'::'b'::'c'::'d'::'e'::'f'::'g'::'h'::'i'::'j'::'k'::'l'::'m'::'n'::'o'::'p'::'q'::'r'::'s'::'t'::'u'::'v'::'w'::'x'::'y'::'z'::'"'::'_'::'-'::[]

let rec isDigit (suspectString : string) (digitList : char list)  =
  match digitList with
  |[] -> true
  |a::b ->
    if(String.contains suspectString a) then
      false
    else
      isDigit suspectString b

let rec hasLetter (suspectString : string) (charList : char list) =
  match charList with
  |[] -> false
  |a::b ->
    if(String.contains suspectString a) then
      true
    else
      hasLetter suspectString b

let rec chopString (listedString : string list) (acc : string list)  = 
  match listedString with
  |[] -> acc
  |a::b -> 
    if(String.contains a '"') then chopString b acc
    else
      a::chopString listedString acc

let getConstant (rawConstant : string list)  =
  (* This function takes in a list of strings which contains a Constant type being pushed onto the stack,
  the Constant type is determined and returned for inserting onto the command stack for processing. *)
  (* Printf.printf "Head of passed constant: %s\n" (String.concat " " rawConstant); *)
  let fullString = String.concat " " rawConstant in
  let fullStringN = String.concat "" rawConstant in
  match rawConstant with
  |[] -> errorState
  |head::tail ->
    if ((String.contains (String.sub fullStringN 0 1) '-') && (not (String.contains fullStringN '.')) && (isDigit (String.lowercase_ascii(String.sub fullStringN 1 ((String.length fullStringN)-1)))  notDigit)) then Int (int_of_string((fullStringN)))
    else if ( (isDigit (String.lowercase_ascii(fullStringN)) notDigit) && (not (String.contains fullStringN '.'))) then Int (int_of_string(fullStringN) )
    else if (String.contains (String.sub fullString 0 1) '"') then String (String.sub (fullString) 1 ((String.length (fullString))-2))
    else if (String.contains (String.sub fullString 0 1) '_' || (hasLetter (String.lowercase_ascii (String.sub fullString 0 1)) alphabet)) then Name (fullStringN)
    else if (String.equal head ":true:") then (Bool head)
    else if (String.equal head ":false:") then Bool head
    else if (String.equal head ":unit:") then Unit head
    else
      errorState
let constructFunction (rawConstant : string list) = 
  (* This function takes in a string list where the head is the name of the first constant and the tail
  contains the name of the second constant to be used in the function. The function type is constructed
  and returned to the command stack. Incoming function body is seperated by whitespace *)
  let name1 = getConstant ((List.hd rawConstant) :: []) in
  let name2 = getConstant ((List.tl rawConstant)) in
 (* Printf.printf "NAME OF PARAM: %s" (List.hd rawConstant);*)
  FUN(name1,name2)

let constructInOutFunction (rawConstant : string list) = 
  (* This function takes in a string list where the head is the name of the first constant and the tail contains
  the name of the second constant to be used in the function. The function type is constructed and returned 
  to the command stack. Incoming function body is seperated by whitespace *)
  let name1 = getConstant ((List.hd rawConstant) :: []) in
  let name2 = getConstant ((List.tl rawConstant)) in

  INOUTFUN(name1,name2)

let rec findCommands (rawList : string list) (commandList : command list) = 
    (* This function takes in a string list of input commands read from file and a command stack
    to be populated with commands. *)
    match rawList with
    | [] -> commandList
    | firstCommand :: tail ->
      if(String.equal firstCommand "add") then Add::findCommands tail commandList
      else if (String.equal firstCommand "sub") then Sub::findCommands tail commandList
      else if (String.equal firstCommand "mul") then Mul::findCommands tail commandList
      else if (String.equal firstCommand "div") then Div::findCommands tail commandList
      else if (String.equal firstCommand "rem") then Rem::findCommands tail commandList
      else if (String.equal firstCommand "neg") then Neg::findCommands tail commandList
      else if (String.equal firstCommand "swap") then Swap::findCommands tail commandList
      else if (String.equal firstCommand "pop") then Pop::findCommands tail commandList
      else if (String.equal firstCommand "toString") then ToString::findCommands tail commandList
      else if (String.equal firstCommand "println") then PrintLn :: findCommands tail commandList
      else if (String.equal firstCommand "quit") then Quit :: findCommands [] commandList
      else if (String.equal firstCommand "cat") then Cat :: findCommands tail commandList
      else if (String.equal firstCommand "and") then And :: findCommands tail commandList
      else if (String.equal firstCommand "or") then Or :: findCommands tail commandList
      else if (String.equal firstCommand "not") then Not :: findCommands tail commandList
      else if (String.equal firstCommand "equal") then Equal :: findCommands tail commandList
      else if (String.equal firstCommand "lessThan") then LessThan :: findCommands tail commandList
      else if (String.equal firstCommand "bind") then Bind :: findCommands tail commandList
      else if (String.equal firstCommand "if") then If :: findCommands tail commandList
      else if (String.equal firstCommand "let") then Let :: findCommands tail commandList
      else if (String.equal firstCommand "end") then End :: findCommands tail commandList
      else if (String.equal firstCommand "funEnd") then FUNEND::findCommands tail commandList
      else if (String.equal firstCommand "call") then CALL::findCommands tail commandList
      else if (String.equal firstCommand "return") then RETURN::findCommands tail commandList
      (*else if (String.equal firstCommand "push") then Push (getConstant String.split_on_char ' ' firstCommand) :: findCommands *)
      (* The below section is reached when we encounter a function command. The string is split again to deduce the type of function. *)
      else
        let seperatedPush = String.split_on_char ' ' firstCommand in
        (*Printf.printf "Debug: %s\n" (String.concat "" (List.tl seperatedPush));*)
        if(String.equal (List.hd seperatedPush) "push") then Push (getConstant (List.tl seperatedPush)) :: findCommands tail commandList
        else if(String.equal (List.hd seperatedPush) "fun") then (constructFunction (List.tl seperatedPush)):: findCommands tail commandList
        else if (String.equal (List.hd seperatedPush) "inOutFun") then constructInOutFunction (List.tl seperatedPush) :: findCommands tail commandList
        else
          Push errorState :: findCommands tail commandList (*findCommands tail commandList*)
  
  
  
  let rec findVar (env : environment list) (target : constant)  = 
  (*This function takes a environment list (memory) and a target name and returns the constant bound to the  target name (such as Int 5) *)
    match env with
    |[] -> errorState
    |Binding(value, name)::b ->  if( name = target) then value else findVar b target

  let isType (flag : string) (mystery : constant) =
  (* This function takes a string telling us the type of constant to compare the given constant to. 
  If flag matches given constant type, return true else false *)
    match mystery with
    |Int a -> if(flag = "int") then true else false
    |Bool a -> if(flag = "bool") then true else false
    |Error a -> false
    |String a -> if(flag = "string") then true else false
    |Name a -> if(flag = "name") then true else false
    |Unit a -> if (flag = "unit") then true else false
    |Closure ((a,b), c, q) ->  if (flag = "closure") then true else false

  let getValue (mystery : constant) : string =
  (* This function takes a constant and returns the data of the constant in string format *)
    match mystery with
    |Int a -> string_of_int a
    |Bool a -> a
    |Error a -> a
    |String a -> a
    |Name a -> a
    |Unit a -> a
    |Closure ((constant,commands), stuff, inOut) -> ":closure:"
  
  let popStack (stack: constant list) =
    match stack with
    |[] -> errorState::stack
    |a::b -> b
  
  (* The arthimetic functions take a stack and an environment list and performs the relevant computation.
    If names are encountered, their bound value is retrieved *)
  let addValues (stack : constant list) (m : environment list) =
    match stack with
    |Int a::Int b:: c -> (Int (a+b)::c)
    |Name a::Int b:: c -> if(isType "int" (findVar m (Name a) ) ) then (Int( int_of_string (getValue ( (findVar m (Name a)) )) + b) :: c) else errorState::stack
    |Name a :: Name b :: c -> if(isType "int" (findVar m (Name a)) && (isType "int" (findVar m (Name b) ) ) ) then (Int( (int_of_string (getValue ( (findVar m (Name a)) )) ) + (int_of_string (getValue ( (findVar m (Name b)) )) ) ) :: c) else errorState::stack
    |Int a :: Name b :: c -> if(isType "int" (findVar m (Name b) ) ) then (Int( int_of_string (getValue ( (findVar m (Name b)) )) + a) :: c) else errorState::stack
    |Int a :: b :: c -> errorState::stack
    |Int a :: [] -> errorState::stack
    |_ -> errorState::stack

  let subValues (stack : constant list) (m : environment list)  =
    match stack with 
    |Int a::Int b:: c -> (Int(b-a)::c)
    |Name a::Int b:: c -> if(isType "int" (findVar m (Name a) ) ) then (Int( b - (int_of_string (getValue ( (findVar m (Name a))) )) ) :: c) else errorState::stack
    |Name a :: Name b :: c -> if(isType "int" (findVar m (Name a)) && (isType "int" (findVar m (Name b) ) ) ) then (Int( (int_of_string (getValue ( (findVar m (Name b)) )) ) - (int_of_string (getValue ( (findVar m (Name a)) )) ) ) :: c) else errorState::stack
    |Int a :: Name b :: c -> if(isType "int" (findVar m (Name b) ) ) then (Int( int_of_string (getValue ( (findVar m (Name b)) )) - a) :: c) else errorState::stack
    |Int a::b ::c -> errorState::stack
    |Int a :: [] -> errorState::stack
    |_ -> errorState::stack

  let mulValues (stack : constant list) (m : environment list) =
    match stack with
    |Int a::Int b :: c -> (Int(a*b)::c)
    |Name a::Int b:: c -> if(isType "int" (findVar m (Name a) ) ) then (Int( int_of_string (getValue ( (findVar m (Name a)) )) * b) :: c) else errorState::stack
    |Name a :: Name b :: c -> if(isType "int" (findVar m (Name a)) && (isType "int" (findVar m (Name b) ) ) ) then (Int( (int_of_string (getValue ( (findVar m (Name a)) )) ) * (int_of_string (getValue ( (findVar m (Name b)) )) ) ) :: c) else errorState::stack
    |Int a :: Name b :: c -> if(isType "int" (findVar m (Name b) ) ) then (Int( int_of_string (getValue ( (findVar m (Name b)) )) * a) :: c) else errorState::stack
    |Int a:: b :: c -> errorState::stack
    |Int a::[]-> errorState::stack
    |_-> errorState::stack

  let divValues (stack : constant list) (m: environment list)=
    match stack with
    |Int y::Int x :: c -> if(y = 0) then errorState::stack else Int(x/y)::c
    |Name y :: Int x :: c -> if(isType "int" (findVar m (Name y) ) && ( int_of_string ( getValue (findVar m (Name y)) )  != 0 ) )  then (Int(   x / ( int_of_string (getValue ( (findVar m (Name y)) )) ) ) :: c) else errorState::stack
    |Name y :: Name x :: c -> if(isType "int" (findVar m (Name y) ) && ( int_of_string ( getValue (findVar m (Name y)) )  != 0 ) && isType "int" (findVar m (Name x) ) ) then (Int(   ( int_of_string (getValue ( (findVar m (Name x)) )) ) / ( int_of_string (getValue ( (findVar m (Name y)) )) ) ) :: c) else errorState::stack
    |Int y :: Name x :: c -> if(isType "int" (findVar m (Name x) ) && ( y  != 0 ) )  then (Int( ( int_of_string (getValue ( (findVar m (Name x) ) ) ) ) / y ) :: c) else errorState::stack
    |Int y :: x :: c -> errorState::stack
    |Int y::[]-> errorState::stack
    |_ -> errorState::stack

  let remValues (stack:constant list) (m : environment list) =
    match stack with
    |Int y :: Int x :: c -> if( y = 0) then errorState::stack else Int(x mod y) :: c
    |Name y :: Int x :: c -> if(isType "int" (findVar m (Name y) ) && ( int_of_string ( getValue (findVar m (Name y)) )  != 0 ) )  then (Int(   x mod ( int_of_string (getValue ( (findVar m (Name y)) )) ) ) :: c) else errorState::stack
    |Name y :: Name x :: c -> if(isType "int" (findVar m (Name y) ) && ( int_of_string ( getValue (findVar m (Name y)) )  != 0 ) && isType "int" (findVar m (Name x) ) ) then (Int(   ( int_of_string (getValue ( (findVar m (Name x)) )) ) mod ( int_of_string (getValue ( (findVar m (Name y)) )) ) ) :: c) else errorState::stack
    |Int y :: Name x :: c -> if(isType "int" (findVar m (Name x) ) && ( y  != 0 ) )  then (Int( ( int_of_string (getValue ( (findVar m (Name x) ) ) ) ) mod y ) :: c) else errorState::stack
    |Int a :: b :: c -> errorState::stack
    |Int a::[] -> errorState::stack
    |_ -> errorState::stack

  let negValue (stack : constant list) (m: environment list) = 
    match stack with
    |[] -> errorState::stack
    |Int a::b -> Int(0-a)::b
    |Name a ::b -> if(isType "int" (findVar m (Name a) ) ) then Int(0 - ( int_of_string (getValue ( (findVar m (Name a)) ) ) ) ) :: b else errorState::stack
    |a::b -> errorState::stack
  
  let notValue (stack : constant list) (m: environment list) =
    match stack with
    |[] -> errorState::stack
    |Bool a::b -> if(a = ":true:") then Bool":false:"::b else Bool":true:"::b
    |Name a :: b -> if(isType "bool" (findVar m (Name a) ) && ( ( getValue (findVar m (Name a) ) ) = ":true:" )  ) then Bool":false:" :: b
    else if(isType "bool" (findVar m (Name a) ) && ( ( getValue (findVar m (Name a) ) ) = ":false:" )  ) then Bool":true:"::b else errorState::stack
    |_ -> errorState::stack

  let swapValues (stack : constant list) = 
    match stack with
    |[] -> errorState::stack
    |a::[] -> errorState::stack
    |a::b::c -> b::a::c

  let toStringValues (stack : constant list ) (m : environment list) = 
    match stack with
    |[] -> errorState::stack
    |a::b ->
      match a with
      |Int x -> (String (string_of_int x))::b
      |Bool value -> (String value)::b
      |Error value -> (String value)::b
      |Unit value -> (String value)::b
      |String value -> String value ::b
      |Name value -> (String value)::b
      |Closure ((constant,commands), stuff, inOut) -> errorState :: b
  
  let cat (stack : constant list ) (m : environment list) = 
    (* Concatonates two strings, if any variable is a name constant then its bound value is returned from the environment *)
    match stack with
    |[] -> errorState :: stack
    |String x:: String y:: b -> String (y^x) :: b
    |Name x :: String y :: b -> if(isType "string" (findVar m (Name x) ) ) then String( y ^ (getValue (findVar m (Name x) ) )  ) :: b else errorState::stack
    |Name x :: Name y :: b -> if(isType "string" (findVar m (Name x) ) && isType "string" (findVar m (Name y) ) ) then String( (getValue (findVar m (Name y)) ^ (getValue (findVar m (Name x) ) )  ))  :: b else errorState::stack
    |String x :: Name y :: b -> if(isType "string" (findVar m (Name y))) then String(  (getValue (findVar m (Name y) ) ) ^ x ) :: b else errorState::stack
    |String x :: b :: c -> errorState::stack
    |String x :: [] -> errorState::stack
    |_ -> errorState::stack

    (* The logical functions below logically evaluates booleans on the stack. If any are names, then their bound values
    are retrieved from the environment *)
  let andOP (stack : constant list ) (m: environment list) = 
    match stack with
    |[] -> errorState :: stack
    |Bool x :: Bool y :: b -> if(x = y) then (Bool":true:") ::b else (Bool":false:") :: b
    |Name x :: Bool y :: b -> if(isType "bool" (findVar m (Name x))) then 
    if (getValue (findVar m (Name x) )) = y then (Bool":true:")::b else (Bool":false:") :: b else errorState::stack
    |Name x :: Name y :: b -> if(isType "bool" (findVar m (Name x)) && isType "bool" (findVar m (Name y) ))  then 
    if(getValue (findVar m (Name x) )) = (getValue (findVar m (Name y)))  then (Bool":true:")::b else (Bool":false:") :: b else errorState::stack
    |Bool x :: Name y :: b -> if(isType "bool" (findVar m (Name y))) then 
    if(x = (getValue (findVar m (Name y)))) then (Bool":true:")::b else (Bool":false:")::b else errorState::stack
    |Bool x :: b :: c -> errorState::stack
    |Bool x :: [] -> errorState::stack
    |_ -> errorState::stack
  
  let orOP (stack : constant list) (m : environment list) = 
    match stack with
    |[] -> errorState :: stack
    |Bool x :: Bool y :: b -> if(x = ":true:"|| y = ":true:") then Bool":true:"::b else Bool":false:"::b
    |Name x :: Bool y :: b -> if(isType "bool" (findVar m (Name x)) ) then
    if( (getValue(findVar m (Name x)) ) = ":true:" || y = ":true:" ) then Bool":true:"::b else Bool":false:"::b else errorState::stack
    |Name x :: Name y :: b -> if(isType "bool" (findVar m (Name x)) && isType "bool" (findVar m (Name y))  ) then
    if( (getValue (findVar m (Name x)) ) = ":true:" || getValue(findVar m (Name y) ) = ":true:" ) then Bool":true:"::b else Bool":false:"::b else errorState::stack
    |Bool x :: Name y :: b -> if(isType "bool" (findVar m (Name y))) then 
    if( getValue(findVar m (Name y)) = ":true:" || x = ":true:" ) then Bool":true:"::b else Bool":false:"::b else errorState::stack
    |Bool x :: b :: c -> errorState::stack
    |Bool x :: [] -> errorState::stack
    |_ -> errorState::stack

  let equality (stack : constant list) (m : environment list) = 
    match stack with
    |[] -> errorState :: stack
    |Int x :: Int y :: b -> if(x = y) then Bool":true:" :: b else Bool":false:" :: b
    |Name x :: Int y :: b -> if(isType "int" (findVar m (Name x) ) ) then
    if(int_of_string (getValue(findVar m (Name x))) = y) then Bool":true:" :: b else Bool ":false:"::b else errorState::stack
    |Int x :: Name y :: b -> if(isType "int" (findVar m (Name y) ) ) then
    if(int_of_string (getValue(findVar m (Name y))) = x) then Bool":true:" :: b else Bool ":false:"::b else errorState::stack
    |Name x :: Name y :: b -> if(isType "int" (findVar m (Name y) )  && isType "int" (findVar m (Name x) ) ) then
    if(int_of_string (getValue(findVar m (Name y))) = int_of_string (getValue(findVar m (Name x)))) then Bool":true:"::b else Bool":false:"::b else errorState::stack
    |Int x :: b :: c -> errorState::stack
    |Int x :: [] -> errorState::stack
    |_ -> errorState::stack

  let lessThan (stack : constant list) (m : environment list) =
    match stack with
    |[] -> errorState :: stack
    |Int x :: Int y :: b -> if(y<x) then Bool":true:" :: b else Bool ":false:" :: b
    |Name x :: Int y :: b -> if(isType "int" (findVar m (Name x) ) ) then
    if(y < int_of_string(getValue(findVar m (Name x)))) then Bool":true:" :: b else Bool":false:" :: b else errorState::stack
    |Int x :: Name y :: b -> if(isType "int" (findVar m (Name y) ) ) then 
    if(int_of_string(getValue(findVar m (Name y))) < x) then Bool":true:" :: b else Bool":false:" :: b else errorState::stack
    |Name x :: Name y :: b -> if(isType "int" (findVar m (Name y) ) && isType "int" (findVar m (Name x)) ) then
    if(int_of_string(getValue(findVar m (Name y))) < int_of_string(getValue(findVar m (Name x)) )) then Bool ":true:" :: b else Bool":false:" ::b else errorState::stack  
    |Int x :: b :: c -> errorState::stack
    |Int x :: [] -> errorState::stack
    |_ -> errorState::stack


  let rec checkMemory (m : environment list) (target : constant) =
    (* Checks if the incoming name has an assigned value. If none, report false *)
    match m with
    |[] -> false
    |Binding(value, name)::b -> if(name = target) then true else checkMemory b target
  
  let bindForStack (stack : constant list) (m : environment list) =
    (* Takes a stack and an environment list, pops the first two values off the stack. If the first value is a name,
    its bound value is retrieved from the environment. A unitstate is pushed on the stack as a result of the binding *)
    match stack with
    |[] -> errorState :: stack
    |Int a :: Name b :: c ->  unitState::c
    |String a :: Name b :: c -> unitState::c
    |Bool a :: Name b :: c -> unitState::c
    |Unit a :: Name b :: c -> unitState::c
    |Name a :: Name b :: c -> if(checkMemory m (Name a)) then unitState::c else errorState::stack
    |_-> (*Printf.printf "CATCHALL BINDFORSTACK"; *) errorState::stack

  let bindForMemory (stack : constant list) (m : environment list) =
    (* Takes a stack and environment list, and binds the first two values on the stack. If the top value is a name, its 
    value is retrieved from the environment  *)
    match stack with
    |[] -> m
    |Int a :: Name b :: c ->  Binding(Int a, Name b)::m
    |String a :: Name b :: c -> Binding(String a, Name b) :: m
    |Bool a :: Name b :: c ->  Binding(Bool a, Name b) :: m
    |Unit a :: Name b :: c ->  Binding(Unit a, Name b) :: m
    |Name a :: Name b :: c -> if(checkMemory m (Name a)) then (Binding( (findVar m (Name a)), Name b) :: m) else  m
    |_-> m
  
  let ifStatement (stack : constant list) (m: environment list) =
    (* Takes a stack and environment list and evaluates an if statement on the top three values *)
    match stack with
    |[] -> errorState::stack
    |a :: b :: Bool c :: d -> if(c = ":true:") then a::d else b::d
    |a :: b :: Name c :: d -> if(isType "bool" (findVar m (Name c))) then 
    if( (getValue(findVar m (Name c))) = ":true:") then a::d else b::d else errorState::stack
    |_-> errorState::stack

    let rec clearScopes (commandList : command list) (scopeCt : int)  =
    (* Takes a command list and removes the following let-end sequence. Should account for nested let-ins *)
      match commandList with
      | Let :: tail -> if(scopeCt = 0) then commandList else clearScopes tail (scopeCt + 1)
      | End :: tail -> if(scopeCt = 0) then commandList else clearScopes tail (scopeCt - 1)
      | command :: tail -> if(scopeCt = 0) then commandList else clearScopes tail scopeCt 
      | [] -> []

    let rec clearFunctions (commandList : command list) (funScopeCt : int) = 
    (* Takes a command list and removes the following function sequence. Should account for curried functions  *)
      match commandList with
      | FUN(name, param) :: tail -> if(funScopeCt = 0) then commandList else clearFunctions tail (funScopeCt + 1)
      | FUNEND :: tail -> if(funScopeCt = 0) then commandList else clearFunctions tail (funScopeCt - 1)
      | command :: tail -> if(funScopeCt = 0) then commandList else clearFunctions tail funScopeCt
      | [] -> []

    let rec buildFunCommands (commandList : command list) (funScopeCt : int) (newCommands : command list) = 
    (* Copy commands until funEnd type into the newCommands list. Build enclosure when we reached our function's end *)
      match commandList with
      | FUN(name, param) :: tail -> if(funScopeCt = 0) then newCommands else buildFunCommands tail  (funScopeCt + 1) ( (FUN(name,param) )::newCommands)
      | FUNEND :: tail -> if(funScopeCt = 1) then newCommands else buildFunCommands tail (funScopeCt - 1) (FUNEND::newCommands) (* Prevents outmost funend from being added*)
      | command :: tail -> if(funScopeCt = 0) then newCommands else buildFunCommands tail funScopeCt (command :: newCommands)
      | [] ->  []

    let rec prettyPrint (stack : constant list) =
      match stack with
      |[] ->  Printf.printf "------------END OF STACK-------------\n";
      |Int a :: rest -> Printf.printf "%d\n" a; prettyPrint rest
      |Error a :: rest -> Printf.printf "%s\n" a; prettyPrint rest
      |Unit a :: rest -> Printf.printf "%s\n" a; prettyPrint rest
      |Bool a :: rest -> Printf.printf "%s\n" a; prettyPrint rest
      |String a :: rest -> Printf.printf "%s\n" a; prettyPrint rest
      |Name a :: rest -> Printf.printf "%s\n" a; prettyPrint rest
      |Closure _ :: rest -> Printf.printf "closure\n"; prettyPrint rest
  


    let rec getClosure (m : environment list) (target : constant) : ((constant * command list) * environment list * bool)  =
      (* Returns closure *)
      match m with
    |[] -> ((errorState,[]),[],false)
    |Binding(Closure((a,d),c, q), name)::b -> if( name = target) then ((a,d),c,q) else getClosure b target
    |Binding(_,name) ::b -> getClosure b target


    let insertResult (resultHolder : constant list) (mainStack : constant list) =
      match resultHolder with
      [] -> mainStack
      |result :: tail -> result::mainStack
  
    let trimStack (stack : constant list) =
     (* Takes a stack and removes the top two elements, used when calling a function. *)
      match stack with
      [] -> []
      |a::b::tail -> tail
      | _ -> errorState :: stack

    let evalFun (stack : constant list) (m : environment list) : constant*bool*constant list* ((constant * command list) * environment list * bool) = 
        (* This function takes a stack and an environment list and returns a closure as well as a flag stating if the function 
        returns anything. Can and should be refactored to not rely on so much state *)
      match stack with
      |[] -> errorState,false,errorState :: stack, ((errorState,[]),[], false)
      |Int a :: Name b :: tail ->  (*Printf.printf "Addition Matched %s\n" b;*) if((isType "closure" (findVar m (Name b) )) ) then  
        let ((const,clist),memory, inOut) = (getClosure m (Name b)) in
        let trimmedStack = trimStack stack in
        Int a,true,trimmedStack,((const,clist), Binding(Int a, const):: memory, inOut)  else errorState,false, (errorState :: stack), ((errorState,[]),[],false)

      |Int a :: Closure ((const,clist),memory, inOut) :: tail -> 
      let trimmedStack = trimStack stack in
      (Int a,true,trimmedStack,((const,clist), (Binding(Int a, const)):: memory, inOut)) 

      |String a :: Name b :: tail -> if( isType "closure" (findVar m (Name b) ) ) then 
        let ((const,clist),memory, inOut) = (getClosure m (Name b)) in
        let trimmedStack = trimStack stack in
        String a,true,trimmedStack,((const,clist), Binding(String a, const):: memory, inOut) else errorState,false,errorState :: stack, ((errorState,[]),[],false)
      
      |String a :: Closure ((const,clist),memory, inOut) :: tail -> 
      let trimmedStack = trimStack stack in 
      (String a,true,trimmedStack,((const,clist), (Binding(String a, const)):: memory, inOut)) 

      |Bool a :: Name b :: tail -> if( isType "closure" (findVar m (Name b) ) ) then 
        let ((const,clist),memory,inOut) = (getClosure m (Name b)) in
        let trimmedStack = trimStack stack in
        Bool a,true,trimmedStack,((const,clist), Binding(Bool a, const):: memory, inOut) else errorState,false,errorState :: stack, ((errorState,[]),[],false)

      |Bool a :: Closure ((const,clist),memory, inOut) :: tail -> 
      let trimmedStack = trimStack stack in 
      (Bool a,true,trimmedStack,((const,clist), (Binding(Bool a, const)):: memory, inOut)) 

      |Unit a :: Name b :: tail -> if( isType "closure" (findVar m (Name b)) ) then 
      let ((const,clist),memory,inOut) = (getClosure m (Name b)) in
      let trimmedStack = trimStack stack in
        Unit a,true,trimmedStack,((const,clist), Binding(Unit a, const):: memory, inOut) else errorState,false,errorState :: stack, ((errorState,[]),[], false)


      |Unit a :: Closure ((const,clist),memory, inOut) :: tail -> 
      let trimmedStack = trimStack stack in 
      (Unit a,true,trimmedStack,((const,clist), (Binding(Unit a, const)):: memory, inOut)) 

      |Name a :: Name b :: tail -> if( (not (isType "error" (findVar m (Name a) ) ) ) && isType "closure"(findVar m (Name b) ) ) then 
      let ((const,clist),memory, inOut) = (getClosure m (Name b)) in
      let trimmedStack = trimStack stack in (* findVar memory or m?*)
        Name a,true,trimmedStack,((const,clist), Binding((findVar m (Name a) ), const):: memory, inOut) else errorState,false,errorState :: stack, ((errorState,[]),[],false)

      |Name a :: Closure ((const,clist),memory, inOut) :: tail -> if( (not (isType "error" (findVar m (Name a) ) ) )) then
      let trimmedStack = trimStack stack in 
      (Name a,true,trimmedStack,((const,clist), (Binding((findVar m (Name a) ), const)):: memory, inOut)) else errorState,false,errorState :: stack, ((errorState,[]),[],false)


      | _ -> errorState,false,errorState :: stack, ((errorState,[]),[],false)


let interpreter ((input: string), (output: string)) : unit =
    let ic = open_in input in
    let oc = open_out output in

    let rec loop_read acc =
      try 
          let l = String.trim(input_line ic) in loop_read (l::acc)
      with
      | End_of_file -> List.rev acc in
    
    
    let processFile string_val = Printf.fprintf oc "%s\n" string_val  in

    let printValue (stack : constant list)  =
        match stack with
        |[] -> errorState::stack
        |String a::b -> processFile a; b
        |a::b -> errorState::stack
        in


    let rec processCommands (commandList : command list) (stack : constant list ) (m : environment list)  = 
    
    (* This function takes a command stack, constant stack and environment and processes the commands in the list
    which was discovered earlier.*)
    (*Printf.printf "Size of Stack: %d\n" (List.length stack); *)

    match commandList with
      | [] -> [], m
      |firstCommand :: tail->
        match firstCommand with
        |Push v -> Printf.printf "EXECUTE Push\n"; prettyPrint stack; if( false) then processCommands tail ((findVar m v)::stack) m else processCommands tail (v::stack) m
        |Add ->  processCommands tail (addValues stack m) m
        |Sub -> processCommands tail (subValues stack m) m
        |Mul -> processCommands tail (mulValues stack m) m
        |Div -> processCommands tail (divValues stack m) m
        |Rem -> processCommands tail (remValues stack m) m
        |Neg -> processCommands tail (negValue stack m) m
        |Swap -> prettyPrint stack;processCommands tail (swapValues stack) m
        |Pop -> prettyPrint stack;processCommands tail (popStack stack) m
        |ToString -> processCommands tail (toStringValues stack m) m
        |PrintLn -> processCommands tail (printValue stack) m
        |Quit -> processCommands tail stack m
        |Bind -> processCommands tail (bindForStack stack m) (bindForMemory stack m)
        |Cat -> processCommands tail (cat stack m) m
        |And -> processCommands tail (andOP stack m) m
        |Or -> processCommands tail (orOP stack m) m
        |Not -> processCommands tail (notValue stack m) m
        |Equal -> processCommands tail (equality stack m) m
        |LessThan -> processCommands tail (lessThan stack m) m
        |If -> processCommands tail (ifStatement stack m) m
        |Let -> let (returnedStack, letMemory) = (processCommands tail [] m) in processCommands (clearScopes tail 1) ((List.hd returnedStack) :: stack) m        
        |End -> (List.hd stack :: []), m
        |FUN (funName, formParam) -> if(funName != formParam) then processCommands (clearFunctions tail 1) (unitState::stack) (Binding( (Closure( (formParam, (buildFunCommands tail 1 [] ) ), m, false)), funName) :: m) 
        else processCommands tail (errorState::stack) m
        |FUNEND -> processCommands tail stack m
        |CALL -> 
        let (actualParam,proceed,trimmedStack,((const,clist),memory, inOut)) = (evalFun stack m) in
        if(proceed = true) then let incomingStack, funcMemory = (processCommands (List.rev clist) [] memory) in 
        
        if(List.length incomingStack = 0 && inOut = true) then processCommands tail trimmedStack (Binding( (findVar funcMemory (const) ), actualParam)::m) else 
        
        if(List.length incomingStack = 1) then if(inOut = true) then processCommands tail (List.hd incomingStack :: trimmedStack) (Binding( (findVar funcMemory (const) ), actualParam)::m) else
        processCommands tail (List.hd incomingStack :: trimmedStack ) m 
        
        else processCommands tail trimmedStack m 
        
        else processCommands tail (errorState::stack) m
        (* If we can proceed, run the function. If the function returned with a head in the list, then add it to our stack. Else, it returned nothing then continue on. 
        If we cannot proceed, then push an error onto the stack.*)
        (*When we execute call, we get back a stack. *)

        (*I am changing the return op and push op to check if its a name in memory. If so, use the memory if not just push it *)
        |RETURN ->  if( isType "name" (List.hd stack) && (checkMemory m (List.hd stack)) ) then (findVar m (List.hd stack))::[], m else ((List.hd stack)::[]), m
        |INOUTFUN (funName, formParam) -> if(funName != formParam) then processCommands (clearFunctions tail 1) (unitState::stack) (Binding( (Closure( (formParam, (buildFunCommands tail 1 [] ) ), m, true)), funName) :: m) 
        else processCommands tail (errorState::stack) m
        
      in


     let rawCommandList = loop_read [] in 

     let envStack = [] in

     let commandList = findCommands rawCommandList [] in
     let valueStack = [] in 

     ignore(processCommands commandList valueStack envStack);;

    (* To run the interpreter, it is called with the file path of the input commands and the
    file path of where the output file should go. *)
    interpreter("input/input17.txt","output/myoutput17.txt");;