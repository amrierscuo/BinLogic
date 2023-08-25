type boolean =
  | One
  | Zero
  | Not of boolean
  | And of boolean * boolean
  | Or of boolean * boolean
  | Implies of boolean * boolean
  | Equiv of boolean * boolean;;

let rec eval b = 
  match b with
  | One -> true
  | Zero -> false
  | Not(a) -> not (eval a)
  | And(a, b) -> (eval a) && (eval b)
  | Or(a, b) -> (eval a) || (eval b)
  | Implies(a, b) -> (not (eval a)) || (eval b)
  | Equiv(a, b) -> (eval a) = (eval b)

let rec input_expression () =
  print_endline "Enter your boolean expression (One, Zero, Not One, And One Zero, Or One Zero, Implies One Zero, Equiv One Zero): ";
  let input = read_line () in
  (* Simplified parsing *)
  match input with
  | "One" -> One
  | "Zero" -> Zero
  | "Not One" -> Not One
  | "Not Zero" -> Not Zero
  | "And One Zero" -> And (One, Zero)
  | "Or One Zero" -> Or (One, Zero)
  | "Implies One Zero" -> Implies (One, Zero)
  | "Equiv One Zero" -> Equiv (One, Zero)
  | "exit" -> exit 0
  | _ -> print_endline "Invalid input. Try again."; input_expression ()

let main () =
  print_endline "Boolean Evaluator Tool";
  print_endline "Type 'exit' to quit.";
  let rec loop () =
    let expr = input_expression () in
    print_endline ("Result: " ^ (string_of_bool (eval expr)));
    loop ()
  in
  loop ()

let _ = main ()

(*
   HOW IT WORKS:
   This tool lets you input basic boolean expressions and evaluates them.
   The supported expressions are One, Zero, Not One, Not Zero, And One Zero, Or One Zero, Implies One Zero, and Equiv One Zero.
   
   HOW TO RUN:
   1. Save the above code to a file, for instance, "bool_tool.ml".
   2. In the terminal, navigate to the directory containing "bool_tool.ml".
   3. Run `ocaml bool_tool.ml` to start the interactive tool.
   4. Enter your boolean expressions as prompted.
   5. Type 'exit' to quit the program.

   EXAMPLES:
   Input: "One"
   Output: "Result: true"

   Input: "And One Zero"
   Output: "Result: false"

   Input: "Or One Zero"
   Output: "Result: true"

   Input: "Not One"
   Output: "Result: false"
a@t:~$ #use "bool_tool.ml";;
a@t:~$ ocamlc -o bool_tool bool_tool.ml
a@t:~$ ./bool_tool
Boolean Evaluator Tool
Type 'exit' to quit.
Enter your boolean expression (One, Zero, Not One, And One Zero, Or One Zero, Implies One Zero, Equiv One Zero):
Zero
Result: false
Enter your boolean expression (One, Zero, Not One, And One Zero, Or One Zero, Implies One Zero, Equiv One Zero):
Implies One Zero
Result: false
Enter your boolean expression (One, Zero, Not One, And One Zero, Or One Zero, Implies One Zero, Equiv One Zero):
Not One
Result: false
Enter your boolean expression (One, Zero, Not One, And One Zero, Or One Zero, Implies One Zero, Equiv One Zero):
One
Result: true
Enter your boolean expression (One, Zero, Not One, And One Zero, Or One Zero, Implies One Zero, Equiv One Zero):
*)