functor OurFlatten (S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM = 
struct

open S
open Exp Transfer

fun transform (Program.T {globals, datatypes, functions, main}) =
   let
      val _ = print("test our flatten\n")
      val shrink = shrinkFunction {globals = globals}
      val functions =
         List.revMap
         (functions, fn f =>
          let
             val {args, blocks, mayInline, name, raises, returns, start} =
                Function.dest f
             val _ = print("Function\n")
             fun visit (Block.T {statements, transfer, ...}): unit -> unit =
                let
                   val _ = print("Block\n")
                   val _ = 
                      Vector.foreach
                      (statements, fn Statement.T {var, exp, ...} =>
                       case exp of
                          ConApp {args, ...} => print("ConApp")
                        | PrimApp {args, ...} => print("PrimApp")
                        | Tuple args => print("Tuple")
                        | Var x => print("Var")
                        | _ => print("Null"))
                   val _ =
                      case transfer of
                         Bug => print("Bug")
                       | Call {args, return, ...} => print("Call")
                       | Case {cases, default, ...} => print("Case")
                       | Goto {dst, args} => print("Goto")
                       | Raise xs => print("Raise")
                       | Return xs => print("Return")
                       | Runtime {args, return, ...} => print("Runtime")
                in
                   fn () => ()
                end
             val _ = Function.dfs (f, visit)
          in
             shrink (Function.new {args = args,
                                   blocks = blocks,
                                   mayInline = mayInline,
                                   name = name,
                                   raises = raises,
                                   returns = returns,
                                   start = start})
          end)
      val program = Program.T {datatypes = datatypes,
                               globals = globals,
                               functions = functions, 
                               main = main}
      val _ = Program.clearTop program
   in
      program
   end
end
