functor OurFlatten (S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM = 
struct

open S
open Exp Transfer


fun transform (Program.T {globals, datatypes, functions, main}) =
   let
      val _ = print("Our flatten starts\n")
      val shrink = shrinkFunction {globals = globals}
      val functions =
         List.revMap
         (functions, fn f =>
          let
             val {args, blocks, mayInline, name, raises, returns, start} =
                Function.dest f
             val _ = print("Function\n")
             fun flat (x: Var.t): unit =
                
                
                
             fun forces (xs: Var.t vector): unit =
                Vector.foreach (xs, flat)
             val _ = forces args
             fun visit (Block.T {statements, transfer, ...}): unit -> unit =
                let
                   val _ = print("Block\n")
                   val _ = 
                      Vector.foreach
                      (statements, fn Statement.T {var, exp, ...} =>
                       case exp of
                          ConApp {args, ...} => ()
                        | PrimApp {args, ...} => ()
                        | Tuple args => ()
                        | Var x => ()
                        | _ => ())
                   val _ =
                      case transfer of
                         Bug => ()
                       | Call {args, return, ...} => ()
                       | Case {cases, default, ...} => ()
                       | Goto {dst, args} => ()
                       | Raise xs => ()
                       | Return xs => ()
                       | Runtime {args, return, ...} => ()
                in
                   fn () => ()
                end
             val _ = Function.dfs (f, visit)
             val blocks =
                Vector.map
                (blocks, fn Block.T {label, args, statements, transfer} =>
                 let
                    
                    
                    
                    
                 in
                    Block.T {label = label,
                             args = args,
                             statements = statements,
                             transfer = transfer}
                 end)
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
