functor OurFlatten (S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM = 
struct

open S
open Exp Transfer


   structure Flatters = 
      struct
      (*
         Record the information of a newly created variable by our routine.
         e.g. if we flat tuple ``bar'' and have
         foo = #2 bar
         then we create a flatter for foo, where
            name = ``foo'',
            parent = ``bar'',
            index = 2,
            isTuple = if foo is tuple, something like that.
            Therefore anytime we see #2 bar later we can just sub it w/ foo.
            May has a scope problem...
      *)
         val names: Var.t of List
         val parent: Var.t
         val len: int
         val isTuple: Bool
      end
   

fun transform (Program.T {globals, datatypes, functions, main}) =
   let
      val record = []: Flatters List (* store newly created vars. *)
      val _ = print("Our flatten starts\n")

      val shrink = shrinkFunction {globals = globals}

      (* transform functions here... *)
      val functions =
         List.revMap
         (functions, fn f =>
          let
             val {args, blocks, mayInline, name, raises, returns, start} =
                Function.dest f
             val _ = print("Function\n")

             fun foo (t: exp): unit = 
               
             (* Not exactly sure the def of Var... *)
             fun flat (xs: (Var.t * Type.t)): unit =
               case #2 xs of
                  Tuple => 
                     (* flat x, add new vars into record. *)
                     foo x

                     
                  
             (* for a in args *)      
             fun forces (xs: (Var.t * Type.t) vector): unit =
                Vector.foreach (xs, fn a => flat #1 a)

            val _ = print (Var.Layout (#1 xs))

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
                        | Tuple args => () (* do something here. *)
                        | Var x => ()
                        | _ => ())
                   val newT =
                      case transfer of
                         Bug => ()
                       | Call {args, return, ...} => Call{foo  args, return, ...}
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
