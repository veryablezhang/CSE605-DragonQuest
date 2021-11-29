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
               

             fun flat (xs: Var.t ): unit =
               case xs of
                  Tuple => 
                     (* flat x, add new vars into record. *)
                     foo xs   
                  
             (* for a in args *)      
             fun forces (xs: (Var.t * Type.t) vector): unit =
                let
                  fun newL xs acc=
                     case xs of 
                        hd ::tl => newL tl (case #2 hd of
                                          | Tuple => flat #2 hd :: acc
                                          | _ => hd ::acc ) 
                        | [] -> List.rev acc
                in 
                   newL xs []

             val _ = print (Var.Layout (#1 xs))
             val newArgs = forces args

             fun visit (Block.T {args, statements, transfer, ...}): unit -> unit =
                let
                   val newA = forces args
                   val newS = 
                      Vector.foreach
                      (statements, fn Statement.T {var, exp, ...} =>
                       case exp of
                          ConApp {args, ...} => ConApp {forces args, ...}
                        | PrimApp {args, ...} => PrimApp {forces args, ...}
                        | Tuple args => forces args (* not sure*)
                        | _ => exp)
                   val newT =
                      case transfer of
                         Call {args, ...} => Call{forces args, ...}
                       | Goto {dst, args} => Goto {dst, forces args}
                       | Runtime {args, ...} => Runtime {forces args, ...}
                       | _ => transfer
                in
                   Block.T {label = label,
                             args = newA,
                             statements = newS,
                             transfer = newT}
                end
             val _ = Function.dfs (f, visit) (* Could the return value be the new blocks? *)
             val newBlocks = Vector.map (blocks,visit)
          in
             shrink (Function.new {args = newArgs,
                                   blocks = newBlocks,
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
