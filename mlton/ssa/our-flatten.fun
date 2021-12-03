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

      (* Flatten tuples and store info*)
      val functions =
         List.revMap
         (functions, fn f =>
          let
             val {args, blocks, mayInline, name, raises, returns, start} =
                Function.dest f
             val _ = print f.layout

             fun getElements(x: Var.t): = 
                (*return the elements in a recorded tuple*)

             fun foo (x:Type.t): (Var.t * Type.t) list = 
               (* flat x, add new vars into record. *)
                 

             fun flat (x: Var.t * Type.t): (Var.t * Type.t) list =    
                let 
                   fun checkType (ts: Type.t vector) acc: = 
                      case Vector.toList ts of 
                         hd ::tl => checkType tl (case hd of
                                          | Tuple => (checkType hd [])@acc
                                          | _ => (foo hd)::acc ) 
                       | [] => acc
                in
                   if List.exists (record, fn #1 x => true) then
                      getElements #1 x
                   else
                      checkType #2 x []

             fun forces (xs: (Var.t * Type.t) vector): (Var.t * Type.t) vector =
                let
                   fun newL ls acc : =
                      case ls of 
                          hd ::tl => newL tl (case #2 hd of
                                           | Tuple => (flat hd)@acc
                                           | _ => hd ::acc ) 
                        | [] => List.rev acc
                in 
                   Vector.fromList(newL Vector.toList(xs) [])

             fun assNew (xs: Var.t vector): =
               (* returns a vector of new statements*)
                  

             val newArgs = forces args

             fun visit (Block.T {args, statements, transfer, ...}): unit -> unit =
                let
                   val newA = forces args
                   val newS = 
                      Vector.map
                      (statements, fn Statement.T {var, ty, exp} =>
                       case exp of
                          Select {tuple, offset} => Vector.new1 Select {tuple, offset} (*change it later*)
                        | Tuple args => Vector.concat [Vector.new1 Statement.T {var, ty, Tuple args}, assNew args]
                        | _ => Vector.new1 Statement.T {var, ty, exp})
                in
                   Block.T {label = label,
                             args = newA,
                             statements = Vector.concatV newS,
                             transfer = transfer}
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

      (*Flattening transfers*)    
      val functions =
         List.revMap
         (functions, fn f =>
          let
             val {args, blocks, mayInline, name, raises, returns, start} =
                Function.dest f
             val _ = print f.layout

             fun checkFlatten (x: Var.t): =
                if List.exists (record, fn x => true) then
                   getElements x
                else 
                   [x]
             fun force (xs: Var.t vector): =
                Vector.concat (Vector.map(xs, checkFlatten))

             fun visit (Block.T {args, statements, transfer, ...}): unit -> unit =
                let
                   val newT =
                      case transfer of
                         Call {args, ...} => Call{force args, ...} (*will the other parts passed to the new call?*)
                       | Goto {dst, args} => Goto {dst, force args}
                       | Runtime {args, ...} => Runtime {force args, ...}
                       | _ => transfer
                in
                   Block.T {label = label,
                             args = args,
                             statements = statements,
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
