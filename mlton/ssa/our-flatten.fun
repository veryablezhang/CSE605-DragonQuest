functor OurFlatten (S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM = 
struct

open S
open Exp Transfer


   structure Flatters = 
      struct
      (*
         Record the information of a newly flattened tuple by our routine.
         e.g. if we flat tuple ``bar'' and have
         foo = #2 
         then we create a flatter for foo, where
            name = ``foo'',
            parent = ``bar'',
            index = 2,
            isTuple = if foo is tuple, something like that.
            Therefore anytime we see #2 bar later we can just sub it w/ foo.
            May has a scope problem...
      *)
      datatype t = T of {val name: Var.t,
                         val elements: (Var.t * Type.t) list}
      end
   

fun transform (Program.T {globals, datatypes, functions, main}) =
   let
      val record = ref [] (* store newly created vars. *)
      val _ = print("Our flatten starts\n")

      val shrink = shrinkFunction {globals = globals}

      (* Flatten tuples and store info*)
      val functions =
         List.revMap
         (functions, fn f =>
          let
             val {argss, blocks, mayInline, name, raises, returns, start} =
                Function.dest f
             val _ = print f.layout
            
             fun getE (Flatters.T {name, elements}) = elements

             fun getElements(x: Var.t) = 
                (*return the elements in a recorded tuple*)
                getE (List.nth List.index(!record fn Flatters.T{name, ...} => name = x)) 
                
             (* fun foo (x: Var.t, t: Type.t): (Var.t * Type.t) =  *)

             fun flat (x: Var.t * Type.t): (Var.t * Type.t) list =    
                let 
                   fun checkType (ts: Type.t vector) acc = 
                      case Vector.toList ts of 
                         hd ::tl => checkType tl (case hd of
                                          | Tuple => (checkType hd [])@acc
                                          | _ => ((Var.newNoname(), hd)::acc ))
                       | [] => acc
                   if List.exists (!record, fn Flatters.T {name, ...} => name = #1 x) then
                      val flattened = getElements #1 x
                   else
                      val flattened = checkType #2 x []
                      record:= List.append(Flatters.T{name = #1 x, elements = flattened})
                in
                   flattened
                end

             fun forces (xs: (Var.t * Type.t) vector): (Var.t * Type.t) vector =
                let
                   fun newL ls acc =
                      case ls of
                          hd ::tl => newL tl (case #2 hd of
                                           | Tuple => hd :: ((flat hd)@acc)
                                           | _ => hd ::acc )
                        | [] => List.rev acc
                in 
                   Vector.fromList(newL Vector.toList(xs) [])
                end

             fun assignNew (x: Var.t, xs: Var.t vector, t: Type.t) =
               (* returns a vector of new statements*)
                let
                   val ts = Type.dest t
                   fun assign (names, types, offset, acc) = 
                      case (names, types) of
                         (name::ntl, type::ttl) => assign ntl ttl (offset + 1) 
                                                      Statement.T {var = name, ty = type, exp = Select {tuple = x, offset = offset}}::acc
                        | _ => acc
                in
                   Vector.fromList (assign xs ts 1 [])
                end

             val newArgs = forces args
             
             fun visit (Block.T {args, statements, transfer, ...}): unit -> unit =
                let
                   val newA = forces args
                   val newS = 
                      Vector.map
                      (statements, fn Statement.T {var, ty, exp} =>
                       case exp of
                          Select {tuple, offset} => Vector.new1 Select {tuple = tuple, offset = offset} (*change it later*)
                        | Tuple args => Vector.concat [Vector.new1 Statement.T {var = var, ty = ty, exp = Tuple args}, 
                                                       assignNew var args ty]
                        | _ => Vector.new1 Statement.T {var = var, ty = ty, exp = exp})
                in
                   Block.T {label = label,
                             args = newA,
                             statements = Vector.concatV newS,
                             transfer = transfer}
                end
             val newBlocks = Vector.map (blocks, visit)
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

             fun checkFlatten (x: Var.t) =
                if List.exists (!record, fn Flatters.T {name, ...} => name = x) then
                   #1 List.unzip (getElements x)
                else 
                   [x]
             fun force (xs: Var.t vector) =
                Vector.concat (Vector.map(xs, checkFlatten))

             fun visit (Block.T {args, statements, transfer, ...}): unit -> unit =
                let
                   val newT =
                      case transfer of
                         Call {args, func, return} => Call{args = force args, func = func, retrun = return} (*will the other parts passed to the new call?*)
                       | Goto {dst, args} => Goto {dst = dst, args = force args}
                       | _ => transfer
                in
                   Block.T {label = label,
                             args = args,
                             statements = statements,
                             transfer = newT}
                end
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
