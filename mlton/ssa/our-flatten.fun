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
      datatype t = T of {name: Var.t,
                         elements: (Var.t * Type.t) list}
      end
   

fun transform (Program.T {globals, datatypes, functions, main}) =
   let
      val record = ref [] (* store newly created vars. *)
      
      fun getE (Flatters.T {name, elements}) = elements

      fun getElements(x: Var.t) = 
                (*return the elements in a recorded tuple*)
          let
              val i = case (List.index (!record, (fn Flatters.T{name, ...} => (Var.equals (name, x))))) of
                  NONE => 0 (*not gonna happen*)
                | SOME ind => ind
          in
              getE (List.nth (!record, i))
          end
           
          
      val _ = print("Our flatten starts\n")
	
      val shrink = shrinkFunction {globals = globals}

      (* Flatten tuples and store info*)
      val functions =
         List.revMap
         (functions, fn f =>
          let
             val {args, blocks, mayInline, name, raises, returns, start} =
                Function.dest f
                
             (* fun foo (x: Var.t, t: Type.t): (Var.t * Type.t) =  *)

             fun flat (x: Var.t * Type.t): (Var.t * Type.t) list =    
                let 
                   fun checkType (ts: Type.t list) acc = 
                      case ts of 
                         hd ::tl => checkType tl (if Type.isTuple hd then
                                            	        (checkType (Vector.toList (Type.deTuple hd)) [])@acc
                                                  else ((Var.newNoname(), hd)::acc ))
                       | [] => acc
                   val flattened = 
                   	if List.exists (!record, fn Flatters.T {name, ...} => Var.equals (name, #1 x)) then
                      	    getElements (#1 x)
                       else
                           checkType (Vector.toList (Type.deTuple (#2 x))) []
                   val _ = 
                      if List.exists (!record, fn Flatters.T {name, ...} => Var.equals (name, #1 x)) then
                      	    ()
                       else
                           record:= Flatters.T{name = (#1 x), elements = flattened}::(!record)
                in
                   flattened
                end

             fun forces (xs: (Var.t * Type.t) vector): (Var.t * Type.t) vector =
                let
                   fun newL ls acc =
                      case ls of
                          hd ::tl => newL tl (if Type.isTuple (#2 hd) then
                                                  hd :: ((flat hd)@acc)
                                              else hd ::acc )
                        | [] => List.rev acc
                in 
                   Vector.fromList (newL (Vector.toList xs) [])
                end

             fun assignNew (x: Var.t option, xs: Var.t vector, t: Type.t) =
               (* returns a list of new statements*)
                let
                   val ts = Vector.toList (Type.deTuple t)
                   val tname = 
                      case x of
                         NONE => Var.newNoname()
                       | SOME tuplename => tuplename
                   fun assign names types offset acc = 
                      case (names, types) of
                         (name::ntl, tt::ttl) =>  assign ntl ttl (offset + 1) 
                                                  (Statement.T {var = (SOME name), ty = tt, exp = Select {tuple = tname, offset = offset}}::acc)
                        | _ => acc
                in 
                   assign (Vector.toList xs) ts 1 []
                end

             val newArgs = forces args
             
             fun visit (Block.T {args, statements, transfer, label}) =
                let
                   val newA = forces args
                   val newS = 
                      Vector.map
                      (statements, fn Statement.T {var, ty, exp} =>
                       case exp of
                          Select {tuple, offset} => Vector.new1 (Statement.T {var = var, ty = ty, exp = (Select {tuple = tuple, offset = offset}) }) (*change it later*)
                        | Tuple args => Vector.fromList ((Statement.T {var = var, ty = ty, exp = Tuple args}):: 
                                                       (assignNew (var, args, ty)))
                        | _ => Vector.new1 (Statement.T {var = var, ty = ty, exp = exp}))
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
      
      val _ = print "-------------------------------2nd run"
      (*Flattening transfers*)    
      val functions =
         List.revMap
         (functions, fn f =>
          let
             val {args, blocks, mayInline, name, raises, returns, start} =
                Function.dest f

             fun checkFlatten (x: Var.t) =
                if List.exists (!record, fn Flatters.T {name, ...} => Var.equals (name, x)) then
                   Vector.fromList (#1 (List.unzip (getElements x)))
                else 
                   Vector.new1 x
             fun force (xs: Var.t vector) =
                Vector.concatV (Vector.map(xs, checkFlatten))

             fun visit (Block.T {args, statements, transfer, label}) =
                let
                   val newT =
                      case transfer of
                         Call {args, func, return} => Call {args = force args, func = func, return = return} (*will the other parts passed to the new call?*)
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
             shrink (Function.new {args = args,
                                   blocks = newBlocks,
                                   mayInline = mayInline,
                                   name = name,
                                   raises = raises,
                                   returns = returns,
                                   start = start})
          end)
      val _ = print("Our flatten ends\n")
      val program = Program.T {datatypes = datatypes,
                               globals = globals,
                               functions = functions, 
                               main = main}
      val _ = Program.clearTop program
   in
      program
   end
end
