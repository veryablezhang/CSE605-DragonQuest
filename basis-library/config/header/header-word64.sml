(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure HeaderWord = Word64

functor HeaderWord_ChooseWordN (A: CHOOSE_WORDN_ARG) :
   sig val f : HeaderWord.word A.t end = 
   ChooseWordN_Word64 (A)
