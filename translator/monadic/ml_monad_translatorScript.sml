open preamble ml_translatorTheory ml_translatorLib ml_pmatchTheory patternMatchesTheory
open astTheory libTheory bigStepTheory semanticPrimitivesTheory
open terminationTheory ml_progLib ml_progTheory
open set_sepTheory cfTheory cfStoreTheory cfTacticsLib Satisfy
open cfHeapsBaseTheory basisFunctionsLib AC_Sort
open determTheory

open ml_monadBaseTheory

val _ = new_theory "ml_monad_translator";

val _ = temp_overload_on ("monad_bind", ``st_ex_bind``);
val _ = temp_overload_on ("monad_unitbind", ``\x y. st_ex_bind x (\z. y)``);
val _ = temp_overload_on ("monad_ignore_bind", ``\x y. st_ex_bind x (\z. y)``);
val _ = temp_overload_on ("ex_bind", ``st_ex_bind``);
val _ = temp_overload_on ("ex_return", ``st_ex_return``);

val _ = temp_overload_on ("CONTAINER", ``ml_translator$CONTAINER``);

val _ = hide "state";

val HCOND_EXTRACT = cfLetAutoTheory.HCOND_EXTRACT;

(*********** Comes from cfLetAutoLib.sml ***********************************************)	 
(* [dest_pure_fact]
   Deconstruct a pure fact (a heap predicate of the form &P) *)
val set_sep_cond_tm = ``set_sep$cond : bool -> hprop``;
fun dest_pure_fact p =
  case (dest_term p) of
  COMB dp =>
    (if same_const set_sep_cond_tm (#1 dp) then (#2 dp)
    else raise (ERR "dest_pure_fact" "Not a pure fact"))
  | _ => raise (ERR "dest_pure_fact" "Not a pure fact");
(***************************************************************************************)

fun PURE_FACTS_FIRST_CONV H =
  let
      val preds = list_dest dest_star H
      val (pfl, hpl) = List.partition (can dest_pure_fact) preds
      val ordered_preds = pfl @ hpl
  in
      if List.null ordered_preds then REFL H
      else
	  let val H' = List.foldl (fn (x, y) => mk_star(y, x)) (List.hd ordered_preds)
				  (List.tl ordered_preds)
          (* For some strange reason, AC_CONV doesn't work *)
          val H_to_norm = STAR_AC_CONV H
	  val norm_to_H' = (SYM(STAR_AC_CONV H') handle UNCHANGED => REFL H')
	  in TRANS H_to_norm norm_to_H'
	  end
  end;

val EXTRACT_PURE_FACTS_CONV =
  (RATOR_CONV PURE_FACTS_FIRST_CONV)
  THENC (SIMP_CONV pure_ss [GSYM STAR_ASSOC])
  THENC (SIMP_CONV pure_ss [HCOND_EXTRACT])
  THENC (SIMP_CONV pure_ss [STAR_ASSOC]);

(* TODO: use EXTRACT_PURE_FACT_CONV to rewrite EXTRACT_PURE_FACTS_TAC *)
fun EXTRACT_PURE_FACTS_TAC (g as (asl, w)) =
  let
      fun is_hprop a = ((dest_comb a |> fst |> type_of) = ``:hprop`` handle HOL_ERR _ => false)
      val hpreds = List.filter is_hprop asl
      val hpreds' = List.map (fst o dest_comb) hpreds
      val hpreds_eqs = mapfilter (PURE_FACTS_FIRST_CONV) hpreds'
  in
      ((fs hpreds_eqs) >> fs[GSYM STAR_ASSOC] >> fs[HCOND_EXTRACT] >> fs[STAR_ASSOC]) g
  end;
(***********************************************************************************************)

val _ = temp_type_abbrev("state",``:'ffi semanticPrimitives$state``);

(* a few basics *)
val with_same_refs = Q.store_thm("with_same_refs",
  `(s with refs := s.refs) = s`,
  simp[state_component_equality])

val with_same_ffi = Q.store_thm("with_same_ffi",
  `(s with ffi := s.ffi) = s`,
  simp[state_component_equality]);

val with_same_clock = Q.store_thm("with_same_clock",
  `(s with clock := s.clock) = s`,
  simp[state_component_equality]);

(* REF_REL *)
val REF_REL_def = Define `REF_REL TYPE r x = SEP_EXISTS v. REF r v * &TYPE x v`;

(* REFS_PRED *)
val REFS_PRED_def = Define `REFS_PRED H refs p s = (H refs * GC) (st2heap p s)`;
val VALID_REFS_PRED_def = Define `VALID_REFS_PRED H = ?(s : unit state) p refs. REFS_PRED H refs p s`;

(* Frame rule for EvalM *)
val REFS_PRED_FRAME_def = Define
`REFS_PRED_FRAME H p (refs1, s1) (refs2, s2) =
!F. (H refs1 * F) (st2heap p s1) ==> (H refs2 * F * GC) (st2heap p s2)`;

val EMP_STAR_H = Q.store_thm("EMP_STAR_GC",
`!H. emp * H = H`,
fs[STAR_def, emp_def, SPLIT_def, ETA_THM]);

val SAT_GC = Q.store_thm("SAT_GC",
`!h. GC h`,
fs[GC_def, SEP_EXISTS_THM] \\ STRIP_TAC \\ qexists_tac `\s. T` \\ fs[]);

val REFS_PRED_FRAME_imp = Q.store_thm("REFS_PRED_FRAME_imp",
`REFS_PRED H refs1 p s1 ==> REFS_PRED_FRAME H p (refs1, s1) (refs2, s2) ==> REFS_PRED H refs2 p s2`,
rw[REFS_PRED_def, REFS_PRED_FRAME_def]
\\ Cases_on `p`
\\ fs[st2heap_def]
\\ metis_tac[GC_STAR_GC, STAR_ASSOC]);

val REFS_PRED_FRAME_trans = Q.store_thm("REFS_PRED_FRAME_trans",
`REFS_PRED_FRAME H p (refs1, s1) (refs2, s2) ==>
REFS_PRED_FRAME H p (refs2, s2) (refs3, s3) ==>
REFS_PRED_FRAME H p (refs1, s1) (refs3, s3)`,
rw[REFS_PRED_FRAME_def] >>
PURE_REWRITE_TAC[Once (GSYM GC_STAR_GC), STAR_ASSOC] >>
`H refs3 * F' * GC * GC = H refs3 * (F' * GC) * GC` by fs[STAR_ASSOC] >>
POP_ASSUM (fn x => PURE_REWRITE_TAC[x]) >>
first_x_assum irule >>
fs[STAR_ASSOC]);


(*
 * Proof of REFS_PRED_APPEND:
 * `REFS_PRED H refs s ==> REFS_PRED H refs (s with refs := s.refs ++ junk)`
 *)
val store2heap_aux_Mem = Q.store_thm("store2heap_aux_Mem",
`!s n x. x IN (store2heap_aux n s) ==> ?n' v. x = Mem n' v`,
Induct_on `s`
 >-(rw[IN_DEF, store2heap_def, store2heap_aux_def]) >>
rw[] >> fs[IN_DEF, store2heap_def, store2heap_aux_def] >>
last_x_assum IMP_RES_TAC >>
fs[]);

val store2heap_aux_IN_LENGTH = Q.store_thm ("store2heap_aux_IN_LENGTH",
`!s r x n. Mem r x IN (store2heap_aux n s) ==> r < n + LENGTH s`,
Induct THENL [all_tac, Cases] \\
fs [store2heap_aux_def] \\
Cases_on `r` \\ fs [] \\ rewrite_tac [ONE] \\
rpt strip_tac \\ fs[ADD_CLAUSES, GSYM store2heap_aux_suc] \\
metis_tac[]
);

val NEG_DISJ_TO_IMP = Q.prove(
`!A B. ~A \/ ~B <=> A /\ B ==> F`,
rw[]);

val store2heap_aux_DISJOINT = Q.store_thm("store2heap_aux_DISJOINT",
`!n s1 s2. DISJOINT (store2heap_aux n s1) (store2heap_aux (n + LENGTH s1) s2)`,
rw[DISJOINT_DEF, INTER_DEF, EMPTY_DEF] >>
fs[GSPECIFICATION_applied] >>
sg `!x. {x | x ∈ store2heap_aux n s1 ∧ x ∈ store2heap_aux (n + LENGTH s1) s2} x = (\x. F) x`
>-(
    rw[] >>
    PURE_REWRITE_TAC[NEG_DISJ_TO_IMP] >>
    DISCH_TAC >> rw[] >>
    IMP_RES_TAC store2heap_aux_Mem >>
    rw[] >>
    IMP_RES_TAC store2heap_aux_IN_bound >>
    IMP_RES_TAC store2heap_aux_IN_LENGTH >>
    bossLib.DECIDE_TAC
) >>
POP_ASSUM (fn x => ASSUME_TAC (EXT x)) >> fs[]);

val store2heap_aux_SPLIT = Q.store_thm("store2heap_aux_SPLIT",
`!s1 s2 n. SPLIT (store2heap_aux n (s1 ++ s2)) (store2heap_aux n s1, store2heap_aux (n + LENGTH s1) s2)`,
fs[SPLIT_def] >> fs[store2heap_aux_append_many] >>
metis_tac[UNION_COMM, store2heap_aux_append_many, store2heap_aux_DISJOINT]);

val store2heap_DISJOINT = Q.store_thm("store2heap_DISJOINT",
`DISJOINT (store2heap s1) (store2heap_aux (LENGTH s1) s2)`,
fs[store2heap_def] >> metis_tac[store2heap_aux_DISJOINT, arithmeticTheory.ADD]);

(* If the goal is: (\x. P x) = (\x. Q x), applies SUFF_TAC ``!x. P x = Q x`` *)
fun SUFF_ABS_TAC (g as (asl, w)) =
  let
      val (e1, e2) = dest_eq w
      val (x1, e1') = dest_abs e1
      val (x2, e2') = dest_abs e2
      val _ = if x1 <> x2 then failwith "" else ()
      val w' = mk_forall(x1,  mk_eq(e1', e2'))
  in
      (SUFF_TAC w' THEN rw[]) g
  end;

val store2heap_SPLIT = Q.store_thm("store2heap_SPLIT",
`!s1 s2. SPLIT (store2heap (s1 ++ s2)) (store2heap s1, store2heap_aux (LENGTH s1) s2)`,
fs[store2heap_def] >> metis_tac[store2heap_aux_SPLIT, arithmeticTheory.ADD]);

val SPLIT_DECOMPOSWAP = Q.store_thm("SPLIT_DECOMPOSWAP",
`SPLIT s1 (s2, s3) ==> SPLIT s2 (u, v) ==> SPLIT s1 (u, v UNION s3)`,
fs[SPLIT_def, UNION_ASSOC, DISJOINT_SYM] >> rw[] >> fs[DISJOINT_SYM, DISJOINT_UNION_BOTH]);

val STORE_APPEND_JUNK = Q.store_thm("STORE_APPEND_JUNK",
`!H s junk. H (store2heap s) ==> (H * GC) (store2heap (s ++ junk))`,
rw[] >>
qspecl_then [`s`, `junk`] ASSUME_TAC store2heap_SPLIT >>
fs[STAR_def] >>
qexists_tac `store2heap s` >>
qexists_tac `store2heap_aux (LENGTH s) junk` >>
`!H. GC H` by (rw[cfHeapsBaseTheory.GC_def, SEP_EXISTS] >> qexists_tac `\x. T` >> fs[]) >>
POP_ASSUM (fn x => fs[x]));

val st2heap_SPLIT_FFI = Q.store_thm("st2heap_SPLIT_FFI",
`!f st. SPLIT ((store2heap st.refs) UNION (ffi2heap f st.ffi)) (store2heap st.refs, ffi2heap f st.ffi)`,
rw[SPLIT_def]
\\ fs[IN_DISJOINT]
\\ STRIP_TAC
\\ PURE_REWRITE_TAC[NEG_DISJ_TO_IMP]
\\ STRIP_TAC
\\ rw[]
\\ fs[store2heap_def]
\\ Cases_on `x`
\\ fs[Mem_NOT_IN_ffi2heap, FFI_split_NOT_IN_store2heap_aux, FFI_full_NOT_IN_store2heap_aux, FFI_part_NOT_IN_store2heap_aux]);

val SPLIT3_swap12 = Q.store_thm("SPLIT3_swap12",
`!h h1 h2 h3. SPLIT3 h (h1, h2, h3) = SPLIT3 h (h2, h1, h3)`,
rw[SPLIT3_def, UNION_COMM, CONJ_COMM] >> metis_tac[DISJOINT_SYM]);

val SPLIT_of_SPLIT3_1u3 = Q.store_thm("SPLIT_of_SPLIT3_1u3",
`∀h h1 h2 h3. SPLIT3 h (h1,h2,h3) ⇒ SPLIT h (h2, h1 ∪ h3)`,
metis_tac[SPLIT3_swap12, SPLIT_of_SPLIT3_2u3]);

val SPLIT2_SPLIT3 = Q.store_thm("SPLIT2_SPLIT3",
`SPLIT s1 (s2, t3) /\ SPLIT s2 (t1, t2) ==> SPLIT3 s1 (t1, t2, t3)`,
rw[SPLIT_def] \\ fs[SPLIT3_def]);

val SPLIT_SYM = Q.store_thm("SPLIT_SYM",
`SPLIT s (s1, s2) = SPLIT s (s2, s1)`,
fs[SPLIT_def, DISJOINT_SYM, UNION_COMM]);

val STATE_APPEND_JUNK = Q.store_thm("STATE_APPEND_JUNK",
`!H p s refs junk. H (st2heap p (s with refs := refs)) ==>
(H * GC) (st2heap p (s with refs := refs ++ junk))`,
rw[]
\\ Cases_on `p`
\\ fs[st2heap_def]
\\ Q.PAT_ABBREV_TAC `h = A UNION B`
\\ sg `SPLIT3 h (store2heap refs, store2heap_aux (LENGTH refs) junk, ffi2heap (q,r) s.ffi)`
>-(
   fs[markerTheory.Abbrev_def] \\ rw[]
   \\ irule SPLIT2_SPLIT3
   \\ qexists_tac `store2heap (refs ++ junk)`
   \\ fs[store2heap_SPLIT, SPLIT_def, IN_DISJOINT, store2heap_def]
   \\ PURE_REWRITE_TAC[NEG_DISJ_TO_IMP]
   \\ rpt STRIP_TAC
   \\ Cases_on `x`
   \\ fs[Mem_NOT_IN_ffi2heap, FFI_split_NOT_IN_store2heap_aux, FFI_full_NOT_IN_store2heap_aux, FFI_part_NOT_IN_store2heap_aux])
\\ fs[markerTheory.Abbrev_def] \\ rw[]
\\ POP_ASSUM(fn x => MATCH_MP SPLIT_of_SPLIT3_1u3 x |> ASSUME_TAC)
\\ fs[Once SPLIT_SYM]
\\ rw[STAR_def]
\\ metis_tac[SAT_GC]);

val STATE_SPLIT_REFS = Q.store_thm("STATE_SPLIT_REFS",
`!a b p s. SPLIT (st2heap p (s with refs := a ++ b))
((st2heap p (s with refs := a)), (store2heap_aux (LENGTH a) b))`,
rw[] \\ Cases_on `p` \\ fs[st2heap_def] \\
sg `SPLIT3 (store2heap (a ++ b) ∪ ffi2heap (q,r) s.ffi) (store2heap a, store2heap_aux (LENGTH a) b, ffi2heap (q,r) s.ffi)`
>-(
   irule SPLIT2_SPLIT3
   \\ qexists_tac `store2heap (a ++ b)`
   \\ fs[store2heap_SPLIT, SPLIT_def, IN_DISJOINT, store2heap_def]
   \\ PURE_REWRITE_TAC[NEG_DISJ_TO_IMP]
   \\ rpt STRIP_TAC
   \\ Cases_on `x`
   \\ fs[Mem_NOT_IN_ffi2heap, FFI_split_NOT_IN_store2heap_aux, FFI_full_NOT_IN_store2heap_aux, FFI_part_NOT_IN_store2heap_aux])
\\ POP_ASSUM(fn x => MATCH_MP SPLIT_of_SPLIT3_1u3 x |> ASSUME_TAC)
\\ fs[Once SPLIT_SYM]
\\ rw[STAR_def]);

val REFS_PRED_append = Q.store_thm("REFS_PRED_append",
`!H refs s. REFS_PRED H refs p s ==> REFS_PRED H refs p (s with refs := s.refs ++ junk)`,
rw[REFS_PRED_def] >> PURE_ONCE_REWRITE_TAC [GSYM GC_STAR_GC] >> fs[STAR_ASSOC] >>
metis_tac[with_same_refs, STATE_APPEND_JUNK]);

val REFS_PRED_FRAME_append = Q.store_thm("REFS_PRED_FRAME_append",
`!H refs s. REFS_PRED_FRAME H p (refs, s) (refs, s with refs := s.refs ++ junk)`,
rw[REFS_PRED_FRAME_def] \\ metis_tac[with_same_refs, STATE_APPEND_JUNK]);

(*
 * Proof of STORE_EXTRACT_FROM_HPROP:
 * `!l xv H s. (REF (Loc l) xv * H) (store2heap s) ==> ?ps. ((ps ++ [Refv xv]) ≼ s) /\ LENGTH ps = l`
 *)

val HEAP_LOC_MEM = Q.store_thm("HEAP_LOC_MEM",
`(l ~~>> rv * H) h ==> Mem l rv IN h`,
rw[STAR_def, SEP_EXISTS_THM, cond_def, cell_def, one_def, SPLIT_def] \\ rw[IN_UNION]);

(* val HEAP_REF_MEM = Q.store_thm("HEAP_REF_MEM",
`(Loc l ~~> xv * H) h ==> Mem l (Refv xv) IN h`,
rw[STAR_def, REF_def, ARRAY_def, SEP_EXISTS_THM, cond_def, cell_def, one_def, SPLIT_def] \\ rw[IN_UNION]); 

val HEAP_ARRAY_MEM = Q.store_thm("HEAP_ARRAY_MEM",
`(ARRAY (Loc l) av * H) h ==> Mem l (Varray av) IN h`,
rw[STAR_def, REF_def, ARRAY_def, SEP_EXISTS_THM, cond_def, cell_def, one_def, SPLIT_def] \\ rw[IN_UNION]); *)

val st2heap_CELL_MEM = Q.store_thm("st2heap_CELL_MEM",
`(l ~~>> rv * H) (st2heap p s) ==> Mem l rv IN (store2heap s.refs)`,
Cases_on `p` \\ rw[st2heap_def] \\ IMP_RES_TAC HEAP_LOC_MEM
\\ fs[IN_UNION]
\\ fs[Mem_NOT_IN_ffi2heap]);

val st2heap_REF_MEM = Q.store_thm("st2heap_REF_MEM",
`(Loc l ~~> xv * H) (st2heap p s) ==> Mem l (Refv xv) IN (store2heap s.refs)`,
rw[REF_def, SEP_CLAUSES, SEP_EXISTS_THM] >>
fs[GSYM STAR_ASSOC, HCOND_EXTRACT] >>
metis_tac[st2heap_CELL_MEM]);

val st2heap_ARRAY_MEM = Q.store_thm("st2heap_ARRAY_MEM",
`(ARRAY (Loc l) av * H) (st2heap p s) ==> Mem l (Varray av) IN (store2heap s.refs)`,
rw[ARRAY_def, SEP_CLAUSES, SEP_EXISTS_THM] >>
fs[GSYM STAR_ASSOC, HCOND_EXTRACT] >>
metis_tac[st2heap_CELL_MEM]);

val store2heap_aux_LOC_MEM = Q.store_thm("store2heap_aux_LOC_MEM",
`!l rv H n s. (l ~~>> rv * H) (store2heap_aux n s) ==> Mem l rv IN (store2heap_aux n s)`,
rw[] \\ IMP_RES_TAC HEAP_LOC_MEM);

(* val store2heap_aux_REF_MEM = Q.store_thm("store2heap_aux_REF_MEM",
`!l xv H n s. (REF (Loc l) xv * H) (store2heap_aux n s) ==> Mem l (Refv xv) IN (store2heap_aux n s)`,
rw[] \\ IMP_RES_TAC HEAP_REF_MEM);

val store2heap_aux_ARRAY_MEM = Q.store_thm("store2heap_aux_ARRAY_MEM",
`!l av H n s. (ARRAY (Loc l) av * H) (store2heap_aux n s) ==> Mem l (Varray av) IN (store2heap_aux n s)`,
rw[] \\ IMP_RES_TAC HEAP_ARRAY_MEM); *)

val store2heap_LOC_MEM = Q.store_thm("store2heap_LOC_MEM",
`!l rv H s. (l ~~>> rv * H) (store2heap s) ==> Mem l rv IN (store2heap s)`,
rw[] \\ IMP_RES_TAC HEAP_LOC_MEM);

(* val store2heap_REF_MEM = Q.store_thm("store2heap_REF_MEM",
`!l xv H s. (REF (Loc l) xv * H) (store2heap s) ==> Mem l (Refv xv) IN (store2heap s)`,
rw[] \\ IMP_RES_TAC HEAP_REF_MEM);

val store2heap_ARRAY_MEM = Q.store_thm("store2heap_REF_MEM",
`!l av H s. (ARRAY (Loc l) av * H) (store2heap s) ==> Mem l (Varray av) IN (store2heap s)`,
rw[] \\ IMP_RES_TAC HEAP_ARRAY_MEM); *)

val isPREFIX_TAKE = Q.store_thm("isPREFIX_TAKE",
`!l s. isPREFIX (TAKE l s) s`,
rw[] >>
`isPREFIX (TAKE l s) (TAKE l s ++ DROP l s)` by fs[TAKE_DROP] >>
metis_tac[TAKE_DROP]);

val isPREFIX_APPEND_EQ = Q.store_thm("isPREFIX_APPEND_EQ",
`!a1 a2 b1 b2. LENGTH a1 = LENGTH a2 ==> (isPREFIX (a1 ++ b1) (a2 ++ b2) <=> a2 = a1 /\ isPREFIX b1 b2)`,
Induct_on `a1` >- fs[LENGTH_NIL_SYM] >>
rw[] >>
Cases_on `a2` >- fs[] >>
fs[] >> metis_tac[]);

val STATE_DECOMPOS_FROM_HPROP = Q.store_thm("STATE_DECOMPOS_FROM_HPROP",
`!l rv H p s. (l ~~>> rv * H) (st2heap p s) ==> ?ps. ((ps ++ [rv]) ≼ s.refs) /\ LENGTH ps = l`,
rw[] >>
IMP_RES_TAC st2heap_CELL_MEM >>
IMP_RES_TAC store2heap_IN_EL >>
qexists_tac `TAKE l s.refs` >>
Cases_on `l + 1 <= LENGTH s.refs`
>-(
    fs[LENGTH_TAKE] >>
    SUFF_TAC ``isPREFIX [rv : v store_v] (DROP l s.refs)``
    >- metis_tac[LENGTH_TAKE, LENGTH_DROP, GSYM isPREFIX_APPEND_EQ, TAKE_DROP] >>
    FIRST_ASSUM (fn x => PURE_REWRITE_TAC[x]) >>
    SUFF_TAC ``(rv : v store_v) = HD(DROP l s.refs)``
    >-( fs[] >> Cases_on `DROP l s.refs` >- fs[DROP_NIL] >> fs[]) >>
    fs[hd_drop]
) >>
irule FALSITY >>
IMP_RES_TAC store2heap_IN_LENGTH >>
fs[]);

val STATE_DECOMPOS_FROM_HPROP_REF = Q.store_thm("STATE_DECOMPOS_FROM_HPROP_REF",
`!l xv H p s. (REF (Loc l) xv * H) (st2heap p s) ==> ?ps. ((ps ++ [Refv xv]) ≼ s.refs) /\ LENGTH ps = l`,
rw[REF_def, SEP_CLAUSES, SEP_EXISTS_THM] >>
fs[GSYM STAR_ASSOC, HCOND_EXTRACT] >>
irule STATE_DECOMPOS_FROM_HPROP >>
instantiate);

val STATE_DECOMPOS_FROM_HPROP_ARRAY = Q.store_thm("STATE_DECOMPOS_FROM_HPROP_ARRAY",
`!l av H p s. (ARRAY (Loc l) av * H) (st2heap p s) ==> ?ps. ((ps ++ [Varray av]) ≼ s.refs) /\ LENGTH ps = l`,
rw[ARRAY_def, SEP_CLAUSES, SEP_EXISTS_THM] >>
fs[GSYM STAR_ASSOC, HCOND_EXTRACT] >>
irule STATE_DECOMPOS_FROM_HPROP >>
instantiate);

(* val STORE_EXTRACT_FROM_HPROP_REF = Q.store_thm("STORE_EXTRACT_REF_FROM_HPROP",
`!l xv H s. (REF (Loc l) xv * H) (store2heap s) ==>
!junk. EL l (s ++ junk) = Refv xv`,
rw[] >>
IMP_RES_TAC STORE_DECOMPOS_REF_FROM_HPROP >>
fs[IS_PREFIX_APPEND] >>
first_x_assum(fn x => CONV_RULE (CHANGED_CONV (SIMP_CONV pure_ss [GSYM APPEND_ASSOC])) x |> ASSUME_TAC) >>
`~NULL ([Refv xv] ++ (l' ++ junk))` by fs[NULL_EQ] >>
IMP_RES_TAC EL_LENGTH_APPEND >>
fs[HD] >>
metis_tac[]); *)

val STATE_EXTRACT_FROM_HPROP = Q.store_thm("STATE_EXTRACT_FROM_HPROP",
`!l rv H p s. (l ~~>> rv * H) (st2heap p s) ==>
!junk. EL l (s.refs ++ junk) = rv`,
rw[] >>
IMP_RES_TAC STATE_DECOMPOS_FROM_HPROP >>
fs[IS_PREFIX_APPEND] >>
first_x_assum(fn x => CONV_RULE (CHANGED_CONV (SIMP_CONV pure_ss [GSYM APPEND_ASSOC])) x |> ASSUME_TAC) >>
`~NULL ([rv] ++ (l' ++ junk))` by fs[NULL_EQ] >>
IMP_RES_TAC EL_LENGTH_APPEND >>
fs[HD] >>
metis_tac[]);

val STATE_EXTRACT_FROM_HPROP_REF = Q.store_thm("STATE_EXTRACT_FROM_HPROP_REF",
`!l xv H p s. ((Loc l) ~~> xv * H) (st2heap p s) ==>
!junk. EL l (s.refs ++ junk) = Refv xv`,
rw[REF_def, ARRAY_def, SEP_CLAUSES, SEP_EXISTS_THM] >>
fs[GSYM STAR_ASSOC, HCOND_EXTRACT] >>
irule STATE_EXTRACT_FROM_HPROP >>
instantiate);

val STATE_EXTRACT_FROM_HPROP_ARRAY = Q.store_thm("STATE_EXTRACT_FROM_HPROP_ARRAY",
`!l av H p s. (ARRAY (Loc l) av * H) (st2heap p s) ==>
!junk. EL l (s.refs ++ junk) = Varray av`,
rw[REF_def, ARRAY_def, SEP_CLAUSES, SEP_EXISTS_THM] >>
fs[GSYM STAR_ASSOC, HCOND_EXTRACT] >>
irule STATE_EXTRACT_FROM_HPROP >>
instantiate);

val SEPARATE_STORE_ELEM_IN_HEAP = Q.store_thm("SEPARATE_STORE_ELEM_IN_HEAP",
`!s0 x s1. SPLIT3 (store2heap (s0 ++ [x] ++ s1)) (store2heap s0, {Mem (LENGTH s0) x}, store2heap_aux (LENGTH s0 + 1) s1)`,
sg `!(s0 : v store) s1 x. SPLIT (store2heap_aux (LENGTH s0) (x::s1)) ({Mem (LENGTH s0) x}, store2heap_aux (LENGTH s0 + 1) s1)`
>-(
    rw[store2heap_def] >>
    PURE_REWRITE_TAC[Once rich_listTheory.CONS_APPEND] >>
    PURE_REWRITE_TAC [GSYM (EVAL ``store2heap_aux (LENGTH (s0 : v store)) [x]``)] >>
    ASSUME_TAC (EVAL ``LENGTH [x : v store_v]``) >>
    metis_tac[store2heap_aux_SPLIT, ADD_COMM]
) >>
rw[] >>
qspecl_then [`s0`, `[x] ++ s1`] ASSUME_TAC store2heap_SPLIT >> fs[] >>
last_x_assum(qspecl_then [`s0`, `s1`, `x`] ASSUME_TAC) >>
fs[SPLIT_def, SPLIT3_def] >>
rw[]
>-(metis_tac[UNION_ASSOC, EQ_REFL])
>-(DISCH_TAC >> IMP_RES_TAC store2heap_IN_LENGTH >> fs[]) >>
metis_tac[DISJOINT_UNION_BOTH, EQ_REFL]);

val CELL_HPROP_SAT_EQ = Q.store_thm("CELL_HPROP_SAT_EQ",
`!l xv s. (l ~~>> xv) s <=> s = {Mem l xv}`,
fs[REF_def, SEP_EXISTS, HCOND_EXTRACT, cell_def, one_def]);

val REF_HPROP_SAT_EQ = Q.store_thm("REF_HPROP_SAT_EQ",
`!l xv s. REF (Loc l) xv s <=> s = {Mem l (Refv xv)}`,
fs[REF_def, SEP_EXISTS, HCOND_EXTRACT, cell_def, one_def]);

val ARRAY_HPROP_SAT_EQ = Q.store_thm("ARRAY_HPROP_SAT_EQ",
`!l av s. ARRAY (Loc l) av s <=> s = {Mem l (Varray av)}`,
fs[ARRAY_def, SEP_EXISTS, HCOND_EXTRACT, cell_def, one_def]);

val SPLIT_UNICITY_R = Q.store_thm("SPLIT_UNICITY_R",
`SPLIT s (u, v) ==> (SPLIT s (u, v') <=> v' = v)`,
fs[SPLIT_EQ]);

(* val STORE_SAT_LOC_STAR_H_EQ = Q.store_thm("STORE_SAT_LOC_STAR_H_EQ",
`!s0 xv s1 H. (Loc (LENGTH s0) ~~> xv * H) (store2heap (s0 ++ [Refv xv] ++ s1)) <=>
H ((store2heap s0) UNION (store2heap_aux (LENGTH s0 + 1) s1))`,
rw[] >>
qspecl_then [`s0`, `Refv xv`, `s1`] ASSUME_TAC SEPARATE_STORE_ELEM_IN_HEAP >>
IMP_RES_TAC SPLIT_of_SPLIT3_1u3 >>
last_x_assum(fn x => ALL_TAC) >>
EQ_TAC
>-(
    rw[STAR_def, REF_HPROP_SAT_EQ] >>
    IMP_RES_TAC SPLIT_UNICITY_R >>
    fs[]
) >>
DISCH_TAC >>
rw[STAR_def] >>
instantiate >>
rw[REF_HPROP_SAT_EQ]); *)

val DIFF_UNION_COMM = Q.store_thm("DIFF_UNION_COMM",
`DISJOINT s2 s3 ==>
(s1 UNION s2) DIFF s3 = (s1 DIFF s3) UNION s2`,
rw[SET_EQ_SUBSET]
\\ fs[SUBSET_DEF, IN_DISJOINT] \\rw[]
\\ last_x_assum (fn x => PURE_ONCE_REWRITE_RULE [NEG_DISJ_TO_IMP] x |> IMP_RES_TAC)
\\ fs[]);

val STATE_SAT_CELL_STAR_H_EQ = Q.store_thm("STATE_SAT_CELL_STAR_H_EQ",
`!p s s0 rv s1 H. ((LENGTH s0) ~~>> rv * H) (st2heap p (s with refs := s0 ++ [rv] ++ s1)) <=>
H ((store2heap s0) UNION (store2heap_aux (LENGTH s0 + 1) s1) UNION (ffi2heap p s.ffi))`,
rw[] >>
Cases_on `p` >>
fs[st2heap_def] >>
qspecl_then [`p`, `s with refs := s0 ++ [rv] ++ s1`] ASSUME_TAC st2heap_SPLIT_FFI >>
fs[] >>
qspecl_then [`s0`, `rv`, `s1`] ASSUME_TAC SEPARATE_STORE_ELEM_IN_HEAP >>
IMP_RES_TAC SPLIT_of_SPLIT3_1u3 >>
EQ_TAC
>-(
    rw[STAR_def, CELL_HPROP_SAT_EQ] >>
    fs[SPLIT_EQ] >>
    rw[] >>
    fs[st2heap_def] >>
    `DISJOINT (ffi2heap (q, r) s.ffi) {Mem (LENGTH s0) rv}` by fs[DISJOINT_DEF, Mem_NOT_IN_ffi2heap] >>
    fs[Once DIFF_UNION_COMM]
) >>
DISCH_TAC >>
rw[STAR_def] >>
instantiate >>
qexists_tac `{Mem (LENGTH s0) rv}` >>
fs[CELL_HPROP_SAT_EQ] >>
fs[SPLIT_def, SPLIT3_def] >>
rw[]
>-(
    rw[store2heap_append_many, store2heap_aux_append_many]
    >> metis_tac[store2heap_aux_def, UNION_COMM, UNION_ASSOC])
\\ fs[Mem_NOT_IN_ffi2heap]
);

val STATE_SAT_REF_STAR_H_EQ = Q.store_thm("STATE_SAT_REF_STAR_H_EQ",
`!p s s0 xv s1 H. (Loc (LENGTH s0) ~~> xv * H) (st2heap p (s with refs := s0 ++ [Refv xv] ++ s1)) <=>
H ((store2heap s0) UNION (store2heap_aux (LENGTH s0 + 1) s1) UNION (ffi2heap p s.ffi))`,
rw[REF_def, ARRAY_def, SEP_CLAUSES, SEP_EXISTS_THM] >>
fs[GSYM STAR_ASSOC, HCOND_EXTRACT] >>
fs[STATE_SAT_CELL_STAR_H_EQ]);

val STATE_SAT_ARRAY_STAR_H_EQ = Q.store_thm("STATE_SAT_ARRAY_STAR_H_EQ",
`!p s s0 av s1 H. (ARRAY (Loc (LENGTH s0)) av * H) (st2heap p (s with refs := s0 ++ [Varray av] ++ s1)) <=>
H ((store2heap s0) UNION (store2heap_aux (LENGTH s0 + 1) s1) UNION (ffi2heap p s.ffi))`,
rw[REF_def, ARRAY_def, SEP_CLAUSES, SEP_EXISTS_THM] >>
fs[GSYM STAR_ASSOC, HCOND_EXTRACT] >>
fs[STATE_SAT_CELL_STAR_H_EQ]);

(* val STORE_UPDATE_HPROP = Q.store_thm("STORE_UPDATE_HPROP",
`(Loc l ~~> xv * H) (store2heap s) ==> (Loc l ~~> xv' * H) (store2heap (LUPDATE (Refv xv') l s))`,
DISCH_TAC >>
sg `?s0 s1. s = s0 ++ [Refv xv] ++ s1 /\ LENGTH s0 = l`
>-(
    IMP_RES_TAC STORE_DECOMPOS_FROM_HPROP >>
    IMP_RES_TAC rich_listTheory.IS_PREFIX_APPEND >>
    SATISFY_TAC
) >>
rw[LUPDATE_APPEND1, LUPDATE_APPEND2, LUPDATE_def] >>
metis_tac[STORE_SAT_LOC_STAR_H_EQ, REF_HPROP_SAT_EQ, STAR_def]); *)

val STATE_UPDATE_HPROP_CELL = Q.store_thm("STATE_UPDATE_HPROP_CELL",
`(l ~~>> rv * H) (st2heap p s) ==> (l ~~>> rv' * H) (st2heap p (s with refs := (LUPDATE rv' l s.refs)))`,
DISCH_TAC >>
sg `?s0 s1. s.refs = s0 ++ [rv] ++ s1 /\ LENGTH s0 = l`
>-(
    IMP_RES_TAC STATE_DECOMPOS_FROM_HPROP >>
    IMP_RES_TAC rich_listTheory.IS_PREFIX_APPEND >>
    SATISFY_TAC
) >>
rw[LUPDATE_APPEND1, LUPDATE_APPEND2, LUPDATE_def] >>
fs[STATE_SAT_CELL_STAR_H_EQ] >>
sg `(st2heap p s) = st2heap p (s with refs := s0 ++ [rv] ++ s1)` 
>-(
   `s = (s with refs := s0 ++ [rv] ++ s1)` by POP_ASSUM (fn x => rw[GSYM x, with_same_refs])
   >> POP_ASSUM(fn x => rw[GSYM x])
) >>
POP_ASSUM(fn x => fs[x]) >>
fs[STATE_SAT_CELL_STAR_H_EQ]);

val STATE_UPDATE_HPROP_REF = Q.store_thm("STATE_UPDATE_HPROP_REF",
`(Loc l ~~> xv * H) (st2heap p s) ==> (Loc l ~~> xv' * H) (st2heap p (s with refs := (LUPDATE (Refv xv') l s.refs)))`,
rw[REF_def, ARRAY_def, SEP_CLAUSES, SEP_EXISTS_THM] >>
fs[GSYM STAR_ASSOC, HCOND_EXTRACT] >>
irule STATE_UPDATE_HPROP_CELL >>
instantiate
);

val STATE_UPDATE_HPROP_ARRAY = Q.store_thm("STATE_UPDATE_HPROP_ARRAY",
`(ARRAY (Loc l) av * H) (st2heap p s) ==> (ARRAY (Loc l) av' * H) (st2heap p (s with refs := (LUPDATE (Varray av') l s.refs)))`,
rw[REF_def, ARRAY_def, SEP_CLAUSES, SEP_EXISTS_THM] >>
fs[GSYM STAR_ASSOC, HCOND_EXTRACT] >>
irule STATE_UPDATE_HPROP_CELL >>
instantiate
);

val evaluate_empty_state_IMP_junk = Q.store_thm("evaluate_empty_state_IMP_junk",
`!junk refs' env s exp x. evaluate F env (empty_state with refs := s.refs ++ junk) exp
 (empty_state with refs := s.refs ++ junk ++ refs',Rval x) ⇒
 evaluate F env (s with refs := s.refs ++ junk) exp (s with refs := s.refs ++ junk ++ refs',Rval x)`,
rw[]
\\ ASSUME_TAC (
Thm.INST_TYPE [``:'ffi`` |-> ``:'a``] evaluate_empty_state_IMP |>
Thm.INST[``s:'a state`` |-> ``(s:'a state) with refs := s.refs ++ junk``])
\\ fs[]);

val UNIQUE_CELLS = Q.prove(
`!p s. !l xv xv' H H'. (l ~~>> xv * H) (st2heap p s) /\ (l ~~>> xv' * H') (st2heap p s) ==> xv' = xv`,
rw[] >>
IMP_RES_TAC st2heap_CELL_MEM >>
IMP_RES_TAC store2heap_IN_unique_key);

val evaluate_unique_result = Q.store_thm("evaluate_unique_result",
`!expr env s s1 s2 res1 res2. evaluate F env s expr (s1, res1) ==>
(evaluate F env s expr (s2, res2) <=> (s2 = s1 /\ res2 = res1))`,
rw[] \\ EQ_TAC >-(rw[] \\ IMP_RES_TAC big_exp_determ \\ rw[]) \\ rw[]);

(*
 * Definition of EvalM
 *)

val EvalM_def = Define `
  EvalM env exp P H <=>
    !(s:unit state) p refs. REFS_PRED H refs p s  ==>
    !junk.
    ?s2 res refs2. evaluate F env (s with refs := s.refs ++ junk) exp (s2,res) /\
    P (refs, s) (refs2, s2, res) /\ REFS_PRED_FRAME H p (refs, s) (refs2, s2)`;

(* refinement invariant for ``:('a, 'b, 'c) M`` *)
val _ = type_abbrev("M", ``:'a -> ('b, 'c) exc # 'a``);

val MONAD_def = Define `
  MONAD (a:'a->v->bool) (b: 'b->v->bool) (x:('refs, 'a, 'b) M)
                                    (state1:'refs,s1:unit state)
                                     (state2:'refs,s2:unit state,
                                      res: (v,v) result) =
    case (x state1, res) of
      ((Success y, st), Rval v) => (st = state2) /\ a y v
    | ((Failure e, st), Rerr (Rraise v)) => (st = state2) /\
                                              b e v
    | _ => F`

(* return *)
val EvalM_return = Q.store_thm("EvalM_return",
  `!H b. Eval env exp (a x) ==>
    EvalM env exp (MONAD a b (ex_return x)) H`,
  rw[Eval_def,EvalM_def,st_ex_return_def,MONAD_def] \\
  first_x_assum(qspec_then`(s with refs := s.refs ++ junk).refs`strip_assume_tac)
  \\ IMP_RES_TAC (evaluate_empty_state_IMP
                  |> INST_TYPE [``:'ffi``|->``:unit``]) \\
  asm_exists_tac \\ simp[] \\ PURE_REWRITE_TAC[GSYM APPEND_ASSOC] \\
  fs[REFS_PRED_FRAME_append]
  );

(* bind *)

val EvalM_bind = Q.store_thm("EvalM_bind",
  `!H c. EvalM env e1 (MONAD b c (x:('refs, 'b, 'c) M)) H /\
    (!x v. b x v ==> EvalM (write name v env) e2 (MONAD a c ((f x):('refs, 'a, 'c) M)) H) ==>
    EvalM env (Let (SOME name) e1 e2) (MONAD a c (ex_bind x f)) H`,
  rw[EvalM_def,MONAD_def,st_ex_return_def,PULL_EXISTS] \\
  first_x_assum drule \\
  disch_then(qspec_then`junk`strip_assume_tac) \\
  Cases_on`x refs` \\ Cases_on`q` \\ Cases_on`res` \\ fs[]
  >- (
    rw[Once evaluate_cases] \\
    srw_tac[DNF_ss][] \\ disj1_tac \\
    asm_exists_tac \\ rw[] \\
    IMP_RES_TAC REFS_PRED_FRAME_imp \\
    first_x_assum drule \\ disch_then drule \\
    disch_then(qspec_then`[]`strip_assume_tac) \\
    fs[GSYM write_def,namespaceTheory.nsOptBind_def,st_ex_bind_def,with_same_refs] \\
    asm_exists_tac \\ fs[] \\ 
    instantiate \\
    IMP_RES_TAC REFS_PRED_FRAME_trans)
  \\ Cases_on`e` \\ fs[] \\
  rw[Once evaluate_cases] \\
  srw_tac[DNF_ss][] \\ disj2_tac \\
  asm_exists_tac \\ rw[] \\
  rw[st_ex_bind_def]);

(* lift pure refinement invariants *)

val _ = type_abbrev("H",``:'a -> 'refs # unit state ->
                                 'refs # unit state # (v,v) result -> bool``);

val PURE_def = Define `
  PURE a (x:'a) (refs1:'refs,s1:unit state) (refs2,s2,res:(v,v) result) =
    ?v:v junk. (res = Rval v) /\ (refs1 = refs2) /\ (s2 = s1 with refs := s1.refs ++ junk) /\ a x v`;

val Eval_IMP_PURE = Q.store_thm("Eval_IMP_PURE",
  `!H env exp P x. Eval env exp (P x) ==> EvalM env exp (PURE P x) H`,
  rw[Eval_def,EvalM_def,PURE_def,PULL_EXISTS]
  \\ first_x_assum(qspec_then`(s with refs := s.refs ++ junk).refs`strip_assume_tac)
  \\ IMP_RES_TAC (evaluate_empty_state_IMP
                  |> INST_TYPE [``:'ffi``|->``:unit``])
  \\ fs[]
  \\ metis_tac[APPEND_ASSOC, REFS_PRED_FRAME_append]);

(* function abstraction and application *)

val ArrowP_def = Define `
  (ArrowP : ('refs -> hprop) -> ('a, 'refs) H -> ('b, 'refs) H -> ('a -> 'b) -> v -> bool) H a b f c =
     !x p refs1 s1 refs2 s2 (res:(v,v) result).
       a x (refs1,s1) (refs2,s2,res) /\ REFS_PRED H refs1 p s1 ==>
       ?junk v env exp.
       (refs2 = refs1) /\ (s2 = s1 with refs := s1.refs ++ junk) /\
       (res = Rval v) /\ do_opapp [c;v] = SOME (env,exp) /\
       !junk. ?refs3 s3 res3.
         evaluate F env (s2 with refs := s2.refs ++ junk) exp (s3,res3) /\
         b (f x) (refs1,s1) (refs3,s3,res3) /\
         REFS_PRED_FRAME H p (refs1, s1) (refs3, s3)`;

val ArrowM_def = Define `
(ArrowM : ('refs -> hprop) -> ('a, 'refs) H -> ('b, 'refs) H -> ('a -> 'b, 'refs) H) H a b =
     PURE (ArrowP H a b)`;

(*val _ = add_infix("-M->",400,HOLgrammars.RIGHT)
val _ = overload_on ("-M->",``ArrowM``) *)

val evaluate_list_cases = let
  val lemma = evaluate_cases |> CONJUNCTS |> el 2
  in CONJ (``evaluate_list a5 a6 a7 [] (a9,Rval a10)``
           |> SIMP_CONV (srw_ss()) [Once lemma])
          (``evaluate_list a5 a6 a7 (x::xs) (a9,Rval a10)``
           |> SIMP_CONV (srw_ss()) [Once lemma]) end

val EvalM_ArrowM = Q.store_thm("EvalM_ArrowM",
  `!H. EvalM env x1 ((ArrowM H a b) f) H ==>
    EvalM env x2 (a x) H ==>
    EvalM env (App Opapp [x1;x2]) (b (f x)) H`,
  rw[EvalM_def,ArrowM_def,ArrowP_def,PURE_def,PULL_EXISTS]
  \\ rw[Once evaluate_cases,evaluate_list_cases,PULL_EXISTS]
  \\ first_x_assum drule \\ rw[]
  \\ srw_tac[DNF_ss][] \\ disj1_tac
  \\ first_x_assum(qspec_then`junk`strip_assume_tac)
  \\ IMP_RES_TAC REFS_PRED_FRAME_imp
  \\ first_x_assum drule \\ rw[]
  \\ first_x_assum(qspec_then`[]`strip_assume_tac)
  \\ fs[with_same_refs]
  \\ first_x_assum drule \\ rw[]
  \\ POP_ASSUM(qspec_then `p` IMP_RES_TAC)
  \\ first_x_assum(qspec_then`junk'`strip_assume_tac) \\ fs[]
  \\ asm_exists_tac \\ rw[] \\ fs[]
  \\ asm_exists_tac \\ rw[]
  \\ asm_exists_tac \\ rw[]
  \\ asm_exists_tac \\ rw[]);

val EvalM_Fun = Q.store_thm("EvalM_Fun",
  `!H. (!v x. a x v ==> EvalM (write name v env) body (b (f x)) H) ==>
    EvalM env (Fun name body) ((ArrowM H (PURE a) b) f) H`,
  rw[EvalM_def,ArrowM_def,ArrowP_def,PURE_def,Eq_def]
  \\ rw[Once evaluate_cases,PULL_EXISTS]
  \\ reverse(rw[Once state_component_equality, REFS_PRED_append]) >-(fs[REFS_PRED_FRAME_append])
  \\ rw[Once state_component_equality]
  \\ rw[do_opapp_def,GSYM write_def]
  \\ last_x_assum drule \\ rw[]
  \\ first_x_assum drule \\ rw[]
  \\ first_x_assum(qspec_then `junk ++ junk'` assume_tac)
  \\ rw[]
  \\ SATISFY_TAC);

val EvalM_Fun_Eq = Q.store_thm("EvalM_Fun_Eq",
  `!H. (!v. a x v ==> EvalM (write name v env) body (b (f x)) H) ==>
    EvalM env (Fun name body) ((ArrowM H (PURE (Eq a x)) b) f) H`,
  rw[EvalM_def,ArrowM_def,ArrowP_def,PURE_def,Eq_def]
  \\ rw[Once evaluate_cases,PULL_EXISTS]
  \\ reverse(rw[Once state_component_equality,REFS_PRED_append]) >-(fs[REFS_PRED_FRAME_append])
  \\ rw[Once state_component_equality]
  \\ rw[do_opapp_def,GSYM write_def]
  \\ PURE_REWRITE_TAC [GSYM APPEND_ASSOC]
  \\ last_x_assum drule \\ rw[]
  \\ first_x_assum drule \\ rw[]
  \\ first_x_assum(qspec_then `junk ++ junk'` assume_tac)
  \\ rw[] \\ SATISFY_TAC);

(* More proofs *)
val EvalM_Fun_PURE_IMP = Q.store_thm("EvalM_Fun_PURE_IMP",
  `!H. VALID_REFS_PRED H ==>
    EvalM env (Fun n exp) (PURE P f) H ==>
    P f (Closure env n exp)`,
  fs [EvalM_def,PURE_def,PULL_EXISTS,Once evaluate_cases, VALID_REFS_PRED_def]
     \\ rw [] \\ metis_tac[]);

val LOOKUP_VAR_EvalM_IMP = Q.store_thm("LOOKUP_VAR_EvalM_IMP",
  `!H. VALID_REFS_PRED H ==>
    (!env. LOOKUP_VAR n env v ==> EvalM env (Var (Short n)) (PURE P g) H) ==>
    P g v`,
  fs [LOOKUP_VAR_def,lookup_var_def,EvalM_def,PURE_def,AND_IMP_INTRO,
      Once evaluate_cases,PULL_EXISTS,PULL_FORALL, VALID_REFS_PRED_def]
  \\ `nsLookup (<|v := nsBind n v nsEmpty|>).v (Short n) = SOME v` by EVAL_TAC
  \\ metis_tac[]);

val EvalM_ArrowM_IMP = Q.store_thm("EvalM_ArrowM_IMP",
  `!H. VALID_REFS_PRED H ==>
   EvalM env (Var x) ((ArrowM H a b) f) H ==>
    Eval env (Var x) (ArrowP H a b f)`,
  rw[ArrowM_def,EvalM_def,Eval_def,PURE_def,PULL_EXISTS, VALID_REFS_PRED_def] \\
  first_x_assum drule \\
  disch_then(qspec_then`[]`strip_assume_tac) \\
  fs[Once evaluate_cases] \\
  rw[state_component_equality]);

val EvalM_PURE_EQ = Q.store_thm("EvalM_PURE_EQ",
  `!H. VALID_REFS_PRED H ==>
   EvalM env (Fun n exp) (PURE P x) H = Eval env (Fun n exp) (P x)`,
  REPEAT STRIP_TAC \\ EQ_TAC \\ REPEAT STRIP_TAC
  \\ FULL_SIMP_TAC std_ss [Eval_IMP_PURE]
  \\ FULL_SIMP_TAC std_ss [Eval_def,EvalM_def,PURE_def,PULL_EXISTS]
  \\ fs[VALID_REFS_PRED_def] \\ rw[]
  \\ first_x_assum drule
  \\ disch_then(qspec_then`[]`strip_assume_tac)
  \\ fs[Once evaluate_cases]
  \\ rw[state_component_equality]);

val EvalM_Var_SIMP = Q.store_thm("EvalM_Var_SIMP",
  `EvalM (write n x env) (Var (Short y)) p H =
    if n = y then EvalM (write n x env) (Var (Short y)) p H
             else EvalM env (Var (Short y)) p H`,
  SIMP_TAC std_ss [EvalM_def] \\ SRW_TAC [] []
  \\ ASM_SIMP_TAC (srw_ss()) [Once evaluate_cases]
  \\ ASM_SIMP_TAC (srw_ss()) [Once evaluate_cases,write_def]);

val EvalM_Recclosure = Q.store_thm("EvalM_Recclosure",
  `!H. (!v. a n v ==>
         EvalM (write name v (write_rec [(fname,name,body)] env2 env2))
               body (b (f n)) H) ==>
    LOOKUP_VAR fname env (Recclosure env2 [(fname,name,body)] fname) ==>
    EvalM env (Var (Short fname)) ((ArrowM H (PURE (Eq a n)) b) f) H`,
  GEN_TAC \\ NTAC 2 STRIP_TAC \\ IMP_RES_TAC LOOKUP_VAR_THM
  \\ POP_ASSUM MP_TAC \\ POP_ASSUM (K ALL_TAC) \\ POP_ASSUM MP_TAC
  \\ rw[Eval_def,Arrow_def,EvalM_def,ArrowM_def,PURE_def,ArrowP_def,PULL_EXISTS]
  \\ ntac 2 (pop_assum mp_tac)
  \\ rw[Once evaluate_cases]
  \\ rw[Once evaluate_cases]
  \\ fs[state_component_equality]
  \\ reverse(rw[Eq_def,do_opapp_def,Once find_recfun_def,REFS_PRED_append])  >-(fs[REFS_PRED_FRAME_append])
  \\ fs[build_rec_env_def,write_rec_def,FOLDR,write_def]
  \\ METIS_TAC[APPEND_ASSOC]);

val IND_HELP = Q.store_thm("IND_HELP",
  `!env cl.
      LOOKUP_VAR x env cl /\
      EvalM env (Var (Short x)) ((ArrowM H b1 b2) f) H ==>
      EvalM (write x cl cl_env) (Var (Short x)) ((ArrowM H b1 b2) f) H`,
  rw[EvalM_def,Eval_def,ArrowM_def,PURE_def,PULL_EXISTS,LOOKUP_VAR_def]
  \\ rw[Once evaluate_cases]
  \\ fs[Once evaluate_cases]
  \\ rfs[write_def,state_component_equality,lookup_var_def]
  \\ METIS_TAC[]);

val write_rec_one = Q.store_thm("write_rec_one",
  `write_rec [(x,y,z)] env env = write x (Recclosure env [(x,y,z)] x) env`,
  SIMP_TAC std_ss [write_rec_def,write_def,build_rec_env_def,FOLDR]);

(* Eq simps *)

val EvalM_FUN_FORALL = Q.store_thm("EvalM_FUN_FORALL",
  `!H. (!x. EvalM env exp (PURE (p x) f) H) ==>
    EvalM env exp (PURE (FUN_FORALL x. p x) f) H`,
  rw[EvalM_def,PURE_def]
  \\ first_x_assum drule
  \\ simp[PULL_EXISTS,FUN_FORALL]
  \\ strip_tac
  \\ first_assum(qspecl_then[`ARB`,`junk`]strip_assume_tac)
  \\ asm_exists_tac \\ simp[]
  \\ qx_gen_tac`x`
  \\ first_assum(qspecl_then[`x`,`junk`]strip_assume_tac)
  \\ imp_res_tac determTheory.big_exp_determ \\ fs[]);

val EvalM_FUN_FORALL_EQ = Q.store_thm("EvalM_FUN_FORALL_EQ",
  `!H. (!x. EvalM env exp (PURE (p x) f) H) =
    EvalM env exp (PURE (FUN_FORALL x. p x) f) H`,
  REPEAT STRIP_TAC \\ EQ_TAC \\ FULL_SIMP_TAC std_ss [EvalM_FUN_FORALL]
  \\ fs [EvalM_def,PURE_def,PULL_EXISTS,FUN_FORALL] \\ METIS_TAC []);

val M_FUN_FORALL_PUSH1 = Q.prove(
  `(FUN_FORALL x. ArrowP H a (PURE (b x))) = (ArrowP H a (PURE (FUN_FORALL x. b x)))`,
  rw[FUN_EQ_THM,FUN_FORALL,ArrowP_def,PURE_def,PULL_EXISTS]
  \\ reverse EQ_TAC >- METIS_TAC[] \\ rw[]
  \\ first_x_assum drule \\ rw[]
  \\ first_x_assum IMP_RES_TAC
  \\ first_assum(qspec_then`ARB`strip_assume_tac) \\ fs[]
  \\ fs[state_component_equality]
  \\ qx_gen_tac`junk2`
  \\ first_assum(qspecl_then[`ARB`,`junk2`]strip_assume_tac)
  \\ asm_exists_tac \\ fs[]
  \\ qx_gen_tac`y`
  \\ first_assum(qspecl_then[`y`,`junk2`]strip_assume_tac)
  \\ imp_res_tac determTheory.big_exp_determ \\ fs[]) |> GEN_ALL;

val M_FUN_FORALL_PUSH2 = Q.prove(
  `(FUN_FORALL x. ArrowP H ((PURE (a x))) b) =
    (ArrowP H (PURE (FUN_EXISTS x. a x)) b)`,
  FULL_SIMP_TAC std_ss [ArrowP_def,FUN_EQ_THM,AppReturns_def,
    FUN_FORALL,FUN_EXISTS,PURE_def] \\ METIS_TAC []) |> GEN_ALL;

val FUN_EXISTS_Eq = Q.prove(
  `(FUN_EXISTS x. Eq a x) = a`,
  SIMP_TAC std_ss [FUN_EQ_THM,FUN_EXISTS,Eq_def]) |> GEN_ALL;

val M_FUN_QUANT_SIMP = save_thm("M_FUN_QUANT_SIMP",
  LIST_CONJ [FUN_EXISTS_Eq,M_FUN_FORALL_PUSH1,M_FUN_FORALL_PUSH2]);

(* otherwise *)

val EvalM_otherwise = Q.store_thm("EvalM_otherwise",
  `!H b n. EvalM env exp1 (MONAD a b x1) H ==>
        (!i. EvalM (write n i env) exp2 (MONAD a b x2) H) ==>
        EvalM env (Handle exp1 [(Pvar n,exp2)]) (MONAD a b (x1 otherwise x2)) H`,
  SIMP_TAC std_ss [EvalM_def] \\ REPEAT STRIP_TAC
  \\ SIMP_TAC (srw_ss()) [Once evaluate_cases]
  \\ Q.PAT_X_ASSUM `!s p refs. bb ==> bbb` (MP_TAC o Q.SPECL [`s`, `p`,`refs`])
  \\ FULL_SIMP_TAC std_ss [] \\ REPEAT STRIP_TAC
  \\ first_x_assum(qspec_then`junk`strip_assume_tac)
  \\ Cases_on `res` THEN1
   (srw_tac[DNF_ss][] >> disj1_tac \\
    asm_exists_tac \\ fs[MONAD_def,otherwise_def] \\
    CASE_TAC \\ fs[] \\ CASE_TAC \\ fs[] )
  \\ Q.PAT_X_ASSUM `MONAD xx yy zz t1 t2` MP_TAC
  \\ SIMP_TAC std_ss [Once MONAD_def] \\ STRIP_TAC
  \\ Cases_on `x1 refs` \\ FULL_SIMP_TAC (srw_ss()) []
  \\ Cases_on `q` \\ FULL_SIMP_TAC (srw_ss()) [otherwise_def]
  \\ Cases_on `e` \\ FULL_SIMP_TAC (srw_ss()) [otherwise_def]
  \\ srw_tac[DNF_ss][] \\ disj2_tac \\ disj1_tac
  \\ asm_exists_tac \\ fs[]
  \\ simp[Once evaluate_cases,pat_bindings_def,pmatch_def,GSYM write_def]
  \\ IMP_RES_TAC REFS_PRED_FRAME_imp
  \\ first_x_assum drule
  \\ qmatch_goalsub_rename_tac`write n v`
  \\ disch_then(qspecl_then[`v`,`[]`]strip_assume_tac)
  \\ fs[with_same_refs]
  \\ IMP_RES_TAC REFS_PRED_FRAME_imp
  \\ asm_exists_tac \\ fs[]
  \\ IMP_RES_TAC REFS_PRED_FRAME_trans
  \\ fs[MONAD_def]
  \\ CASE_TAC \\ fs[]
  \\ CASE_TAC \\ fs[]
  \\ asm_exists_tac \\ fs[]);

(* if *)

val EvalM_If = Q.store_thm("EvalM_If",
  `!H. (a1 ==> Eval env x1 (BOOL b1)) /\
    (a2 ==> EvalM env x2 (a b2) H) /\
    (a3 ==> EvalM env x3 (a b3) H) ==>
    (a1 /\ (CONTAINER b1 ==> a2) /\ (~CONTAINER b1 ==> a3) ==>
     EvalM env (If x1 x2 x3) (a (if b1 then b2 else b3)) H)`,
  rpt strip_tac \\ fs[]
  \\ `∀(H:'a -> hprop). EvalM env x1 (PURE BOOL b1) H` by metis_tac[Eval_IMP_PURE]
  \\ fs[EvalM_def,PURE_def, BOOL_def,PULL_EXISTS]
  \\ rpt strip_tac
  \\ first_x_assum drule
  \\ disch_then(qspec_then`junk`strip_assume_tac)
  \\ simp[Once evaluate_cases]
  \\ simp_tac(srw_ss()++DNF_ss)[]
  \\ disj1_tac
  \\ asm_exists_tac
  \\ simp[do_if_def]
  \\ rw[]
  \\ first_x_assum (match_mp_tac o MP_CANON)
  \\ fs[ml_translatorTheory.CONTAINER_def]);

val Eval_Var_SIMP2 = Q.store_thm("Eval_Var_SIMP2",
  `Eval (write x i env) (Var (Short y)) p =
      if x = y then p i else Eval env (Var (Short y)) p`,
  SIMP_TAC (srw_ss()) [Eval_def,Once evaluate_cases] \\ SRW_TAC [] []
  \\ ASM_SIMP_TAC (srw_ss()) [Eval_def,Once evaluate_cases]
  \\ ASM_SIMP_TAC (srw_ss()) [Eval_def,
       Once evaluate_cases,write_def]
  \\ simp[state_component_equality]);

val EvalM_Let = Q.store_thm("EvalM_Let",
  `!H. Eval env exp (a res) /\
    (!v. a res v ==> EvalM (write name v env) body (b (f res)) H) ==>
    EvalM env (Let (SOME name) exp body) (b (LET f res)) H`,
  rw[]
  \\ imp_res_tac Eval_IMP_PURE
  \\ fs[EvalM_def]
  \\ rpt strip_tac
  \\ first_x_assum drule
  \\ disch_then(qspec_then`junk`strip_assume_tac)
  \\ simp[Once evaluate_cases,GSYM write_def,namespaceTheory.nsOptBind_def]
  \\ srw_tac[DNF_ss][]
  \\ fs[PURE_def] \\ rveq
  \\ srw_tac[DNF_ss][] \\ disj1_tac
  \\ asm_exists_tac \\ fs[]);

(* PMATCH *)

val EvalM_PMATCH_NIL = Q.store_thm("EvalM_PMATCH_NIL",
  `!H b x xv a.
      Eval env x (a xv) ==>
      CONTAINER F ==>
      EvalM env (Mat x []) (b (PMATCH xv [])) H`,
  rw[ml_translatorTheory.CONTAINER_def]);

val EvalM_PMATCH = Q.store_thm("EvalM_PMATCH",
  `!H b a x xv.
      ALL_DISTINCT (pat_bindings p []) ⇒
      (∀v1 v2. pat v1 = pat v2 ⇒ v1 = v2) ⇒
      Eval env x (a xv) ⇒
      (p1 xv ⇒ EvalM env (Mat x ys) (b (PMATCH xv yrs)) H) ⇒
      EvalPatRel env a p pat ⇒
      (∀env2 vars.
        EvalPatBind env a p pat vars env2 ∧ p2 vars ⇒
        EvalM env2 e (b (res vars)) H) ⇒
      (∀vars. PMATCH_ROW_COND pat (K T) xv vars ⇒ p2 vars) ∧
      ((∀vars. ¬PMATCH_ROW_COND pat (K T) xv vars) ⇒ p1 xv) ⇒
      EvalM env (Mat x ((p,e)::ys))
        (b (PMATCH xv ((PMATCH_ROW pat (K T) res)::yrs))) H`,
  rw[EvalM_def] >>
  imp_res_tac Eval_IMP_PURE >>
  fs[EvalM_def] >>
  rw[Once evaluate_cases,PULL_EXISTS] >> fs[] >>
  first_x_assum drule >>
  disch_then(qspec_then`junk`strip_assume_tac) >>
  fs[PURE_def] \\ rveq \\
  srw_tac[DNF_ss][] \\ disj1_tac \\
  asm_exists_tac \\ fs[] \\
  rw[Once evaluate_cases,PULL_EXISTS] >>
  Cases_on`∃vars. PMATCH_ROW_COND pat (K T) xv vars` >> fs[] >- (
    imp_res_tac pmatch_PMATCH_ROW_COND_Match >>
    qpat_x_assum`p1 xv ⇒ $! _`kall_tac >>
    qpat_x_assum`_ ==> p1 xv`kall_tac >>
    fs[EvalPatRel_def] >>
    first_x_assum(qspec_then`vars`mp_tac)>>simp[] >> strip_tac >>
    first_x_assum(fn th => first_assum(strip_assume_tac o MATCH_MP th)) >>
    fs[PMATCH_ROW_COND_def] \\
    last_x_assum (qspec_then `s.refs ++ junk'` ASSUME_TAC) \\ rw[] \\
    `EvalPatBind env a p pat vars (env with v := nsAppend (alist_to_ns env2) env.v)`
    by (
	simp[EvalPatBind_def,sem_env_component_equality] \\
        qexists_tac `v` >> fs[] >>
      qspecl_then[`s.refs ++ junk'`,`p`,`v`,`[]`,`env`]mp_tac(CONJUNCT1 pmatch_imp_Pmatch) \\
      simp[] \\
      metis_tac[] ) \\
    first_x_assum drule \\ simp[]
    \\ disch_then(qspec_then`s`mp_tac)
    \\ disch_then drule
    \\ disch_then(qspec_then`junk'`strip_assume_tac) \\ fs[]
    \\ asm_exists_tac \\ fs[]
    \\ simp[PMATCH_def,PMATCH_ROW_def,PMATCH_ROW_COND_def] >>
    `(some x. pat x = pat vars) = SOME vars` by (
      simp[optionTheory.some_def] >>
      METIS_TAC[] ) >>
    simp[] >>
    asm_exists_tac \\ fs[]) >>
  drule (GEN_ALL pmatch_PMATCH_ROW_COND_No_match)
  \\ disch_then drule \\ disch_then drule
  \\ simp[] \\ strip_tac \\
  first_x_assum(qspec_then`s`mp_tac) \\
  disch_then drule \\
  simp[Once evaluate_cases,PULL_EXISTS] \\
  disch_then(qspec_then`junk`mp_tac) \\
  strip_tac \\ imp_res_tac determTheory.big_exp_determ \\ fs[] \\
  rw[] \\ asm_exists_tac \\ fs[] \\
  fs[PMATCH_def,PMATCH_ROW_def] \\
  asm_exists_tac \\ fs[]);

(* Exception handling *)
val EvalM_handle_MODULE = Q.store_thm("EvalM_handle_MODULE",
 `!cons_name module_name ECons TYPE EXN_TYPE handle_fun H n x1 exp1 x2 exp2 env a.
  (!s e s1. x1 s = (Failure (ECons e), s1) ==> handle_fun x1 x2 s = x2 e s1) ==>
  (!s. (!e s1. x1 s <> (Failure (ECons e), s1)) ==> handle_fun x1 x2 s = x1 s) ==>
  (!e ev. EXN_TYPE (ECons e) ev ==>
   ?ev'.
   ev = Conv (SOME (cons_name,TypeExn (Long module_name (Short cons_name)))) [ev'] /\ 
   TYPE e ev') ==>
  (!e ev. EXN_TYPE e ev ==>
   (!e'. e <> ECons e') ==>
   ?ev' cons_name_1.
   ev = Conv (SOME (cons_name_1,TypeExn (Long module_name (Short cons_name_1)))) [ev'] /\
   cons_name_1 <> cons_name) ==>
  lookup_cons cons_name env = SOME (1,TypeExn (Long module_name (Short cons_name))) ==>
  EvalM env exp1 (MONAD a EXN_TYPE x1) H ==>
  (∀t v.
     TYPE t v ==>
     EvalM (write n v env) exp2 (MONAD a EXN_TYPE (x2 t)) H) ==>
  EvalM env (Handle exp1 [(Pcon (SOME (Short cons_name)) [Pvar n],exp2)])
    (MONAD a EXN_TYPE (handle_fun x1 x2)) H`,
  rw[EvalM_def]
  \\ rw[Once evaluate_cases]
  \\ qpat_x_assum `!s p refs. REFS_PRED H refs p s ==> P` IMP_RES_TAC
  \\ first_x_assum(qspec_then `junk` STRIP_ASSUME_TAC)
  \\ first_x_assum(fn x => MATCH_MP evaluate_unique_result x |> ASSUME_TAC)
  \\ rw[]
  \\ rw[MONAD_def]
  \\ Cases_on `res` >> fs[MONAD_def]
  >> Cases_on `x1 refs` >> fs[]
  >> Cases_on `q` >> fs[]
  >> Cases_on `e` >> fs[]
  \\ rw[]
  \\ Cases_on `?e. b = ECons e`
  >-(
      rw[]
      \\ last_x_assum(qspecl_then[`refs`, `e`, `r`] IMP_RES_TAC)
      \\ last_x_assum(fn x => ALL_TAC)
      \\ rw[]
      \\ rw[Once evaluate_cases]
      \\ last_x_assum IMP_RES_TAC
      \\ last_x_assum(fn x => ALL_TAC)
      \\ rw[]
      \\ fs[pat_bindings_def, pmatch_def]
      \\ fs[lookup_cons_def, same_tid_def,namespaceTheory.id_to_n_def,same_ctor_def]
      \\ IMP_RES_TAC REFS_PRED_FRAME_imp
      \\ qpat_x_assum `!t v. TYPE t v ==> P` IMP_RES_TAC
      \\ first_x_assum(fn x => ALL_TAC)
      \\ first_x_assum(qspec_then `[]` STRIP_ASSUME_TAC)
      \\ fs[with_same_refs]
      \\ fs[write_def]
      \\ first_x_assum(fn x => simp[MATCH_MP evaluate_unique_result x]) (**)
      \\ Cases_on `x2 e r` >> fs[]
      \\ Cases_on `q` >> fs[]
      \\ Cases_on `res` >> fs[]
      >> IMP_RES_TAC REFS_PRED_FRAME_trans
      >> Cases_on `e'` >> fs[])
  \\ rw[]
  \\ last_x_assum(fn x => ALL_TAC)
  \\ last_x_assum(qspec_then `refs` ASSUME_TAC)
  \\ `!e s1. x1 refs <> (Failure (ECons e), s1)` by (rw[] \\ DISJ1_TAC \\ fs[])
  \\ qpat_x_assum `P ==> Q` IMP_RES_TAC
  \\ fs[]
  \\ rw[Once evaluate_cases]
  \\ last_x_assum(fn x => ALL_TAC)
  \\ last_x_assum IMP_RES_TAC
  \\ rw[]
  \\ fs[pat_bindings_def, pmatch_def]
  \\ fs[lookup_cons_def, same_tid_def,namespaceTheory.id_to_n_def,same_ctor_def]
  \\ rw[Once evaluate_cases]);

val EvalM_handle_SIMPLE = Q.store_thm("EvalM_handle_SIMPLE",
 `!cons_name ECons TYPE EXN_TYPE handle_fun H n x1 exp1 x2 exp2 env a.
  (!s e s1. x1 s = (Failure (ECons e), s1) ==> handle_fun x1 x2 s = x2 e s1) ==>
  (!s. (!e s1. x1 s <> (Failure (ECons e), s1)) ==> handle_fun x1 x2 s = x1 s) ==>
  (!e ev. EXN_TYPE (ECons e) ev ==>
   ?ev'.
   ev = Conv (SOME (cons_name,TypeExn (Short cons_name))) [ev'] /\ 
   TYPE e ev') ==>
  (!e ev. EXN_TYPE e ev ==>
   (!e'. e <> ECons e') ==>
   ?ev' cons_name_1.
   ev = Conv (SOME (cons_name_1,TypeExn (Short cons_name_1))) [ev'] /\
   cons_name_1 <> cons_name) ==>
  lookup_cons cons_name env = SOME (1,TypeExn (Short cons_name)) ==>
  EvalM env exp1 (MONAD a EXN_TYPE x1) H ==>
  (∀t v.
     TYPE t v ==>
     EvalM (write n v env) exp2 (MONAD a EXN_TYPE (x2 t)) H) ==>
  EvalM env (Handle exp1 [(Pcon (SOME (Short cons_name)) [Pvar n],exp2)])
    (MONAD a EXN_TYPE (handle_fun x1 x2)) H`,
  rw[EvalM_def]
  \\ rw[Once evaluate_cases]
  \\ qpat_x_assum `!s p refs. REFS_PRED H refs p s ==> P` IMP_RES_TAC
  \\ first_x_assum(qspec_then `junk` STRIP_ASSUME_TAC)
  \\ first_x_assum(fn x => MATCH_MP evaluate_unique_result x |> ASSUME_TAC)
  \\ rw[]
  \\ rw[MONAD_def]
  \\ Cases_on `res` >> fs[MONAD_def]
  >> Cases_on `x1 refs` >> fs[]
  >> Cases_on `q` >> fs[]
  >> Cases_on `e` >> fs[]
  \\ rw[]
  \\ Cases_on `?e. b = ECons e`
  >-(
      rw[]
      \\ last_x_assum(qspecl_then[`refs`, `e`, `r`] IMP_RES_TAC)
      \\ last_x_assum(fn x => ALL_TAC)
      \\ rw[]
      \\ rw[Once evaluate_cases]
      \\ last_x_assum IMP_RES_TAC
      \\ last_x_assum(fn x => ALL_TAC)
      \\ rw[]
      \\ fs[pat_bindings_def, pmatch_def]
      \\ fs[lookup_cons_def, same_tid_def,namespaceTheory.id_to_n_def,same_ctor_def]
      \\ IMP_RES_TAC REFS_PRED_FRAME_imp
      \\ qpat_x_assum `!t v. TYPE t v ==> P` IMP_RES_TAC
      \\ first_x_assum(fn x => ALL_TAC)
      \\ first_x_assum(qspec_then `[]` STRIP_ASSUME_TAC)
      \\ fs[with_same_refs]
      \\ fs[write_def]
      \\ first_x_assum(fn x => simp[MATCH_MP evaluate_unique_result x]) (**)
      \\ Cases_on `x2 e r` >> fs[]
      \\ Cases_on `q` >> fs[]
      \\ Cases_on `res` >> fs[]
      >> IMP_RES_TAC REFS_PRED_FRAME_trans
      >> Cases_on `e'` >> fs[])
  \\ rw[]
  \\ last_x_assum(fn x => ALL_TAC)
  \\ last_x_assum(qspec_then `refs` ASSUME_TAC)
  \\ `!e s1. x1 refs <> (Failure (ECons e), s1)` by (rw[] \\ DISJ1_TAC \\ fs[])
  \\ qpat_x_assum `P ==> Q` IMP_RES_TAC
  \\ fs[]
  \\ rw[Once evaluate_cases]
  \\ last_x_assum(fn x => ALL_TAC)
  \\ last_x_assum IMP_RES_TAC
  \\ rw[]
  \\ fs[pat_bindings_def, pmatch_def]
  \\ fs[lookup_cons_def, same_tid_def,namespaceTheory.id_to_n_def,same_ctor_def]
  \\ rw[Once evaluate_cases]);


(* read and update refs *)
val EvalM_read_heap = Q.store_thm("EvalM_read_heap",
`!vname loc TYPE EXC_TYPE H get_var.
  (nsLookup env.v (Short vname) = SOME (Loc loc)) ==>
  EvalM env (App Opderef [Var (Short vname)])
  (MONAD TYPE EXC_TYPE (λrefs. (Success (get_var refs), refs)))
  (λrefs. REF_REL TYPE (Loc loc) (get_var refs)  * H refs)`,
  rw[EvalM_def, REF_REL_def]
  \\ rw[Once evaluate_cases,evaluate_list_cases,PULL_EXISTS]
  \\ ntac 6 (rw[Once evaluate_cases])
  \\ fs[REFS_PRED_def]
  \\ fs[SEP_CLAUSES, SEP_EXISTS_THM]
  \\ EXTRACT_PURE_FACTS_TAC
  \\ fs[GSYM STAR_ASSOC]
  \\ rw[do_app_def]
  \\ fs[MONAD_def]
  \\ rw[store_lookup_def,EL_APPEND1,EL_APPEND2]
  >-(
      qexists_tac `s with refs := s.refs ++ junk`
      \\ IMP_RES_TAC STATE_EXTRACT_FROM_HPROP_REF
      \\ POP_ASSUM (fn x => fs[x])
      \\ fs[with_same_refs, with_same_ffi]
      \\ fs[REFS_PRED_FRAME_append]
  ) >>
  IMP_RES_TAC st2heap_REF_MEM
  \\ IMP_RES_TAC store2heap_IN_LENGTH
  \\ fs[]);

val EvalM_write_heap = Q.store_thm("EvalM_write_heap",
  `!vname loc TYPE EXC_TYPE H get_var set_var x exp env.
  (!refs x. get_var (set_var x refs) = x) ==>
  (!refs x. H (set_var x refs) = H refs) ==>
  nsLookup env.v (Short vname) = SOME (Loc loc) ==>
  Eval env exp (TYPE x) ==>
  EvalM env (App Opassign [Var (Short vname); exp])
  ((MONAD UNIT_TYPE EXC_TYPE) (λrefs. (Success (), set_var x refs)))
  (λrefs. REF_REL TYPE (Loc loc) (get_var refs) * H refs)`,
  rw[REF_REL_def]
  \\ ASSUME_TAC (Thm.INST_TYPE [``:'a`` |-> ``:'c``, ``:'b`` |-> ``:'a``] Eval_IMP_PURE)
  \\ POP_ASSUM IMP_RES_TAC
  \\ fs[EvalM_def] \\ rw[]
  \\ rw[Once evaluate_cases,evaluate_list_cases,PULL_EXISTS]
  \\ ntac 3 (rw[Once(CONJUNCT2 evaluate_cases)])
  \\ rw[CONJUNCT1 evaluate_cases |> Q.SPECL[`F`,`env`,`s`,`Var _`]]
  \\ srw_tac[DNF_ss][] \\ disj1_tac
  \\ Q.PAT_X_ASSUM `!H. P` IMP_RES_TAC
  \\ first_x_assum(qspec_then `junk` strip_assume_tac)
  \\ fs[PURE_def] \\ rw[]
  \\ asm_exists_tac \\ fs[]
  \\ fs[do_app_def]
  \\ qexists_tac `Rval (Conv NONE [])`
  \\ qexists_tac `set_var x refs`
  \\ qexists_tac `LUPDATE (Refv v) loc (s.refs ++ junk')`
  \\ qexists_tac `s.ffi`
  \\ IMP_RES_TAC (Thm.INST_TYPE [``:'b`` |-> ``:unit``, ``:'c`` |-> ``:unit``] REFS_PRED_FRAME_imp)
  \\ fs[REFS_PRED_def]
  \\ qpat_x_assum `P (st2heap p s)` (fn x => ALL_TAC)
  \\ fs[store_assign_def,EL_APPEND1,EL_APPEND2,store_v_same_type_def]
  \\ fs[SEP_CLAUSES, SEP_EXISTS_THM]
  \\ EXTRACT_PURE_FACTS_TAC
  \\ fs[GSYM STAR_ASSOC] \\ IMP_RES_TAC st2heap_REF_MEM
  \\ IMP_RES_TAC store2heap_IN_LENGTH
  \\ IMP_RES_TAC STATE_EXTRACT_FROM_HPROP_REF
  \\ POP_ASSUM (qspec_then `[]` ASSUME_TAC)
  \\ fs[] \\ POP_ASSUM(fn x => ALL_TAC)
  \\ fs[MONAD_def]
  \\ fs[REFS_PRED_FRAME_def]
  \\ fs[SEP_CLAUSES, SEP_EXISTS_THM]
  \\ rpt STRIP_TAC
  \\ qexists_tac `v`
  \\ qpat_x_assum `!F. P` IMP_RES_TAC
  \\ POP_ASSUM (fn x => ASSUME_TAC (CONV_RULE (RATOR_CONV PURE_FACTS_FIRST_CONV) x))
  \\ CONV_TAC (STRIP_QUANT_CONV (RATOR_CONV PURE_FACTS_FIRST_CONV))
  \\ fs[GSYM STAR_ASSOC, HCOND_EXTRACT]
  \\ fs[LUPDATE_APPEND1,LUPDATE_APPEND2,LUPDATE_def]
  \\ IMP_RES_TAC STATE_UPDATE_HPROP_REF
  \\ last_x_assum(qspec_then `v` ASSUME_TAC)
  \\ fs[with_same_ffi]);

(* Dynamic allocation of references *)

val STATE_REF_def = Define`
STATE_REF A r x = SEP_EXISTS v. REF r v * &A x v`;

val STATE_REFS_def = Define`
STATE_REFS A [] [] = emp /\
STATE_REFS A (r::refs) (x::state) = STATE_REF A r x * STATE_REFS A refs state /\
STATE_REFS A [] (x::state) = &F /\
STATE_REFS A (r::refs) [] = &F`;

val RES_MONAD = Define `RES_MONAD A = MONAD A (\x v. F)`;

val GC_ABSORB_L = Q.prove(`!A B s. (A * B * GC) s ==> (A * GC) s`,
rw[]
\\ fs[GSYM STAR_ASSOC]
\\ fs[Once STAR_def]
\\ qexists_tac `u`
\\ qexists_tac `v`
\\ fs[SAT_GC]);

val GC_ABSORB_R = Q.prove(`!A B s. (A * GC * B) s ==> (A * GC) s`,
rw[]
\\ `A * GC * B = A * B * GC` by metis_tac[STAR_COMM, STAR_ASSOC]
\\ POP_ASSUM(fn x => fs[x])
\\ IMP_RES_TAC GC_ABSORB_L);

(* Validity of a store extension *)
val valid_state_refs_frame_extension = Q.prove(
`!H junk. A (cons x) res ==> (STATE_REFS A ptrs state * H) (st2heap p s) ==>
(STATE_REFS A (Loc (LENGTH (s.refs ++ junk))::ptrs) (cons x::state) * H * GC) (st2heap p (s with refs := s.refs ++ junk ++ [Refv res]))`,
rw[]
\\ rw[Once STATE_REFS_def]
\\ rw[GSYM STAR_ASSOC]
\\ rw[Once STAR_COMM]
\\ rw[STAR_ASSOC]
\\ rw[Once (GSYM STAR_ASSOC)]
\\ rw[Once STAR_def]
\\ qexists_tac `st2heap p s`
\\ qexists_tac `store2heap_aux (LENGTH s.refs) (junk++[Refv res])`
\\ PURE_REWRITE_TAC[GSYM APPEND_ASSOC]
\\ `st2heap p s = st2heap p (s with refs := s.refs)` by fs[with_same_refs]
\\ POP_ASSUM(fn x => PURE_REWRITE_TAC[x])
\\ PURE_REWRITE_TAC[STATE_SPLIT_REFS]
\\ rw[with_same_refs]
\\ rw[STAR_def]
\\ qexists_tac `store2heap_aux (LENGTH s.refs) junk`
\\ qexists_tac `store2heap_aux (LENGTH s.refs + LENGTH junk) [Refv res]`
\\ fs[store2heap_aux_SPLIT]
\\ fs[SAT_GC, STATE_REF_def]
\\ fs[SEP_EXISTS_THM, SEP_CLAUSES]
\\ qexists_tac `res`
\\ EXTRACT_PURE_FACTS_TAC
\\ fs[store2heap_aux_def, REF_def, cell_def, one_def]
\\ fs[SEP_EXISTS_THM, HCOND_EXTRACT]
);

val valid_state_refs_extension = Q.prove(
`A (cons x) res ==> REFS_PRED (STATE_REFS A ptrs) refs p s ==>
REFS_PRED (STATE_REFS A (Loc (LENGTH (s.refs ++ junk))::ptrs)) (cons x ::refs) p (s with refs := s.refs ++ junk ++ [Refv res])`,
rw[REFS_PRED_def, REFS_PRED_FRAME_def]
\\ IMP_RES_TAC valid_state_refs_frame_extension
\\ fs[GSYM STAR_ASSOC, GC_STAR_GC]
);

val STATE_REFS_LENGTH = Q.prove(
`!ptrs state H. (STATE_REFS A ptrs state * H) s ==> LENGTH ptrs = LENGTH state`,
Induct
>-(
    rw[STATE_REFS_def]
    >> Cases_on `state`
    >> fs[STATE_REFS_def]
    >> fs[SEP_CLAUSES, SEP_F_def])
\\ rw[]
\\ Cases_on `state`
>-(
    fs[STATE_REFS_def]
    >> fs[STATE_REFS_def]
    >> fs[SEP_CLAUSES, SEP_F_def])
\\ fs[STATE_REFS_def]
\\ fs[GSYM STAR_ASSOC]
\\ POP_ASSUM(fn x => SIMP_RULE bool_ss [Once STAR_COMM] x |> ASSUME_TAC)
\\ fs[GSYM STAR_ASSOC]
\\ last_x_assum IMP_RES_TAC
);

val valid_state_refs_reduction = Q.prove(
`(STATE_REFS A (rv::ptrs) refs * H * GC) s ==> (STATE_REFS A ptrs (TL refs) * H * GC) s`,
rw[]
\\ fs[GSYM STAR_ASSOC]
\\ IMP_RES_TAC STATE_REFS_LENGTH
\\ Cases_on`refs`
\\ fs[]
\\ fs[STATE_REFS_def]
\\ fs[GSYM STAR_ASSOC]
\\ last_x_assum(fn x => SIMP_RULE bool_ss [Once STAR_COMM] x |> ASSUME_TAC)
\\ fs[STAR_ASSOC]
\\ IMP_RES_TAC GC_ABSORB_R);

(* Validity of ref_bind *)
val EvalM_ref_bind = Q.store_thm("EvalM_ref_bind",
`Eval env xexpr (A (cons x)) ==>
(!rv r. EvalM (write rname rv env) exp (MONAD TYPE MON_EXN_TYPE (f r)) (STATE_REFS A (rv::ptrs))) ==>
EvalM env (Let (SOME rname) (App Opref [xexpr]) exp) (MONAD TYPE MON_EXN_TYPE (ref_bind (Mref cons x) f (Mpop_ref e))) (STATE_REFS A ptrs)`,
rw[]
\\ fs[Eval_def]
\\ rw[EvalM_def]
\\ ntac 3 (rw[Once evaluate_cases])
\\ first_x_assum(qspec_then `s.refs ++ junk` STRIP_ASSUME_TAC)
\\ first_x_assum (fn x => MATCH_MP evaluate_empty_state_IMP_junk x |> STRIP_ASSUME_TAC)
\\ first_x_assum(fn x => simp[MATCH_MP evaluate_unique_result x])
\\ rw[evaluate_list_cases]
\\ rw[do_app_def]
\\ rw[store_alloc_def]
\\ rw[namespaceTheory.nsOptBind_def]
\\ fs[write_def]
\\ last_x_assum(qspec_then `Loc (LENGTH junk + (LENGTH refs' + LENGTH s.refs))` ASSUME_TAC)
\\ first_x_assum(qspec_then `StoreRef (LENGTH refs)` ASSUME_TAC)
\\ fs[with_same_ffi]
\\ fs[EvalM_def]
\\ first_x_assum(qspecl_then [`s with refs := s.refs ++ junk ++ refs' ++ [Refv res]`, `p`, `(cons x)::refs`] ASSUME_TAC)
\\ IMP_RES_TAC valid_state_refs_extension
\\ first_x_assum(qspec_then`junk ++ refs'` ASSUME_TAC)
\\ fs[]
\\ first_x_assum(qspec_then`[]` STRIP_ASSUME_TAC)
\\ fs[]
\\ first_x_assum(fn x => simp[MATCH_MP evaluate_unique_result x])
\\ qexists_tac `s2`
\\ qexists_tac `res'`
\\ fs[]
\\ qexists_tac `TL refs2`
\\ fs[REFS_PRED_FRAME_def]
\\ fs[REFS_PRED_def]
\\ rw[]
>-(fs[MONAD_def]
   >> fs[ref_bind_def]
   >> fs[Mref_def]
   >> fs[Mpop_ref_def]
   >> fs[length_eq]
   >> Cases_on `f (StoreRef (LENGTH refs)) (cons x::refs)` 
   >> fs[]
   >> Cases_on `q`
   >> Cases_on `res'`
   >> Cases_on `r`
   >> fs[]
   >> rw[]
   >-(
	qpat_x_assum `!F. P` IMP_RES_TAC
        >> fs[Once STATE_REFS_def]
	>> fs[SEP_CLAUSES, SEP_F_def])
   >-(
	Cases_on `e'`
        >> fs[]
        >> irule FALSITY
	>> qpat_x_assum `!F. P` IMP_RES_TAC
	>> fs[GSYM STAR_ASSOC] 
	>> IMP_RES_TAC STATE_REFS_LENGTH
        >> rw[]
	>> fs[LENGTH])
   >> Cases_on `e'`
   >> fs[]
   >> rw[])
\\ first_x_assum(qspec_then `F' * GC` ASSUME_TAC)
\\ fs[STAR_ASSOC]
\\ qspecl_then [`F'`, `junk++refs'`] IMP_RES_TAC valid_state_refs_frame_extension
\\ ntac 2 (POP_ASSUM(fn x => ALL_TAC))
\\ fs[]
\\ POP_ASSUM(fn x => ALL_TAC)
\\ fs[GSYM STAR_ASSOC, GC_STAR_GC]
\\ fs[STAR_ASSOC]
\\ IMP_RES_TAC valid_state_refs_reduction);

(* Validity of a deref operation *)
val STATE_REFS_EXTRACT = Q.prove(
`!ptrs1 r ptrs2 refs TYPE H p s. ((STATE_REFS TYPE (ptrs1 ++ [r] ++ ptrs2) refs) * H) (st2heap p s) ==>
((STATE_REFS TYPE ptrs1 (TAKE (LENGTH ptrs1) refs) *
(STATE_REF TYPE r (EL (LENGTH ptrs1) refs)) *
(STATE_REFS TYPE ptrs2 (DROP (LENGTH ptrs1 + 1) refs)) *
H)) (st2heap p s)`,
Induct
>-(
    rw[]
    >> rw[STATE_REFS_def]
    >> rw[SEP_CLAUSES]
    >> rw[GSYM STATE_REFS_def]
    >> IMP_RES_TAC STATE_REFS_LENGTH
    >> Cases_on `refs`
    >> fs[])
\\ rw[]
\\ IMP_RES_TAC STATE_REFS_LENGTH
\\ Cases_on `refs`
\\ fs[]
\\ fs[STATE_REFS_def]
\\ fs[GSYM STAR_ASSOC]
\\ qpat_x_assum `H' (st2heap p s)` (fn x => PURE_ONCE_REWRITE_RULE[GSYM STAR_COMM] x |> ASSUME_TAC)
\\ rw[Once STAR_COMM]
\\ fs[STAR_ASSOC]
\\ qpat_x_assum `H' (st2heap p s)` (fn x => PURE_ONCE_REWRITE_RULE[GSYM STAR_ASSOC] x |> ASSUME_TAC)
\\ rw[Once (GSYM STAR_ASSOC)]
\\ last_x_assum IMP_RES_TAC
\\ fs[SUC_ONE_ADD]);

val STATE_REFS_EXTRACT_2 = Q.prove(
`!ptrs1 r ptrs2 refs1 x refs2 TYPE H p s.
LENGTH ptrs1 = LENGTH refs1 ==>
LENGTH ptrs2 = LENGTH refs2 ==>
(STATE_REFS TYPE (ptrs1 ++ [r] ++ ptrs2) (refs1 ++ [x] ++ refs2) * H) (st2heap p s) ==>
(STATE_REFS TYPE ptrs1 refs1 *
STATE_REF TYPE r x *
STATE_REFS TYPE ptrs2 refs2 *
H) (st2heap p s)`,
rw[]
\\ IMP_RES_TAC STATE_REFS_EXTRACT
\\ sg `TAKE (LENGTH ptrs1) (refs1 ++ [x] ++ refs2) = refs1`
>-(
    last_x_assum (fn x => PURE_REWRITE_TAC[x])
    >> PURE_REWRITE_TAC[GSYM APPEND_ASSOC]
    >> PURE_REWRITE_TAC[TAKE_LENGTH_APPEND]
    >> fs[])
>> sg `EL(LENGTH ptrs1) (refs1 ++ [x] ++ refs2) = x`
>-(
    last_x_assum (fn x => PURE_REWRITE_TAC[x])
    >> PURE_REWRITE_TAC[el_append3]
    >> fs[])
>> sg `DROP (LENGTH ptrs1 + 1) (refs1 ++ [x] ++ refs2) = refs2`
>-(
    `(LENGTH ptrs1 + 1) = LENGTH(refs1 ++ [x])` by rw[]
    >> POP_ASSUM(fn x => PURE_REWRITE_TAC[x])
    >> PURE_REWRITE_TAC[DROP_LENGTH_APPEND]
    >> fs[])
>> metis_tac[]);

val STATE_REFS_RECONSTRUCT = Q.prove(
`!ptrs1 r ptrs2 refs1 y refs2 TYPE H p s.
((STATE_REFS TYPE ptrs1 refs1) *
(STATE_REF TYPE r y) *
(STATE_REFS TYPE ptrs2 refs2) *
H) (st2heap p s) ==>
((STATE_REFS TYPE (ptrs1 ++ [r] ++ ptrs2) (refs1 ++ [y] ++ refs2)) * H) (st2heap p s)`,
Induct
>-(
    rw[]
    >> Cases_on `refs1`
    >> fs[STATE_REFS_def]
    >> fs[SEP_CLAUSES, SEP_F_def])
\\ rw[]
\\ Cases_on `refs1`
\\ fs[]
\\ fs[SUC_ONE_ADD]
\\ fs[STATE_REFS_def, GSYM STAR_ASSOC, HCOND_EXTRACT]
\\ first_x_assum (fn x => PURE_ONCE_REWRITE_RULE[GSYM STAR_COMM] x |> ASSUME_TAC)
\\ rw[Once STAR_COMM]
\\ fs[STAR_ASSOC]
\\ first_x_assum (fn x => PURE_ONCE_REWRITE_RULE[GSYM STAR_ASSOC] x |> ASSUME_TAC)
\\ rw[Once (GSYM STAR_ASSOC)]);

val STATE_REFS_DECOMPOSE = Q.store_thm("STATE_REFS_DECOMPOSE",
`!ptrs1 r ptrs2 refs TYPE H p s. ((STATE_REFS TYPE (ptrs1 ++ [r] ++ ptrs2) refs) * H) (st2heap p s) <=>
?refs1 y refs2.
refs = refs1 ++ [y] ++ refs2 /\
((STATE_REFS TYPE ptrs1 refs1 *
(STATE_REF TYPE r y) *
(STATE_REFS TYPE ptrs2 refs2) *
H)) (st2heap p s)`,
rpt STRIP_TAC
\\ EQ_TAC
>-(     
    rw[]
    >> sg `?refs1 refs'. refs = refs1 ++ refs' /\ LENGTH refs1 = LENGTH ptrs1`
    >-(
	IMP_RES_TAC STATE_REFS_LENGTH
        >> qexists_tac `TAKE (LENGTH ptrs1) refs`
        >> qexists_tac `DROP (LENGTH ptrs1) refs`
        >> rw[TAKE_DROP]
	>> fs[LENGTH_TAKE])
    >> sg `?y refs2. refs' = [y] ++ refs2 /\ LENGTH refs2 = LENGTH ptrs2`
    >-(
	qexists_tac `HD refs'`
        >> qexists_tac `TL refs'`
        >> IMP_RES_TAC STATE_REFS_LENGTH
        >> Cases_on `refs'`
        >> rw[]
        >> fs[])
    >> rw[]
    >> qexists_tac `refs1`
    >> qexists_tac `y`
    >> qexists_tac `refs2`
    >> fs[]
    >> sg `TAKE (LENGTH ptrs1) (refs1 ++ [y] ++ refs2) = refs1`
    >-(
        PURE_REWRITE_TAC[GSYM APPEND_ASSOC]
        >> qpat_x_assum `LENGTH refs1 = X` (fn x => PURE_REWRITE_TAC[GSYM x])
	>> PURE_REWRITE_TAC[TAKE_LENGTH_APPEND]
        >> fs[])
    >> sg `EL(LENGTH ptrs1) (refs1 ++ [y] ++ refs2) = y`
    >-(
	qpat_x_assum `LENGTH refs1 = X` (fn x => PURE_REWRITE_TAC[GSYM x])
        >> PURE_REWRITE_TAC[el_append3]
	>> fs[])
    >> sg `DROP (LENGTH ptrs1 + 1) (refs1 ++ [y] ++ refs2) = refs2`
    >-(
	`(LENGTH ptrs1 + 1) = LENGTH(refs1 ++ [y])` by rw[]
        >> POP_ASSUM(fn x => PURE_REWRITE_TAC[x])
        >> PURE_REWRITE_TAC[DROP_LENGTH_APPEND]
	>> fs[])
    >> IMP_RES_TAC STATE_REFS_EXTRACT
    >> metis_tac[])
\\ rw[]
\\ fs[STATE_REFS_RECONSTRUCT]);

val STATE_REFS_DECOMPOSE_2 = Q.store_thm("STATE_REFS_DECOMPOSE_2",
`!ptrs1 r ptrs2 refs1 x refs2 TYPE H p s.
LENGTH ptrs1 = LENGTH refs1 ==>
LENGTH ptrs2 = LENGTH refs2 ==>
(((STATE_REFS TYPE (ptrs1 ++ [r] ++ ptrs2) (refs1 ++ [x] ++ refs2)) * H) (st2heap p s) <=>
((STATE_REFS TYPE ptrs1 refs1 *
(STATE_REF TYPE r x) *
(STATE_REFS TYPE ptrs2 refs2) *
H)) (st2heap p s))`,
rpt STRIP_TAC
\\ EQ_TAC
>-(     
    rw[]
    >> fs[STATE_REFS_EXTRACT_2])
\\ rw[]
\\ fs[STATE_REFS_RECONSTRUCT]);

val REF_EXISTS_LOC = Q.prove(`(rv ~~> v * H) s ==> ?l. rv = Loc l`,
rw[REF_def, SEP_CLAUSES, SEP_EXISTS_THM, GSYM STAR_ASSOC, HCOND_EXTRACT]);

val store_lookup_CELL_st2heap = Q.store_thm("store_lookup_CELL_st2heap",
`(l ~~>> res * H) (st2heap p s) ==> store_lookup l (s.refs ++ junk) = SOME res`,
rw[]
\\ IMP_RES_TAC STATE_EXTRACT_FROM_HPROP
\\ IMP_RES_TAC st2heap_CELL_MEM
\\ IMP_RES_TAC store2heap_IN_LENGTH
\\ fs[store_lookup_def]);

val store_lookup_REF_st2heap = Q.store_thm("store_lookup_REF_st2heap",
`(Loc l ~~> v * H) (st2heap p s) ==> store_lookup l (s.refs ++ junk) = SOME (Refv v)`,
rw[]
\\ IMP_RES_TAC STATE_EXTRACT_FROM_HPROP_REF
\\ IMP_RES_TAC st2heap_REF_MEM
\\ IMP_RES_TAC store2heap_IN_LENGTH
\\ fs[store_lookup_def]);

val store_lookup_ARRAY_st2heap = Q.store_thm("store_lookup_ARRAY_st2heap",
`(ARRAY (Loc l) av * H) (st2heap p s) ==> store_lookup l (s.refs ++ junk) = SOME (Varray av)`,
rw[]
\\ IMP_RES_TAC STATE_EXTRACT_FROM_HPROP_ARRAY
\\ IMP_RES_TAC st2heap_ARRAY_MEM
\\ IMP_RES_TAC store2heap_IN_LENGTH
\\ fs[store_lookup_def]);

val EvalM_Mdref = Q.store_thm("EvalM_Mdref",
`nsLookup env.v (Short rname) = SOME rv ==>
r = LENGTH ptrs2 ==>
EvalM env (App Opderef [Var (Short rname)])
(MONAD TYPE (\x v. F) (Mdref e (StoreRef r))) (STATE_REFS TYPE (ptrs1 ++ [rv] ++ ptrs2))`,
rw[]
\\ fs[EvalM_def]
\\ rw[]
\\ ntac 20 (rw[Once evaluate_cases])
\\ rw[do_app_def]
\\ fs[REFS_PRED_def]
\\ IMP_RES_TAC STATE_REFS_EXTRACT
\\ fs[GSYM STAR_ASSOC]
\\ POP_ASSUM(fn x => PURE_ONCE_REWRITE_RULE[STAR_COMM] x |> ASSUME_TAC)
\\ fs[STAR_ASSOC]
\\ fs[STATE_REF_def, SEP_CLAUSES, SEP_EXISTS_THM]
\\ EXTRACT_PURE_FACTS_TAC
\\ fs[GSYM STAR_ASSOC]
\\ IMP_RES_TAC REF_EXISTS_LOC
\\ fs[]
\\ IMP_RES_TAC store_lookup_REF_st2heap
\\ fs[]
\\ qexists_tac `s with refs := s.refs ++ junk`
\\ qexists_tac `Rval v`
\\ fs[with_same_ffi]
\\ qexists_tac `refs`
\\ fs[MONAD_def]
\\ `LENGTH ptrs2 < LENGTH refs` by (IMP_RES_TAC STATE_REFS_LENGTH \\ fs[])
\\ fs[Mdref_eq]
\\ fs[dref_def]
\\ `LENGTH refs - (LENGTH ptrs2 + 1) = LENGTH ptrs1` by (IMP_RES_TAC STATE_REFS_LENGTH \\ fs[])
\\ POP_ASSUM(fn x => fs[x])
\\ fs[REFS_PRED_FRAME_def]
\\ rw[]
\\ fs[Once (GSYM with_same_refs)]
\\ fs[STATE_APPEND_JUNK]);

(* Validity of an assigment operation *)
val store_assign_REF_st2heap = Q.store_thm("store_assign_REF_st2heap",
`(Loc l ~~> v * H) (st2heap p s) ==>
store_assign l (Refv res) (s.refs ++ junk) = SOME (LUPDATE (Refv res) l (s.refs ++ junk))`,
rw[]
\\ simp[store_assign_def]
\\ IMP_RES_TAC st2heap_REF_MEM
\\ IMP_RES_TAC store2heap_IN_LENGTH
\\ fs[store_v_same_type_def]
\\ IMP_RES_TAC store2heap_IN_EL
\\ fs[EL_APPEND1]);

val UPDATE_STATE_REFS = Q.prove(
`!ptrs2 l ptrs1 x res TYPE junk refs p s.
TYPE x res ==>
REFS_PRED_FRAME (STATE_REFS TYPE (ptrs1 ++ [Loc l] ++ ptrs2)) p (refs, s)
(ref_assign (LENGTH ptrs2) x refs, s with refs := LUPDATE (Refv res) l (s.refs ++ junk))`,
rw[]
\\ fs[REFS_PRED_def, REFS_PRED_FRAME_def]
\\ rw[]
\\ fs[STATE_REFS_DECOMPOSE]
\\ rw[]
\\ rw[ref_assign_def]
\\ sg `LENGTH ptrs2 = LENGTH refs2`
   >-(fs[Once STAR_COMM, STAR_ASSOC]
      >> fs[Once STAR_COMM]
      >> IMP_RES_TAC STATE_REFS_LENGTH)
\\ fs[lupdate_append2]
\\ fs[STATE_REFS_DECOMPOSE]
\\ fs[GSYM STAR_ASSOC]
\\ IMP_RES_TAC STATE_REFS_LENGTH
\\ fs[STATE_REFS_DECOMPOSE_2]
\\ fs[STAR_ASSOC]
\\ fs[GSYM STAR_ASSOC]
\\ fs[Once STAR_COMM]
\\ fs[GSYM STAR_ASSOC]
\\ fs[STATE_REF_def]
\\ fs[SEP_EXISTS_THM, SEP_CLAUSES]
\\ qexists_tac `res`
\\ EXTRACT_PURE_FACTS_TAC
\\ fs[SEP_CLAUSES]
\\ fs[GSYM STAR_ASSOC]
\\ IMP_RES_TAC STATE_UPDATE_HPROP_REF
\\ POP_ASSUM(qspec_then `res` ASSUME_TAC)
\\ IMP_RES_TAC st2heap_REF_MEM
\\ IMP_RES_TAC store2heap_IN_LENGTH
\\ IMP_RES_TAC STATE_APPEND_JUNK
\\ fs[LUPDATE_APPEND1]
\\ metis_tac[STAR_ASSOC, STAR_COMM]);

val EvalM_Mref_assign = Q.store_thm("EvalM_Mref_assign",
`nsLookup env.v (Short rname) = SOME rv ==>
r = LENGTH ptrs2 ==>
Eval env xexpr (TYPE x) ==>
EvalM env (App Opassign [Var (Short rname); xexpr])
(MONAD UNIT_TYPE (\x v. F) (Mref_assign e (StoreRef r) x)) (STATE_REFS TYPE (ptrs1 ++ [rv] ++ ptrs2))`,
rw[]
\\ fs[EvalM_def]
\\ ntac 2 (rw[Once evaluate_cases])
\\ fs[Eval_def]
\\ first_x_assum(qspec_then `s.refs++junk` STRIP_ASSUME_TAC)
\\ first_x_assum (fn x => MATCH_MP evaluate_empty_state_IMP_junk x |> STRIP_ASSUME_TAC)
\\ first_x_assum(fn x => simp[MATCH_MP evaluate_unique_result x])
\\ rw[evaluate_list_cases]
\\ fs[REFS_PRED_def]
\\ IMP_RES_TAC STATE_REFS_EXTRACT
\\ fs[GSYM STAR_ASSOC]
\\ POP_ASSUM(fn x => PURE_ONCE_REWRITE_RULE[STAR_COMM] x |> ASSUME_TAC)
\\ fs[STATE_REF_def, SEP_CLAUSES, SEP_EXISTS_THM]
\\ EXTRACT_PURE_FACTS_TAC
\\ fs[GSYM STAR_ASSOC]
\\ IMP_RES_TAC REF_EXISTS_LOC
\\ rw[Once evaluate_cases]
\\ rw[do_app_def]
\\ IMP_RES_TAC store_assign_REF_st2heap
\\ PURE_REWRITE_TAC[GSYM APPEND_ASSOC]
\\ POP_ASSUM(fn x => simp[x])
\\ qexists_tac `s with refs := LUPDATE (Refv res) l (s.refs ++ junk ++ refs')`
\\ qexists_tac `Rval (Conv NONE [])`
\\ fs[state_component_equality]
\\ qexists_tac `ref_assign (LENGTH ptrs2) x refs`
\\ `s.refs ++ junk ++ refs' = s.refs ++ (junk ++ refs')` by fs[]
\\ POP_ASSUM(fn x => PURE_REWRITE_TAC[x])
\\ fs[UPDATE_STATE_REFS]
\\ fs[MONAD_def]
\\ IMP_RES_TAC STATE_REFS_LENGTH
\\ fs[Mref_assign_eq]);

(* Resizable arrays *)
val ABS_NUM_EQ = Q.prove(`Num(ABS(&n))=n`,
rw[DB.fetch "integer" "Num", integerTheory.INT_ABS]);

val RARRAY_def = Define `
RARRAY rv av = SEP_EXISTS arv. REF rv arv * ARRAY arv av`;

val RARRAY_REL_def = Define `
RARRAY_REL TYPE rv l = SEP_EXISTS av. RARRAY rv av * &LIST_REL TYPE l av`;

val evaluate_Opdref_REF = Q.prove(
`nsLookup env.v (Short vname) = SOME (Loc loc) ==>
(REF (Loc loc) v * H refs) (st2heap p s) ==>
!junk. evaluate F env (s with refs := s.refs ++ junk) (App Opderef [Var (Short vname)]) (s with refs := s.refs ++ junk, Rval v)`,
rw[] 
\\ rw[Once evaluate_cases]
\\ CONV_TAC SWAP_EXISTS_CONV
\\ qexists_tac `s with refs := s.refs ++ junk`
\\ fs[state_component_equality]
\\ rw[Once evaluate_cases, evaluate_list_cases]
\\ rw[do_app_def]
\\ IMP_RES_TAC store_lookup_REF_st2heap
\\ fs[]);

val do_app_Alength_ARRAY = Q.prove(
`(ARRAY rv v * H) (st2heap p (s with refs := s.refs ++ junk)) ==>
do_app (s.refs ++ junk, s.ffi) Alength [rv] =
SOME ((s.refs ++ junk, s.ffi), Rval (Litv(IntLit(int_of_num(LENGTH v)))))`,
rw[do_app_def]
\\ fs[ARRAY_def, SEP_CLAUSES, SEP_EXISTS_THM]
\\ fs[GSYM STAR_ASSOC, HCOND_EXTRACT]
\\ IMP_RES_TAC store_lookup_CELL_st2heap
\\ first_x_assum(qspec_then `[]` ASSUME_TAC)
\\ fs[]);

val EvalM_Marray_length = Q.store_thm("EvalM_Marray_length",
  `!vname loc TYPE EXC_TYPE H get_arr x env.
    nsLookup env.v (Short vname) = SOME (Loc loc) ==>
    EvalM env (App Alength [App Opderef [Var (Short vname)]])
    ((MONAD NUM EXC_TYPE) (Marray_length get_arr x))
    (λrefs. RARRAY_REL TYPE (Loc loc) (get_arr refs) * H refs)`,
  rw[EvalM_def]
  \\ rw[Once evaluate_cases,evaluate_list_cases,PULL_EXISTS]
  \\ fs[REFS_PRED_def, RARRAY_REL_def, RARRAY_def]
  \\ fs[SEP_CLAUSES, SEP_EXISTS_THM]
  \\ EXTRACT_PURE_FACTS_TAC
  \\ fs[GSYM STAR_ASSOC]
  \\ IMP_RES_TAC evaluate_Opdref_REF
  \\ first_x_assum(qspec_then `junk` ASSUME_TAC)
  \\ first_x_assum(fn x => simp[MATCH_MP evaluate_unique_result x])
  \\ rw[Marray_length_def]
  \\ fs[MONAD_def]
  \\ qexists_tac `s with refs := s.refs ++ junk`
  \\ fs[state_component_equality]
  \\ fs[STAR_ASSOC]
  \\ fs[Once (GSYM with_same_refs)]
  \\ IMP_RES_TAC STATE_APPEND_JUNK
  \\ fs[GSYM STAR_ASSOC, GC_STAR_GC]
  \\ first_x_assum(qspec_then `junk` ASSUME_TAC)
  \\ fs[Once (GSYM STAR_COMM)]
  \\ fs[GSYM STAR_ASSOC]
  \\ IMP_RES_TAC do_app_Alength_ARRAY
  \\ POP_ASSUM(fn x => fs[x])
  \\ qexists_tac `Rval (Litv (IntLit (&LENGTH av)))`
  \\ fs[]
  \\ IMP_RES_TAC LIST_REL_LENGTH
  \\ fs[length_eq]
  \\ fs[REFS_PRED_FRAME_append]);

val do_app_Asub_ARRAY = Q.prove(
`(ARRAY rv v * H) (st2heap p (s with refs := s.refs ++ junk)) ==>
do_app (s.refs ++ junk, s.ffi) Asub [rv; Litv (IntLit (&n))] =
if n < LENGTH v then SOME ((s.refs ++ junk, s.ffi), Rval (EL n v))
else SOME ((s.refs ++ junk, s.ffi), Rerr (Rraise (prim_exn "Subscript")))`,
rw[do_app_def]
\\ fs[ARRAY_def, SEP_CLAUSES, SEP_EXISTS_THM]
\\ fs[GSYM STAR_ASSOC, HCOND_EXTRACT]
\\ IMP_RES_TAC store_lookup_CELL_st2heap
\\ first_x_assum(qspec_then `[]` ASSUME_TAC)
\\ fs[ABS_NUM_EQ]);

val EvalM_Marray_sub = Q.store_thm("EvalM_Marray_sub",
  `!vname loc TYPE EXC_TYPE H get_arr e rexp env n nexp.
   nsLookup env.v (Short vname) = SOME (Loc loc) ==>
   lookup_cons "Subscript" env = SOME (0,TypeExn (Short "Subscript")) ==>
   Eval env nexp (NUM n) ==>
   Eval env rexp (EXC_TYPE e) ==>
   EvalM env (Handle (App Asub [App Opderef [Var (Short vname)]; nexp])
              [(Pcon (SOME (Short("Subscript"))) [], Raise rexp)])
   ((MONAD TYPE EXC_TYPE) (Marray_sub get_arr e n))
   (λrefs. RARRAY_REL TYPE (Loc loc) (get_arr refs) * H refs)`,
  rw[EvalM_def]
  \\ rw[Once evaluate_cases,evaluate_list_cases,PULL_EXISTS]
  \\ fs[Eval_def, NUM_def, INT_def]
  \\ first_assum(fn x => SIMP_RULE bool_ss [REFS_PRED_def, RARRAY_def, RARRAY_REL_def] x |> ASSUME_TAC)
  \\ fs[SEP_EXISTS_THM, SEP_CLAUSES, GSYM STAR_ASSOC]
  \\ rw[Once evaluate_cases,evaluate_list_cases]
  \\ last_assum(qspec_then `s.refs ++ junk` STRIP_ASSUME_TAC)
  \\ first_x_assum(fn x => MATCH_MP evaluate_empty_state_IMP_junk x |> STRIP_ASSUME_TAC)
  \\ first_assum(fn x => simp[MATCH_MP evaluate_unique_result x])
  \\ fs[GSYM STAR_ASSOC]
  \\ IMP_RES_TAC evaluate_Opdref_REF
  \\ POP_ASSUM(qspec_then `junk++refs'` ASSUME_TAC)
  \\ fs[]
  \\ first_assum(fn x => simp[MATCH_MP evaluate_unique_result x])
  \\ fs[STAR_ASSOC]
  \\ EXTRACT_PURE_FACTS_TAC
  \\ IMP_RES_TAC LIST_REL_LENGTH
  \\ rw[]
  \\ rw[Once evaluate_cases,evaluate_list_cases]
  \\ fs[Once (GSYM with_same_refs)]
  \\ first_x_assum(fn x => MATCH_MP STATE_APPEND_JUNK x |> ASSUME_TAC)
  \\ POP_ASSUM(fn x => PURE_REWRITE_RULE [GSYM STAR_ASSOC, GC_STAR_GC] x |> ASSUME_TAC)
  \\ fs[Once STAR_COMM]
  \\ fs[GSYM STAR_ASSOC]
  \\ POP_ASSUM(qspec_then `junk ++ refs'` ASSUME_TAC)
  \\ IMP_RES_TAC do_app_Asub_ARRAY
  \\ last_assum(fn x => simp[MATCH_MP evaluate_unique_result x])
  \\ Cases_on `n < LENGTH (get_arr refs)`
  >-(fs[]
     \\ fs[MONAD_def, Marray_sub_def]
     \\ qexists_tac `s with refs := s.refs ++ junk ++ refs'`
     \\ qexists_tac `Rval (EL n av)`
     \\ fs[state_component_equality]
     \\ fs[Msub_eq]
     \\ fs[LIST_REL_EL_EQN]
     \\ PURE_REWRITE_TAC[GSYM APPEND_ASSOC, REFS_PRED_FRAME_append])
  \\ fs[]
  \\ first_assum(fn x => simp[MATCH_MP evaluate_unique_result x])
  \\ qpat_x_assum `evaluate a0 a1 a2 a3 a4` (fn x => SIMP_RULE pure_ss [GSYM APPEND_ASSOC] x |> ASSUME_TAC)
  \\ PURE_REWRITE_TAC[GSYM APPEND_ASSOC]
  \\ first_assum(fn x => simp[MATCH_MP evaluate_unique_result x])
  \\ rw[Once evaluate_cases]
  \\ last_assum(fn x => simp[MATCH_MP evaluate_unique_result x])
  \\ rw[Once evaluate_cases]
  \\ first_assum(fn x => MATCH_MP evaluate_unique_result x |> ASSUME_TAC)
  \\ fs[]
  \\ rw[Once evaluate_cases]
  \\ rw[Once evaluate_cases]
  \\ rw[prim_exn_def]
  \\ fs[lookup_cons_def]
  \\ fs[same_tid_def,namespaceTheory.id_to_n_def,same_ctor_def]
  \\ rw[pat_bindings_def]
  \\ rw[pmatch_def]
  \\ fs[same_tid_def,namespaceTheory.id_to_n_def,same_ctor_def]
  \\ rw[Once evaluate_cases]
  \\ first_assum(qspec_then `s.refs ++ (junk ++ refs')` STRIP_ASSUME_TAC)
  \\ first_x_assum(fn x => MATCH_MP evaluate_empty_state_IMP_junk x |> STRIP_ASSUME_TAC)
  \\ fs[with_same_ffi]
  \\ first_assum(fn x => simp[MATCH_MP evaluate_unique_result x])
  \\ fs[]
  \\ qexists_tac `s with refs := s.refs ++ junk ++ refs' ++ refs''`
  \\ qexists_tac `Rerr (Rraise res)`
  \\ fs[state_component_equality]
  \\ fs[MONAD_def, Marray_sub_def, Msub_exn_eq]
  \\ PURE_REWRITE_TAC[GSYM APPEND_ASSOC]
  \\ rw[REFS_PRED_FRAME_append]);
  
val EvalM_Marray_update = Q.store_thm("EvalM_Marray_update",
  `!vname loc TYPE EXC_TYPE H get_arr set_arr e rexp env n x xexp nexp.
   nsLookup env.v (Short vname) = SOME (Loc loc) ==>
   lookup_cons "Subscript" env = SOME (0,TypeExn (Short "Subscript")) ==>
   (!refs x. get_arr (set_arr x refs) = x) ==>
   (!refs x. H (set_arr x refs) = H refs) ==>
   Eval env nexp (NUM n) ==>
   Eval env rexp (EXC_TYPE e) ==>
   Eval env xexp (TYPE x) ==>
   EvalM env (Handle (App Aupdate [App Opderef [Var (Short vname)]; nexp; xexp])
              [(Pcon (SOME (Short("Subscript"))) [], Raise rexp)])
   ((MONAD UNIT_TYPE EXC_TYPE) (Marray_update get_arr set_arr e n x))
   (λrefs. RARRAY_REL TYPE (Loc loc) (get_arr refs) * H refs)`,
  rw[EvalM_def]
  \\ fs[Eval_def, NUM_def, INT_def]
  \\ rw[Once evaluate_cases,evaluate_list_cases,PULL_EXISTS]
  \\ rw[Once evaluate_cases]
  \\ rw[Once evaluate_cases]
  \\ first_assum(fn x => SIMP_RULE bool_ss [REFS_PRED_def, RARRAY_def, RARRAY_REL_def] x |> ASSUME_TAC)
  \\ fs[SEP_EXISTS_THM, SEP_CLAUSES, GSYM STAR_ASSOC]
  \\ first_x_assum(qspec_then `s.refs ++ junk` STRIP_ASSUME_TAC)
  \\ first_x_assum(fn x => MATCH_MP evaluate_empty_state_IMP_junk x |> STRIP_ASSUME_TAC)
  \\ first_assum(fn x => MATCH_MP evaluate_unique_result x |> ASSUME_TAC)
  \\ fs[]
  \\ rw[Once evaluate_cases]
  \\ last_x_assum(qspec_then `s.refs ++ (junk ++ refs')` STRIP_ASSUME_TAC)
  \\ first_x_assum(fn x => MATCH_MP evaluate_empty_state_IMP_junk x |> STRIP_ASSUME_TAC)
  \\ first_assum(fn x => MATCH_MP evaluate_unique_result x |> ASSUME_TAC)
  \\ fs[]
  \\ rw[]
  \\ IMP_RES_TAC evaluate_Opdref_REF
  \\ first_x_assum(qspec_then `junk ++ refs' ++ refs''` ASSUME_TAC)
  \\ first_x_assum(fn x => MATCH_MP evaluate_unique_result x |> ASSUME_TAC)
  \\ rw[Once evaluate_list_cases]
  \\ fs[]
  \\ rw[Once evaluate_list_cases]
  \\ fs[Once STAR_COMM]
  \\ fs[GSYM STAR_ASSOC]
  \\ fs[Once (GSYM with_same_refs)]
  \\ IMP_RES_TAC STATE_APPEND_JUNK
  \\ POP_ASSUM(qspec_then`junk++refs'++refs''` ASSUME_TAC o (PURE_REWRITE_RULE[GSYM STAR_ASSOC]))
  \\ Cases_on `n < LENGTH av`
  >-(
      rw[do_app_def]
      >> fs[ARRAY_def, SEP_EXISTS_THM, SEP_CLAUSES]
      >> EXTRACT_PURE_FACTS_TAC
      >> rw[]
      >> IMP_RES_TAC LIST_REL_LENGTH
      >> fs[GSYM STAR_ASSOC]
      >> IMP_RES_TAC store_lookup_CELL_st2heap
      >> POP_ASSUM(fn x => ALL_TAC)
      >> POP_ASSUM(qspec_then `[]` ASSUME_TAC)
      >> fs[ABS_NUM_EQ]
      >> IMP_RES_TAC st2heap_CELL_MEM
      >> IMP_RES_TAC store2heap_IN_LENGTH
      >> fs[store_assign_def, store_v_same_type_def]
      >> IMP_RES_TAC store2heap_IN_EL
      >> fs[]
      >> qexists_tac `s with refs := LUPDATE (Varray (LUPDATE res n av)) loc'
         (s.refs ++ junk ++ refs' ++ refs'')`
      >> fs[state_component_equality]
      >> qexists_tac `Rval (Conv NONE [])`
      >> rw[]
      >> qexists_tac `set_arr (LUPDATE x n (get_arr refs)) refs`
      >> fs[MONAD_def, Marray_update_def, Mupdate_eq]
      >> fs[REFS_PRED_FRAME_def]
      >> rw[]
      >> fs[Once (GSYM with_same_refs)]
      >> POP_ASSUM(fn x => MATCH_MP STATE_APPEND_JUNK x |> ASSUME_TAC)
      >> POP_ASSUM(qspec_then`junk++refs'++refs''` ASSUME_TAC)
      >> fs[GSYM STAR_ASSOC]
      >> fs[Once STAR_COMM]
      >> fs[RARRAY_def, RARRAY_REL_def, SEP_CLAUSES, SEP_EXISTS_THM]
      >> fs[STAR_ASSOC, PULL_EXISTS]
      >> qexists_tac `LUPDATE res n av`
      >> qexists_tac `arv`
      >> EXTRACT_PURE_FACTS_TAC
      >> fs[EVERY2_LUPDATE_same]
      >> fs[GSYM STAR_ASSOC]
      >> fs[Once STAR_COMM]
      >> sg `arv  = Loc loc'`
      >-(fs[STAR_ASSOC]
	 >> POP_ASSUM(fn x => PURE_REWRITE_RULE[Once STAR_COMM] x |> ASSUME_TAC)
	 >> fs[GSYM STAR_ASSOC]
	 >> qpat_x_assum `(GC * H1) X` (fn x => PURE_REWRITE_RULE[Once STAR_COMM] x |> ASSUME_TAC)
	 >> fs[GSYM STAR_ASSOC]
	 >> fs[REF_def, SEP_EXISTS_THM, SEP_CLAUSES, GSYM STAR_ASSOC, HCOND_EXTRACT]
	 >> IMP_RES_TAC UNIQUE_CELLS
	 >> rw[])
      >> rw[]
      >> fs[GSYM STAR_ASSOC]
      >> IMP_RES_TAC STATE_UPDATE_HPROP_ARRAY
      >> POP_ASSUM(qspec_then `LUPDATE res n av` ASSUME_TAC)
      >> fs[])
  \\ rw[do_app_def]
  \\ fs[ARRAY_def, SEP_EXISTS_THM, SEP_CLAUSES]
  \\ EXTRACT_PURE_FACTS_TAC
  \\ rw[]
  \\ IMP_RES_TAC LIST_REL_LENGTH
  \\ fs[GSYM STAR_ASSOC]
  \\ IMP_RES_TAC store_lookup_CELL_st2heap
  \\ POP_ASSUM(fn x => ALL_TAC)
  \\ POP_ASSUM(qspec_then `[]` ASSUME_TAC)
  \\ fs[ABS_NUM_EQ]
  \\ rw[Once evaluate_cases, evaluate_list_cases]
  \\ reverse(rw[do_app_def]) >-(irule FALSITY >> fs[ABS_NUM_EQ])
  \\ ntac 4 (rw[Once evaluate_cases])
  \\ rw[prim_exn_def]
  \\ rw[Once evaluate_cases]
  \\ fs[lookup_cons_def]
  \\ fs[same_tid_def,namespaceTheory.id_to_n_def,same_ctor_def]
  \\ rw[pat_bindings_def]
  \\ rw[pmatch_def]
  \\ fs[same_tid_def,namespaceTheory.id_to_n_def,same_ctor_def]
  \\ rw[Once evaluate_cases]
  \\ fs[with_same_ffi]
  \\ last_x_assum(qspec_then `s.refs ++ (junk ++ refs' ++ refs'')` STRIP_ASSUME_TAC)
  \\ first_x_assum(fn x => MATCH_MP evaluate_empty_state_IMP_junk x |> STRIP_ASSUME_TAC)
  \\ fs[]
  \\ first_assum(fn x => simp[MATCH_MP evaluate_unique_result x])
  \\ qexists_tac `s with refs := s.refs ++ junk ++ refs' ++ refs'' ++ refs'''`
  \\ qexists_tac `Rerr (Rraise res')`
  \\ fs[state_component_equality]
  \\ fs[MONAD_def, Marray_update_def, Mupdate_exn_eq]
  \\ PURE_REWRITE_TAC[GSYM APPEND_ASSOC, REFS_PRED_FRAME_append]);

val HPROP_TO_GC_R = Q.prove(`(A * B) s ==> (A * GC) s`,
rw[STAR_def]
\\ qexists_tac `u`
\\ qexists_tac `v`
\\ fs[SAT_GC]);

val HPROP_TO_GC_L = Q.prove(`(A * B) s ==> (GC * B) s`,
rw[STAR_def]
\\ qexists_tac `u`
\\ qexists_tac `v`
\\ fs[SAT_GC]);
 
val EvalM_Marray_alloc = Q.store_thm("EvalM_Marray_alloc",
  `!vname loc TYPE EXC_TYPE H get_arr set_arr n x env nexp xexp.
   nsLookup env.v (Short vname) = SOME (Loc loc) ==>
   (!refs x. get_arr (set_arr x refs) = x) ==>
   (!refs x. H (set_arr x refs) = H refs) ==>
   Eval env nexp (NUM n) ==>
   Eval env xexp (TYPE x) ==>
   EvalM env (App Opassign [Var (Short vname); App Aalloc [nexp; xexp]])
   ((MONAD UNIT_TYPE EXC_TYPE) (Marray_alloc set_arr n x))
   (λrefs. RARRAY_REL TYPE (Loc loc) (get_arr refs) * H refs)`,
  rw[EvalM_def]
  \\ fs[Eval_def, NUM_def, INT_def]
  \\ rw[Once evaluate_cases,evaluate_list_cases,PULL_EXISTS]
  \\ rw[Once evaluate_cases]
  \\ rw[Once evaluate_cases]
  \\ first_x_assum(qspec_then `s.refs ++ junk` STRIP_ASSUME_TAC)
  \\ first_x_assum(fn x => MATCH_MP evaluate_empty_state_IMP_junk x |> STRIP_ASSUME_TAC)
  \\ first_x_assum(fn x => simp[MATCH_MP evaluate_unique_result x])
  \\ rw[Once evaluate_cases]
  \\ first_x_assum(qspec_then `s.refs ++ (junk ++ refs')` STRIP_ASSUME_TAC)
  \\ first_x_assum(fn x => MATCH_MP evaluate_empty_state_IMP_junk x |> STRIP_ASSUME_TAC)
  \\ fs[]
  \\ first_x_assum(fn x => simp[MATCH_MP evaluate_unique_result x])
  \\ rw[Once evaluate_cases]
  \\ rw[do_app_def]
  \\ rw[store_alloc_def]
  \\ rw[with_same_ffi]
  \\ rw[Once evaluate_cases]
  \\ qpat_x_assum `REFS_PRED H1 refs p s` (fn x => PURE_REWRITE_RULE[REFS_PRED_def, RARRAY_def, RARRAY_REL_def] x |> ASSUME_TAC)
  \\ fs[SEP_EXISTS_THM, SEP_CLAUSES]
  \\ fs[Once (GSYM with_same_refs)]
  \\ fs[GSYM STAR_ASSOC]
  \\ IMP_RES_TAC store_assign_REF_st2heap
  \\ PURE_REWRITE_TAC[GSYM APPEND_ASSOC]
  \\ fs[]
  \\ Q.PAT_ABBREV_TAC `loc' = LENGTH junk + L`
  \\ Q.PAT_ABBREV_TAC `srefs = A ++ [Varray X]`
  \\ qexists_tac `s with refs := LUPDATE (Refv (Loc loc')) loc srefs`
  \\ qexists_tac `Rval (Conv NONE [])`
  \\ fs[state_component_equality]
  \\ fs[MONAD_def, Marray_alloc_def]
  \\ rw[REFS_PRED_FRAME_def]
  \\ fs[RARRAY_def, RARRAY_REL_def, SEP_EXISTS_THM, SEP_CLAUSES]
  \\ qexists_tac `REPLICATE (Num (ABS (&n))) res`
  \\ qexists_tac `Loc loc'`
  \\ qpat_x_assum `Abbrev X` (fn x => fs[PURE_REWRITE_RULE[markerTheory.Abbrev_def] x])
  \\ IMP_RES_TAC st2heap_REF_MEM
  \\ IMP_RES_TAC store2heap_IN_LENGTH
  \\ fs[with_same_refs]
  \\ PURE_REWRITE_TAC[GSYM APPEND_ASSOC]
  \\ fs[LUPDATE_APPEND1]
  \\ rw[GSYM STAR_ASSOC]
  \\ rw[Once STAR_COMM]
  \\ rw[GSYM STAR_ASSOC]
  \\ rw[Once STAR_def]
  \\ qexists_tac `store2heap_aux (LENGTH(LUPDATE (Refv (Loc loc')) loc s.refs ++ junk ++ refs' ++ refs'')) [Varray (REPLICATE (Num (ABS (&n))) res)]`
  \\ qexists_tac `st2heap p (s with
        refs := LUPDATE (Refv (Loc loc')) loc s.refs ++ junk ++ refs' ++ refs'')`
  \\ PURE_REWRITE_TAC[Once SPLIT_SYM]
  \\ fs[STATE_SPLIT_REFS]
  \\ simp[ARRAY_def, store2heap_aux_def, SEP_EXISTS_THM, GSYM STAR_ASSOC, HCOND_EXTRACT, cell_def, one_def]
  \\ simp[LIST_REL_REPLICATE_same, ABS_NUM_EQ, replicate_eq]
  \\ rw[STAR_ASSOC, Once STAR_COMM]
  \\ EXTRACT_PURE_FACTS_TAC
  \\ sg `(Loc loc ~~> arv' * H refs * F' * GC) (st2heap p s)`
  >-(fs[GSYM STAR_ASSOC]
     \\ fs[Once STAR_COMM]
     \\ fs[GSYM STAR_ASSOC]
     \\ ntac 2 (POP_ASSUM (fn x => ALL_TAC))
     \\ POP_ASSUM(fn x => MATCH_MP HPROP_TO_GC_L x |> ASSUME_TAC)
     \\ metis_tac[STAR_ASSOC, STAR_COMM])
  \\ fs[GSYM STAR_ASSOC]
  \\ first_x_assum(fn x => MATCH_MP (GEN_ALL STATE_UPDATE_HPROP_REF) x |> ASSUME_TAC)
  \\ first_x_assum(qspec_then `Loc loc'` ASSUME_TAC)
  \\ fs[Once (GSYM with_same_refs)]
  \\ first_x_assum(fn x => MATCH_MP STATE_APPEND_JUNK x |> ASSUME_TAC)
  \\ POP_ASSUM(qspec_then `junk ++ refs' ++ refs''` ASSUME_TAC)
  \\ fs[GSYM STAR_ASSOC, GC_STAR_GC]);

(* TODO: implement a resize pattern *)
(* val array_copy_v = ``
Letrec [("array_copy", "d",
Fun "n" (Fun "x" (Fun "src" (Fun "dst" (
If (App (Opb Lt) [Var (Short "x"); Lit(IntLit 0)])
(Con NONE [])
(Let NONE (App Aupdate [Var (Short "dst"); App (Opn Plus) [Var (Short "n"); Var (Short "d")];
           App Asub [Var (Short "src")]])
(App Opapp[
App Opapp[
App Opapp[
App Opapp [App Opapp [Var (Short "array_copy"); App (Opn Plus) [Var (Short "d"); Lit(IntLit 1)]];
           App (Opn Minus) [Var (Short "n"); Lit(IntLit 1)]];
Var (Short "x")];
Var (Short "src")];
Var (Short "dest")]))
)))))] (Var (Short "array_copy"))
``; *)

(* TODO: implement support for n-dimensional arrays? *)
val ARRAY2D_def = Define `
ARRAY2D av l = SEP_EXISTS fl. ARRAY av fl * &(fl = FLAT l)`;

val RARRAY2D_def = Define `
RARRAY2D rv l = SEP_EXISTS av. REF rv av * ARRAY2D av l`;

val _ = (print_asts := true);

val _ = export_theory();
