(*
  Normalizes pbc into npbc
*)
open preamble pbcTheory npbcTheory;

val _ = new_theory "pbc_normalise";

val _ = numLib.prefer_num();

(* Normalization proceeds in three steps (for string variables):

  'a pbc -> string pbc (for graph encoders) ~> print out
  string pbc -> int pbc
  int pbc -> int pbc with ≥
  int pbc with ≥ -> npbc

  ----
  'a pbc -> 'a pbc with ≥ ~> print out
  'a pbc with ≥ -> string pbc (for graph encoders)


  There is a builtin string normalization using hashing for
  the supported characters

  TODO: normalise string names using a hashmap or similar...
*)

(*
  Injective mapping from mlstring into num, supports

  a-z, A-Z, 0-9, _ -

  EVAL ``MAP ORD (explode (strlit "_-"))``
*)
Definition hashNon_def:
  hashNon n =
  if n = 45 then 63
  else if n = 95 then 64
  else 0
End

Definition hashChar_def:
  hashChar c =
  let oc = ORD c in
  if 48 ≤ oc ∧ oc ≤ 57 (* char 0 to 9 hashes to 1-10 respectively *)
  then oc - 47
  else if 65 ≤ oc ∧ oc ≤ 90 (* char A to Z hashes to 11-36 *)
  then oc - 54
  else if 97 ≤ oc ∧ oc ≤ 122 (* char a to z hashes to 37-62 *)
  then oc - 60
  else hashNon oc
End

Definition hashChars_alt_def:
  (hashChars_alt [] = 0) ∧
  (hashChars_alt (c::cs) =
    hashChar c + 65 * hashChars_alt cs)
End

Definition hashString_def:
  hashString s = hashChars_alt (explode s)
End

(* Kind of a circular definition ... *)
Definition goodChar_def:
  goodChar c ⇔ hashChar c ≠ 0
End

Definition goodChars_def:
  (goodChars 0 str = T) ∧
  (goodChars (SUC n) str =
    (goodChar (strsub str n) ∧
    goodChars n str))
End

Definition goodString_def:
  goodString str = goodChars (strlen str) str
End

Theorem hashString_INJ:
  INJ hashString goodString UNIV
Proof
  rw[INJ_DEF,SPECIFICATION,goodString_def,goodChars_def,hashString_def]>>
  cheat
QED

Definition convert_pbf_def:
  convert_pbf pbf = MAP (map_pbc hashString) pbf
End

Theorem convert_pbf_thm:
  pbf_vars (set pbf) ⊆ goodString ⇒
  (satisfiable (set (convert_pbf pbf)) ⇔ satisfiable (set pbf))
Proof
  rw[]>>
  simp[convert_pbf_def,LIST_TO_SET_MAP]>>
  match_mp_tac satisfiable_INJ_iff>>
  match_mp_tac INJ_SUBSET>>
  first_x_assum (irule_at Any)>>
  metis_tac[hashString_INJ,SUBSET_REFL]
QED

Definition flip_coeffs_def:
  flip_coeffs xs = MAP (λ(c,l). (-c:int,l)) xs
End

(* Convert a list of pbc to one with ≥ constraints only *)
Definition pbc_ge_def:
  (pbc_ge ((GreaterEqual,xs,n):'a pbc) = [(GreaterEqual,xs,n)]) ∧
  (pbc_ge (Greater,xs,n) = [(GreaterEqual,xs,(n+1))]) ∧
  (pbc_ge (LessEqual,xs,n) = [(GreaterEqual,flip_coeffs xs,-n)]) ∧
  (pbc_ge (Less,xs,n) = [(GreaterEqual,flip_coeffs xs,-(n-1))]) ∧
  (pbc_ge (Equal,xs,n) =
      [(GreaterEqual,xs,n); (GreaterEqual,flip_coeffs xs,(-n))])
End

Theorem eq_disj:
  (∀x. x = a ∨ x = b ⇒ P x) ⇔ P a ∧ P b
Proof
  metis_tac[]
QED

Theorem eval_lin_term_flip_coeffs:
  ∀f.
  eval_lin_term w (flip_coeffs f) =
  -eval_lin_term w f
Proof
  Induct>>fs[eval_lin_term_def,iSUM_def,flip_coeffs_def]>>
  Cases_on`h`>>rw[]>>
  Cases_on`r`>>rw[]>>Cases_on`w a`>>rw[]>>
  intLib.ARITH_TAC
QED

Theorem pbc_ge_thm:
  satisfies w (set (pbc_ge c)) ⇔
  satisfies_pbc w c
Proof
  PairCases_on`c`>>
  rename1`(pbop,xs,n)`>>
  Cases_on`pbop`>>
  simp[pbc_ge_def,satisfies_def]
  >- ( (* Equal *)
    fs[satisfies_pbc_def,eq_disj,eval_lin_term_flip_coeffs]>>
    intLib.ARITH_TAC)
  >- ( (* Greater *)
    simp[satisfies_pbc_def]>>
    intLib.ARITH_TAC)
  >- ( (* LessEqual *)
    simp[satisfies_pbc_def,eval_lin_term_flip_coeffs]>>
    intLib.ARITH_TAC)
  >- ( (* Less*)
    simp[satisfies_pbc_def,eval_lin_term_flip_coeffs]>>
    intLib.ARITH_TAC)
QED

Definition term_lt_def[simp]:
  term_lt (_,l1) (_,l2) = (lit_var l1 < lit_var l2)
End

Definition term_le_def[simp]:
  term_le (_,l1) (_,l2) = (lit_var l1 <= lit_var l2)
End

(* Ensure compact LHS in preconstraint form:
  sort by variables *)
Definition compact_lhs_def:
  (compact_lhs ((c1:int,l1)::(c2,l2)::cs) n =
    if lit_var l1 = lit_var l2 then
      if l1 = l2 then
        compact_lhs ((c1+c2,l1)::cs) n
      else
        compact_lhs ((c1-c2,l1)::cs) (n+c2)
    else
      let (cs',n') = compact_lhs ((c2,l2)::cs) n in
      ((c1,l1)::cs',n')) ∧
  (compact_lhs c n = (c,n))
End

Theorem compact_lhs_MEM:
  ∀xs n xs' n' y l.
  compact_lhs xs n = (xs',n') ∧
  MEM (y,l) xs' ⇒
  ∃y'. MEM (y',l) xs
Proof
  ho_match_mp_tac (theorem "compact_lhs_ind")>>
  rw[compact_lhs_def]>> fs[]
  >- metis_tac[]
  >- metis_tac[]
  >- (
    pairarg_tac>>gs[]>>rw[]>>fs[]>>rw[]>>
    metis_tac[])
  >> metis_tac[]
QED

Theorem transitive_term_le:
  transitive term_le
Proof
  simp[transitive_def]>>
  rpt Cases >>
  simp[term_le_def]
QED

Theorem transitive_term_lt:
  transitive term_lt
Proof
  simp[transitive_def]>>
  rpt Cases >>
  simp[term_lt_def]
QED

Theorem lit_var_eq_term_le:
  lit_var l1 = lit_var l2 ⇒
  (term_le (a,l1) x ⇔ term_le (b,l2) x)
Proof
  Cases_on`x`>>rw[term_le_def]
QED

Theorem compact_lhs_no_dup:
  ∀xs n xs' n'.
  SORTED term_le xs ∧
  compact_lhs xs n = (xs',n') ⇒
  SORTED term_lt xs'
Proof
  ho_match_mp_tac (theorem "compact_lhs_ind")>>
  rw[compact_lhs_def]>> fs[]
  >- (
    first_x_assum match_mp_tac>>
    qpat_x_assum `SORTED _ _` mp_tac>>
    DEP_REWRITE_TAC[SORTED_EQ]>>simp[transitive_term_le]>>
    metis_tac[lit_var_eq_term_le])
  >- (
    first_x_assum match_mp_tac>>
    qpat_x_assum `SORTED _ _` mp_tac>>
    DEP_REWRITE_TAC[SORTED_EQ]>>simp[transitive_term_le]>>
    metis_tac[lit_var_eq_term_le])>>
  pairarg_tac>>fs[]>>rw[]>>
  qpat_x_assum `SORTED _ _` mp_tac>>
  DEP_REWRITE_TAC[SORTED_EQ]>>
  simp[transitive_term_le,transitive_term_lt]>>
  simp[FORALL_PROD]>>rw[]>>
  drule compact_lhs_MEM>>
  disch_then drule>>strip_tac>>
  fs[]>>first_x_assum drule>>
  fs[]
QED

Theorem iSUM_PERM:
  ∀l1 l2. PERM l1 l2 ⇒
  iSUM l1 = iSUM l2
Proof
  ho_match_mp_tac PERM_IND>>rw[iSUM_def]>>
  intLib.ARITH_TAC
QED

Theorem iSUM_QSORT_term_le[simp]:
  iSUM (MAP (eval_term w) (QSORT $≤ l)) =
  iSUM (MAP (eval_term w) l)
Proof
  match_mp_tac iSUM_PERM>>
  match_mp_tac PERM_MAP>>
  metis_tac[QSORT_PERM,PERM_SYM]
QED

Theorem eval_lit_eq_flip:
  q * eval_lit w r = q + (-q * eval_lit w (negate r))
Proof
  Cases_on`r` \\ EVAL_TAC
  \\ Cases_on`w a` \\ EVAL_TAC
  \\ fs[]
QED

Theorem compact_lhs_sound:
  ∀xs n xs' n'.
  compact_lhs xs n = (xs',n') ⇒
  iSUM (MAP (pbc$eval_term w) xs) + n = iSUM (MAP (pbc$eval_term w) xs') + n'
Proof
  ho_match_mp_tac (theorem "compact_lhs_ind")>>
  rw[compact_lhs_def]>> fs[]
  >- (
    (* l1 = l2 *)
    fs[iSUM_def]>>
    intLib.ARITH_TAC)
  >- (
    (* l1 = negate l2 *)
    fs[iSUM_def]>>
    qmatch_goalsub_abbrev_tac` A + _ + _`>>
    REWRITE_TAC[Once eval_lit_eq_flip]>>
    `negate l2 = l1` by
      (Cases_on`l1`>>Cases_on`l2`>>fs[])>>
    fs[Abbr`A`]>>
    qpat_x_assum`_ = _ + _` sym_sub_tac>>
    simp[integerTheory.INT_SUB_RDISTRIB]>>
    qmatch_goalsub_abbrev_tac`_ * wl2 + _ +_ = _ - _ + is + _`>>
    rpt (pop_assum kall_tac)>>
    intLib.ARITH_TAC)>>
  pairarg_tac>>fs[]>>
  rw[]>>
  fs[iSUM_def]>>
  intLib.ARITH_TAC
QED

Definition mk_coeff_def:
  (mk_coeff c (Pos v) = c) ∧
  (mk_coeff c (Neg v) = -c:int)
End

Definition normalise_lhs_def:
  (normalise_lhs [] acc n = (REVERSE acc,n)) ∧
  (normalise_lhs ((x,l)::xs) acc n =
    let v = lit_var l in
    if x < 0 then
      normalise_lhs xs ((mk_coeff x l,v)::acc) (n+x)
    else if x > 0 then normalise_lhs xs ((mk_coeff x l,v)::acc) n
    else normalise_lhs xs acc n)
End

Theorem normalise_lhs_normalises:
  ∀ls acc n ls' n'.
  normalise_lhs ls acc n = (ls',n') ⇒
  iSUM (MAP (pbc$eval_term w) ls) + &(SUM (MAP (eval_term w) acc)) + n =
  &(SUM (MAP (eval_term w) ls')) + n'
Proof
  Induct>>rw[normalise_lhs_def,iSUM_def]
  >-
    metis_tac[SUM_REVERSE,MAP_REVERSE] >>
  Cases_on`h`>>fs[normalise_lhs_def]>>
  every_case_tac>>fs[]
  >- intLib.ARITH_TAC>>
  first_x_assum drule>>
  simp[GSYM integerTheory.INT_ADD]
  >- (
    qmatch_goalsub_abbrev_tac`&SUM _ + qq`>>
    `qq = q * eval_lit w r` by
      (fs[Abbr`qq`]>>Cases_on`r`>>simp[mk_coeff_def]>>
      rename1`b2n( w a)`>>
      Cases_on`w a`>>simp[]>>
      intLib.COOPER_TAC)>>
    rw[] >>
    rename1`A +B + C + D = E`>>
    pop_assum mp_tac>> rpt (pop_assum kall_tac)>> intLib.ARITH_TAC)
  >- (
    qmatch_goalsub_abbrev_tac`&SUM _ + qq`>>
    `qq + q  = q * eval_lit w r` by (
      fs[Abbr`qq`]>>Cases_on`r`>>simp[mk_coeff_def]>>
      rename1`b2n (w a)`>>
      Cases_on`w a`>>simp[]>>
      intLib.ARITH_TAC)>>
    rw[] >>
    rename1`A +B + C + D = E`>>
    ntac 2 (pop_assum mp_tac)>> rpt (pop_assum kall_tac)>> intLib.ARITH_TAC)
  >- (
    `q=0` by intLib.ARITH_TAC>>
    simp[])
QED

Definition pbc_to_npbc_def:
  (pbc_to_npbc (GreaterEqual,lhs,n) =
    let (lhs',m') = compact_lhs (QSORT term_le lhs) 0 in
    let (lhs'',m'') = normalise_lhs lhs' [] 0 in
    let rhs = if n-(m'+m'') ≥ 0 then Num(n-(m'+m'')) else 0 in
    (lhs'',rhs):npbc) ∧
  (pbc_to_npbc _ = ([],0))
End

Definition normalise_def:
  normalise pbf =
  let pbf' = FLAT (MAP pbc_ge pbf) in
  MAP pbc_to_npbc pbf'
End

Theorem pbc_to_npbc_thm:
  FST pbc = GreaterEqual ⇒
  (satisfies_pbc w pbc ⇔ satisfies_npbc w (pbc_to_npbc pbc))
Proof
  PairCases_on`pbc`>>fs[]>>
  rw[satisfies_pbc_def,satisfies_npbc_def,pbc_to_npbc_def]>>
  pairarg_tac>>fs[]>>
  pairarg_tac>>fs[]>>
  drule compact_lhs_sound>>
  disch_then(qspec_then`w` assume_tac)>>fs[eval_lin_term_def]>>
  drule normalise_lhs_normalises>>
  disch_then(qspec_then`w` assume_tac)>>fs[]>>
  simp[satisfies_npbc_def]>>
  intLib.ARITH_TAC
QED

Theorem normalise_thm:
  satisfies w (set (normalise pbf)) ⇔
  satisfies w (set pbf)
Proof
  simp[normalise_def]>>
  qmatch_goalsub_abbrev_tac`MAP _ pbf'`>>
  `satisfies w (set pbf) ⇔ satisfies w (set pbf')` by
    (simp[Abbr`pbf'`]>>
    Induct_on`pbf`>>
    simp[]>>
    metis_tac[pbc_ge_thm])>>
  simp[]>>
  `!x. MEM x pbf' ⇒ FST x = GreaterEqual` by
    (simp[Abbr`pbf'`,MEM_FLAT,MEM_MAP,PULL_EXISTS]>>
    rw[]>>
    PairCases_on`y`>>Cases_on`y0`>>fs[pbc_ge_def])>>
  pop_assum mp_tac>>
  rpt(pop_assum kall_tac)>>
  Induct_on`pbf'`>>simp[]>>
  rw[]>>
  metis_tac[pbc_to_npbc_thm]
QED

Definition full_normalise_def:
  full_normalise pbf = normalise (convert_pbf pbf)
End

Theorem full_normalise_satisfiable:
  pbf_vars (set pbf) ⊆ goodString ⇒
  (satisfiable (set (full_normalise pbf)) ⇔
    satisfiable (set pbf))
Proof
  rw[pbcTheory.satisfiable_def,npbcTheory.satisfiable_def,full_normalise_def,normalise_thm]>>
  metis_tac[convert_pbf_thm,pbcTheory.satisfiable_def,npbcTheory.satisfiable_def]
QED

Theorem lit_var_negate[simp]:
  lit_var (negate r) = lit_var r
Proof
  Cases_on`r`>>simp[]
QED

Theorem normalise_lhs_compact1:
  ∀lhs acc n lhs' n'.
  normalise_lhs lhs acc n = (lhs',n') ∧
  SORTED $< (MAP SND (REVERSE acc) ++ MAP (lit_var o SND) lhs) ⇒
  SORTED $< (MAP SND lhs')
Proof
  Induct>>simp[normalise_lhs_def]>>
  Cases>>simp[normalise_lhs_def]>>rw[]>>
  first_x_assum match_mp_tac>>
  asm_exists_tac>>fs[]
  >- (
    PURE_REWRITE_TAC[Once (GSYM APPEND_ASSOC),APPEND]>>
    fs[])
  >- (
    PURE_REWRITE_TAC[Once (GSYM APPEND_ASSOC),APPEND]>>
    fs[]) >>
  pop_assum mp_tac>>
  DEP_REWRITE_TAC[SORTED_APPEND,SORTED_EQ]  >>
  CONJ_TAC>- simp[transitive_def]>>
  simp[]
QED

Theorem normalise_lhs_compact2:
  ∀lhs acc n lhs' n'.
  normalise_lhs lhs acc n = (lhs',n') ∧
  EVERY (λc. c ≠ 0) (MAP FST acc) ⇒
  EVERY (λc. c ≠ 0) (MAP FST lhs')
Proof
  Induct>>simp[normalise_lhs_def]
  >-
    simp[EVERY_MAP]>>
  Cases>>fs[normalise_lhs_def]>>rw[]>>
  first_x_assum match_mp_tac>>
  asm_exists_tac>>fs[]>>
  Cases_on`r`>>fs[mk_coeff_def]>>
  intLib.ARITH_TAC
QED

Theorem compact_pbc_to_npbc:
  compact (pbc_to_npbc c)
Proof
  PairCases_on`c`>>Cases_on`c0`>>
  rw[pbc_to_npbc_def]>>
  pairarg_tac>>fs[]>>
  pairarg_tac>>fs[]>>
  imp_res_tac compact_lhs_no_dup>>
  pop_assum mp_tac>>
  impl_tac>- (
    match_mp_tac QSORT_SORTED>>
    fs[transitive_term_le]>>
    simp[total_def]>>
    Cases>>Cases>>simp[])>>
  strip_tac>>
  CONJ_TAC>- (
    match_mp_tac normalise_lhs_compact1>>
    asm_exists_tac>>
    simp[sorted_map]>>
    qmatch_goalsub_abbrev_tac`_ tlt _`>>
    `tlt = term_lt` by
      simp[Abbr`tlt`,FUN_EQ_THM,FORALL_PROD]>>
    fs[])>>
  match_mp_tac normalise_lhs_compact2>>
  asm_exists_tac>>
  simp[]
QED

Theorem normalise_compact:
  EVERY compact (normalise pbf)
Proof
  simp[normalise_def,EVERY_MAP,compact_pbc_to_npbc]
QED

val _ = export_theory();
