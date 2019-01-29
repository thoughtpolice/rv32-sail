/*
** execute/epilogue.sail: close scattered defs, helpers
** See Copyright Notice in LICENSE.txt
*/

/* -------------------------------------------------------------------------- */
/* -- Close scattered defs -------------------------------------------------- */

function clause execute i = {
  let rep : bits(32) = encode_base(i);
  let msg = "Unimplemented instruction encountered: '" ^ bits_str(rep) ^ "'"
          ^ ", AKA '" ^ print_insn(i) ^ "'";

  throw (ENotImplemented(msg))
}

end execute

/* -------------------------------------------------------------------------- */
/* -- El Fin (execute/epilogue.sail) ---------------------------------------- */
