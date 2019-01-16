/*
** scattered.sail: end module that closes scattered definitions, etc.
** See Copyright Notice in LICENSE.txt
*/

/* -------------------------------------------------------------------------- */
/* -- Close scattered defs with an exception clause ------------------------- */

union clause ast = RI : unit

/* -- Final clause for 'decode ' which simply does nothing -- */
function clause decode _ = Some(RI())

/* -- c.f. 'execute' -- */
function clause execute (RI()) = {
   throw (EInvalidInstr())
}

/* -- Close everything -- */
end ast
end decode
end execute

/* -------------------------------------------------------------------------- */
/* -- El Fin (setup.sail) --------------------------------------------------- */
