/*
** execute/prologue.sail: RISC-V execution engine setup
** See Copyright Notice in LICENSE.txt
*/

/* -------------------------------------------------------------------------- */

val execute : ast -> unit effect pure
scattered function execute

/* TODO FIXME: no compressed ones yet, so fake one */
function clause execute C_NOP() = ()

/* -------------------------------------------------------------------------- */
/* -- El Fin (execute/prologue.sail) ---------------------------------------- */
