/*
** ecall.sail: functions for the ECALL "hypervisor" ABI to the host
** See Copyright Notice in LICENSE.txt
*/

/* -------------------------------------------------------------------------- */

val __ecall_putchar : unit -> unit effect { rreg }
function __ecall_putchar () = {
  let c : xlen_t = X(to_bits(5, 11)); // a1
  putchar(unsigned(c));
}

/* -------------------------------------------------------------------------- */
/* -- Top-level dispatch for all ECall routines ----------------------------- */

val ecall_dispatch : unit -> unit effect { escape, rreg }
function ecall_dispatch () = {
  let a0 = X(to_bits(5, 10)); // a0

  match a0 {
    0x00000000 => { __ecall_putchar() },
    _ => { throw(EInvalidECall(a0)) }
  }
}

/* -------------------------------------------------------------------------- */
/* -- El Fin (ecall.sail) --------------------------------------------------- */
