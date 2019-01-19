/*
** preamble/operators.sail: operator overloads
** See Copyright Notice in LICENSE.txt
*/

/* -------------------------------------------------------------------------- */

infix 6 >>
infix 6 <<

overload operator == = { eq_string, eq_real, eq_unit, eq_bit, eq_bits, eq_int, eq_bool }
overload operator != = { neq_int, neq_bool }
overload operator &  = { and_vec }
overload operator |  = { or_vec }
overload operator ^  = { xor_vec, int_power, real_power, string_append }
overload operator +  = { add_int, add_vec, add_vec_int, add_real }
overload operator -  = { sub_int, sub_vec, sub_vec_int, sub_real }
overload operator *  = { mult_atom, mult_int, mult_real }
overload operator >= = { gteq_atom, gteq_int, gteq_real }
overload operator <= = { lteq_atom, lteq_int, lteq_real }
overload operator >  = { gt_atom, gt_int, gt_real }
overload operator <  = { lt_atom, lt_int, lt_real }
overload operator /  = { div_int, div, quotient_nat, quotient, quotient_real }
overload operator %  = { mod_int, mod, modulus }

overload operator << = { shiftleft }
overload operator >> = { shiftright }

/* -------------------------------------------------------------------------- */
/* -- El Fin (preamble/operators.sail) -------------------------------------- */
