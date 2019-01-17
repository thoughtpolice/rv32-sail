f = """val hex_bits_{0} : bits({0}) <-> string
val hex_bits_{0}_forwards = "decimal_string_of_bits" : bits({0}) -> string
val hex_bits_{0}_forwards_matches : bits({0}) -> bool
function hex_bits_{0}_forwards_matches bv = true
val "hex_bits_{0}_matches_prefix" : string -> option((bits({0}), nat))
val hex_bits_{0}_backwards_matches : string -> bool
function hex_bits_{0}_backwards_matches s = match s {{
  s if match hex_bits_{0}_matches_prefix(s) {{
    Some (_, n) if n == string_length(s) => true,
    _ => false
  }} => true,
  _ => false
}}
val hex_bits_{0}_backwards : string -> bits({0})
function hex_bits_{0}_backwards s =
  match hex_bits_{0}_matches_prefix(s) {{
      Some (bv, n) if n == string_length(s) => bv
  }}
"""

for i in list(range(1, 34)) + [48, 64]:
  print(f.format(i))
