type letter = A | B | C | D [@@deriving every]

let char_of_letter = function A -> 'A' | B -> 'B' | C -> 'C' | D -> 'D'
let pp_letter ppf letter = Format.fprintf ppf "%c" (char_of_letter letter)
let pp_comma ppf () = Format.fprintf ppf ", "

let () =
  Format.printf "%a\n"
    (Format.pp_print_list ~pp_sep:pp_comma pp_letter)
    every_letter
