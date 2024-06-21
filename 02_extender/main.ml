let expected_password = [%env "PASSWORD"]

let () =
  match Sys.argv |> Array.to_list with
  | [ _; password ] ->
      if String.equal password expected_password then
        Printf.printf "Password matches\n"
      else Printf.printf "Invalid password\n"
  | prog :: _ -> Printf.printf "Usage: %s password\n" prog
  | [] -> assert false
