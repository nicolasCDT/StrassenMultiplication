(** Definition of the 'matrix' type as an array of arrays of floats *)
type matrix = float array array;;

(** Returns if v is an integer *)
let is_int v =
  v = (snd (modf v)) ;;

(** Returns the base 2 logarithm of a number *)
let rec log2 n p =
  if n <= 1 then p
  else log2 (n / 2) (p + 1);;

(** Renvoie la puissance de 2 suivante *)
let next_power_of_2 n =
  1 lsl (log2 n 0 + 1);;

(** Function for generating an m*n matrix using a generation function 'generator *)
let init_matrix generator m n = Array.init m (fun i -> Array.init n (generator i));;

(** Create an empty matrix of size m*n *)
let empty_matrix m n = (init_matrix (fun i j -> 0.) m n);;

(** Read a matrix from a file. *)
let read_matrix_from_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true do
      let line = input_line chan in
      let row = List.map float_of_string (String.split_on_char ' ' line) in
      lines := !lines@[row]
    done;
    assert false
  with End_of_file ->
    close_in chan;
    let nrows = List.length !lines in
    let ncols = List.length (List.hd !lines) in
    let mat = empty_matrix nrows ncols in
    List.iteri (fun i row -> List.iteri (fun j x -> mat.(i).(j) <- x) row) !lines;
    mat

(** Create the identity matrix of size n*n *)
let matrice_identite n = 
  (init_matrix (fun i j -> if ( i = j ) then 1. else 0.) n n);;

(** Add two matrices of the same size *)
let sum_matrix m n =
  (init_matrix (fun i j -> m.(i).(j) +. n.(i).(j)) (Array.length m) (Array.length m.(0)));;

(** Sub two matrices of the same size *)
let sub_matrix m n =
  (init_matrix (fun i j -> m.(i).(j) -. n.(i).(j)) (Array.length m) (Array.length m.(0)));;

(** Combine 4 matrices into one *)
let regroup_matrix a b c d =
  let n = Array.length a in
  let m = empty_matrix (2*n) (2*n) in
  let rec copy i j =
    if i = n then ()
    else if j = n then copy (i+1) 0
    else (m.(i).(j) <- a.(i).(j);
          m.(i).(j+n) <- b.(i).(j);
          m.(i+n).(j) <- c.(i).(j);
          m.(i+n).(j+n) <- d.(i).(j);
          copy i (j+1))
  in
  copy 0 0;
  m;;

(** Complete a matrix with zeros to make it of size 2^n *)
let complete_matrix m =
  let rows = Array.length m in
  let cols = Array.length m.(0) in
  let n = next_power_of_2 cols in
  let completed = empty_matrix n n in
  
  let copy_row i row =
    Array.blit row 0 completed.(i) 0 cols
  in
  Array.iteri copy_row m;

  (completed, n - rows, n - cols)

(** Auxiliary function to split a matrix into four submatrices *)
let split_matrix m =
  let rows = Array.length m in
  let cols = Array.length m.(0) in
  let half_rows = rows / 2 in
  let half_cols = cols / 2 in
  let a11 = Array.init half_rows (fun i -> Array.sub m.(i) 0 half_cols)
  and a12 = Array.init half_rows (fun i -> Array.sub m.(i) half_cols half_cols)
  and a21 = Array.init half_rows (fun i -> Array.sub m.(i + half_rows) 0 half_cols)
  and a22 = Array.init half_rows (fun i -> Array.sub m.(i + half_rows) half_cols half_cols) in
  (a11, a12, a21, a22)

(** Multiply matrices m and n *)
let mult_matrix m n =
  let m_rows = Array.length m in
  let m_cols = Array.length m.(0) in
  let n_cols = Array.length n.(0) in

  let result_matrix = Array.init m_rows (fun i ->
    Array.init n_cols (fun j ->
      let dot_product = Array.map (fun k -> m.(i).(k) *. n.(k).(j)) (Array.init m_cols (fun k -> k)) in
      Array.fold_left (+.) 0. dot_product
    )
  ) in
  result_matrix;;

(* Remove the added zeros to simplify the calculation *)
let trim_matrix m lines colum =
  init_matrix (fun i j -> m.(i).(j)) (Array.length m - lines) (Array.length m.(0) - colum);;


(** Strassen's multiplication for a function of size 2^n *)
let rec strassen a b =
  if Array.length a = 1 then
    [|[|a.(0).(0) *. b.(0).(0) |]|]
  else
    let a1, a2, a3, a4 = split_matrix a in
    let b1, b2, b3, b4 = split_matrix b in
    let p1 = strassen (sum_matrix a1 a4) (sum_matrix b1 b4) in
    let p2 = strassen (sub_matrix a1 a3) (sum_matrix b1 b2) in
    let p3 = strassen (sub_matrix a2 a4) (sum_matrix b3 b4) in
    let p4 = strassen a1 (sub_matrix b2 b4) in
    let p5 = strassen (sum_matrix a1 a2) b4 in
    let p6 = strassen (sum_matrix a3 a4) b1 in
    let p7 = strassen a4 (sub_matrix b1 b3) in
    let c1 = sub_matrix (sum_matrix p1 p3) (sum_matrix p5 p7) in
    let c2 = sum_matrix p4 p5 in
    let c3 = sub_matrix p6 p7 in
    let c4 = sum_matrix (sub_matrix p1 p2) (sub_matrix p4 p6) in
    regroup_matrix c1 c2 c3 c4;;

(** Display matrix *)
let print_matrix mat =
  Array.iter (fun row ->
    Array.iter (fun elem -> print_float elem; print_string " ") row;
    print_newline ()
  ) mat;;

(** Calculate and display the execution time of a function with two parameters *)
let time f x y =
    let t = Sys.time() in
    let fx = f x y in
    Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
    fx

let opp m = 
    (init_matrix (fun i j -> -1. *. m.(i).(j)) (Array.length m) (Array.length m.(0)));;

(** Function to invert a matrix using the Strassen method *)
let rec inverse mat =
  if Array.length mat = 1 then
    if mat.(0).(0) = 0. then
      [|[|0.|]|]
    else
      [|[|1. /. mat.(0).(0)|]|]
  else
    let a11, a12, a21, a22 = split_matrix mat in
    let p = inverse a11 in
    let q = strassen a21 p in
    let r = strassen p a12 in
    let s = strassen a21 r in 
    let t = sub_matrix s a22 in
    let u = inverse t in
    let b12 = strassen r u in
    let b21 = strassen u q in
    let b22 = opp u in
    let b11 = sub_matrix p (strassen r b21) in
    regroup_matrix b11 b12 b21 b22;;

(* Calculate the sum of the components of a matrix *)
let sum_matrix_abs_components matrix =
  let sum = ref 0. in
  Array.iter (fun row -> Array.iter (fun x -> sum := !sum +. abs_float x) row) matrix;
  !sum;;

let m1 = matrice_identite 11;;
let m2 = init_matrix (fun i j -> (float_of_int (i+j))) 11 11;;
let m2_c, diff_rows, diff_colomns = complete_matrix m2;;
let m1 = read_matrix_from_file "matrices/matrix1.txt";;
let m1_c, rows, colomns = complete_matrix m1;;
let m2 = read_matrix_from_file "matrices/matrix1.txt";;
let m2_c, rows2, colomns2 = complete_matrix m2;;

let a, b, c, d = split_matrix m1_c;;

print_string "Multiplication de la matrice 1 :\n";;
print_matrix m1;;
print_string "\nAvec la matrice 2 : \n";;
print_matrix m2;;
print_string "\n\nRésultat : \n";;
print_matrix (trim_matrix (strassen m1_c m2_c) rows colomns);;
print_string "\n\nInverse de la matrice 1 :\n";;
print_matrix (trim_matrix (inverse m1_c) rows colomns);;