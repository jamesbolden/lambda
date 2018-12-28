let desugar_list xs =
    let rec go acc = function
        | [] -> acc
        | x :: xs -> go (Syntax.Exp.App (Syntax.Exp.App (Syntax.Exp.Var ":", x), acc)) xs
    in
    go (Syntax.Exp.Var "[]") (List.rev xs)

let desugar_block xs =
    let rec go acc = function
        | [] -> acc
        | x :: xs -> go (Syntax.Exp.Seq (x, acc)) xs
    in
    match List.rev xs with
        | [] -> failwith "Impossible error: empty block"
        | x :: xs -> go x xs
