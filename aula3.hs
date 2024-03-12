fun_intervalo x = if x < 5 && x > 1 then "X está dentro do intervalo" else "X não está dentro do intervalo"

_fun_intervalo x
    | x < 5 && x > 1 = "X está dentro do intervalo"
    | otherwise = "X não está dentro do intervalo"
