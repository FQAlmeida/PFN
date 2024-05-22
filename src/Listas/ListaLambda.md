### 1) Reescreva os seguintes termos utilizando parênteses explicitamente em volta de cada subtermo.
    a)  a b c d
        (a(b(c(d))))
    b)  λq.λi.q
        (λq.λi.q)
    c)  λx.λy.λz.x z (y z)
        (λx.λy.λz.x) (z) (y (z))

### 2) Para os seguintes termos, digam que variáveis estão livres, e que variáveis estão ligadas (e a que λ!).
    a) λs.s z λq.s q
    b) (λs.s z) λq.w λw.w q z s
    c) (λs.s) (λq.q s)
    d) λz.((λs.s q) (λq.q z)) λz.z z

### 3) Aplique reduções β às expressões abaixo, os reduzindo à forma normal (isto é, a um formato onde não é mais possível aplicar reduções).
    a)  (λz.z) (λq.q q) (λs.s a)
        (λz.z) (λq.q q) (a)
        (λz.z) (q) (a)
        q a
    b)  (λs.λq.s q q) (λa.a) b
        (λq.q q) (λa.a) b
        q (λa.a) b
        q b
    c)  (λs.λq.s q q) (λx.x) c
        (λq.q q ) (λx.x) c
        (q) (λx.x) c
        q c
    d)  ((λs.s s) (λq.q)) (λr.r)
        (s (λq.q)) (λr.r)

### 4) Considere uma versão do cálculo lambda estendido com numerais e operações aritméticas; sendo assim, além da redução β, podemos realizar reduções aritméticas quando ambos os lados de um operador forem numerais (e.g., 1 + 1 → 2). Reduza as expressões abaixo às suas formas normais.
    a)  (λx.x) 5
        5
    b)  (λx.x + 10) 42
        42 + 10
        52
    c)  (λf.f (f 10)) (λx.x + 2)
        (f 10) (λx.x + 2)
    d)  (λf.f) (λx.x) 51
        (λx.x) 51
        51

### 5) Considerando que:
    ONE = λa.λb.a b
    TWO = λc.λd.c (c d)
    PLUS = λm.λn.λf.λx.m f (n f x)
    ...aplique as reduções β à expressão PLUS TWO ONE à sua forma normal (dica: são 
    necessárias 6 reduções). Cuidado com os parênteses!

    (λm.λn.λf.λx.m f (n f x)) (λc.λd.c (c d)) (λa.λb.a b)
    (λm.λn.λf.λx.m f (n f x)) (λc.λd.c (c d)) (λb.b)
    (λm.λn.λf.λx.m f (n f x)) (λd.(c d)) (λb.b)
    (λn.λf.λx.(n f x) f) (λd.(c d)) (λb.b)
    (λf.λx.(f f x)) (λd.(c d)) (λb.b)
    (λx.((λd.(c d)) (λd.(c d)) x)) (λb.b)
    (λd.(c d)) (λd.(c d)) (λb.b)
    (c (λd.(c d))) (λb.b)

    c (c (λb.b))

