# Lista Lambda

## Otávio Almeida

### 1) Reescreva os seguintes termos utilizando parênteses explicitamente em volta de cada subtermo.
    a) a b c d
       (((a b) c) d)

    b) λq.λi.q
       (λq.(λi.q))

    c) λx.λy.λz.x z (y z)
       (λx.(λy.(λz.(x (z (y z))))))

### 2) Para os seguintes termos, digam que variáveis estão livres, e que variáveis estão ligadas (e a que λ!).
    a) λs.s z λq.s q
       λs.(s z λq.(s q))
       FV = {z}
       Ligadas = {s -> λs, q -> λq}

    b) (λs.s z) λq.w λw.w q z s
       (λs.s z) λq.(w λw.(w q z s))
       (λs.s z) λq.(**w** (Esse w é livre) λw.(**w** (Esse w é ligado à λw) q z s))
       FV = {z, s, w}
       Ligadas = {s -> λs, w -> λw}

    c) (λs.s) (λq.q s)
       (λs.**s** (Esse s é ligado à λs)) (λq.q **s** (Esse s é livre))
       FV = {s}
       Ligadas = {s -> λs, q -> λq}

    d) λz.((λs.s q) (λq.q z)) λz.z z
       λz'.((λs.s **q** (Esse q é livre)) (λq.**q** (Esse q esta ligado a λq) **z** (Esse z é ligado a λz'))) λz''.**z z** (Esses z são ligados a λz'')
       FV = {q}
       Ligadas = {z -> λz' ou λz'', s -> λs, q -> λq}

### 3) Aplique reduções β às expressões abaixo, os reduzindo à forma normal (isto é, a um formato onde não é mais possível aplicar reduções).
    a)  (λz.z) (λq.q q) (λs.s a)
        (λq.q q) (λs.s a)
        (λs.s a) (λs.s a)
        (λs.s a) a
        a a
        
    b)  (λs.λq.s q q) (λa.a) b
        (λq.(λa.a) q q) b
        (λa.a) b b
        b b
    
    c)  (λs.λq.s q q) (λx.x) c
        (λq.(λx.x) q q) c
        (λx.x) c c
        c c

    d)  ((λs.s s) (λq.q)) (λr.r)
        (λq.q) (λq.q) (λr.r)
        (λq.q) (λr.r) 
        λr.r

### 4) Considere uma versão do cálculo lambda estendido com numerais e operações aritméticas; sendo assim, além da redução β, podemos realizar reduções aritméticas quando ambos os lados de um operador forem numerais (e.g., 1 + 1 → 2). Reduza as expressões abaixo às suas formas normais.
    a) (λx.x) 5
       5

    b) (λx.x + 10) 42
       42 + 10
       52

    c) (λf.f (f 10)) (λx.x + 2)
       (λx.x + 2) ((λx.x + 2) 10)
       (λx.x + 2) (10 + 2)
       (λx.x + 2) 12
       (12 + 2)
       14

    d) (λf.f) (λx.x) 51
       (λx.x) 51
       51

### 5) Considerando que:
    ONE = λa.λb.a b
    TWO = λc.λd.c (c d)
    PLUS = λm.λn.λf.λx.m f (n f x)
...aplique as reduções β à expressão PLUS TWO ONE à sua forma normal (dica: são 
necessárias 6 reduções). Cuidado com os parênteses!

    PLUS TWO ONE
    (λm.λn.λf.λx.m f (n f x)) (λc.λd.c (c d)) (λa.λb.a b)
    (λm.λn.λf.λx.m f (n f x)) (λc.λd.c (c d)) (λa.λb.a b)
    (λn.λf.λx.(λc.λd.c (c d)) f (n f x)) (λa.λb.ab)
    λf.λx.(λc.λd.c (c d)) f ((λa.λb.a b) f x)
    λf.λx.(λd.f (f d))((λa.λb.a b) f x)
    λf.λx.f(f ((λa.λb.a b) f x))
    λf.λx.f(f ((λb.f b) x))
    λf.λx.f(f(f x))
