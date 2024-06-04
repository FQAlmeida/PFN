# Revisão Cálculo Lambda

## Beta Redução

     1) (λz.z) (λy.y y) (λx.x a)
        (λy.y y) (λx.x a)
        (λx.x a) (λx.x a)
        (λx.x a) a
        a a

     2) (λz.z) (λz.z z) (λz.z y)
        (λz.z z) (λz.z y)
        (λz.z y) (λz.z y)
        (λz.z y) y
        y y

     3) (λxλy.x y y) (λy.y) y
        -- Alpha reduction: trocar nome de variável para não confundir contexto
        (λxλa.x a a) (λy.y) y 
        (λa.(λy.y) a a) y
        (λy.y) y y
        y y

     4) (λx.x x) (λy.y x) z
        (λy.y x) (λy.y x) z
        (λy.y x) x z
        x x z

     5.1) (λx.(λy.(x y)) y) z
          (λx.(x y)) z
          z y

     5.2) (λx.(λy.(x y)) y) z
          (λy.(z y) y) 
          z y
