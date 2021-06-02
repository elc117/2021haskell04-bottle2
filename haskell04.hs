import Text.Printf

-- Prática 04 de Haskell
-- Nome: Bento Borges Schirmer

faixaIdoso :: Int -> String
faixaIdoso idade
  | idade `elem` [60..64] = "IDO64"
  | idade `elem` [65..69] = "IDO69"
  | idade `elem` [70..74] = "IDO74"
  | idade `elem` [75..79] = "IDO79"
  | idade >= 80           = "IDO80"
  | otherwise             = "ND"

classifIdosos :: [(String,Int)] -> [(String,Int,String)]
classifIdosos pessoas = [(nome, idade, faixaIdoso idade) | (nome, idade) <- pessoas]

classifIdosos' :: [(String,Int)] -> [(String,Int,String)]
classifIdosos' = map (\(nome, idade) -> (nome, idade, faixaIdoso idade))

strColor :: (Int,Int,Int) -> String
strColor (red, green, blue) = concat ["rgb(",show red,",",show green,",",show blue,")"]

genCircs :: Int -> (Int,Int) -> Int -> [(Int,Int,Int)] -- Assinatura alterada
genCircs n (x, y) raio = map (\i -> (x + i * 4, y, raio)) [0..n - 1]
  where fator = 4

genReds :: Int -> [(Int,Int,Int)] -- Assinatura alterada
genReds n = map (\tom -> ((255 `div` n * tom), 0, 0)) [1..n]
