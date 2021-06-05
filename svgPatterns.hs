-- mss middle-square sequence

import Text.Printf

type Point     = (Float,Float)
type Rect      = (Point,Float,Float)
type Circle    = (Point,Float)


-- Utilidades (talvez esteja reinventando a roda)

skip 0 xs     = xs
skip n (_:xs) = skip (n - 1) xs


-- Sequências de números

-- https://en.wikipedia.org/wiki/Middle-square_method
mss :: (Integral a) => a -> a -> [a]
mss n_digits number
  | not (even n_digits) = error "n_digits must be even"
  | otherwise           = (middleNumber : mss n_digits middleNumber)
  where middleNumber = leftNumber - ((leftNumber `div` (10 ^ n_digits)) * (10 ^ n_digits))
        leftNumber   = (number ^ 2) `div` (10 ^ (n_digits `div` 2))

distribute n cap = map (\x -> cap `div` n * x) [1..n]

-- Paletas

greenPalette :: Int -> [(Int,Int,Int)]
greenPalette n = [(0, tom, 0) | tom <- distribute n 255]

-- Paleta com n valores retirados de uma lista com sequências de R, G e B 
-- O '$' é uma facilidade sintática que substitui parênteses
-- O cycle é uma função bacana -- procure saber mais sobre ela :-)
rgbPalette :: Int -> [(Int,Int,Int)]
rgbPalette n = take n $ cycle [(255,0,0),(0,255,0),(0,0,255)]

genPalette :: (Integral a) => [a] -> [a] -> [a] -> [(a, a, a)]
genPalette reds greens blues = zip3 (cap reds) (cap greens) (cap blues)
  where cap = map (\color -> color `mod` 256)


-- Geração de retângulos em suas posições

genRectsInLine :: Int -> [Rect]
genRectsInLine n  = [((m*(w+gap), 0.0), w, h) | m <- [0..fromIntegral (n-1)]]
  where (w,h) = (50,80)
        gap = 10

--genRectsGrid :: (Integral a) => a -> a -> a -> a -> a -> [Rect]
genRectsGrid x y w h gap = map (\((x, y), w, h) -> ((deviate x, deviate y), deviate w, deviate h)) base
  where base      = [((x' * (w + gap), y' * (h + gap)), w, h) | x' <- [0..x], y' <- [0..y]]
        deviate x = (x * ((fromIntegral (head (skip x (mss 10 8461095437)))) / (fromIntegral 10000000000)))


-- Strings SVG

-- Gera string representando retângulo SVG 
-- dadas coordenadas e dimensões do retângulo e uma string com atributos de estilo
svgRect :: Rect -> String -> String 
svgRect ((x,y),w,h) style = 
  printf "<rect x='%.3f' y='%.3f' width='%.2f' height='%.2f' style='%s' />\n" x y w h style

-- Wrapper do conteúdo em tags <svg></svg>
svgBody :: Float -> Float -> String -> String
svgBody = printf "<svg width='%.2f' height='%.2f' xmlns='http://www.w3.org/2000/svg'>\n%s</svg>\n"

-- Gera string com atributos de estilo para uma dada cor
-- Atributo mix-blend-mode permite misturar cores
svgStyle :: (Int,Int,Int) -> String
svgStyle (r,g,b) = printf "fill:rgb(%d,%d,%d); mix-blend-mode: screen;" r g b

-- Gera strings SVG para uma dada lista de figuras e seus atributos de estilo
-- Recebe uma função geradora de strings SVG, uma lista de círculos/retângulos e strings de estilo
svgElements :: (a -> String -> String) -> [a] -> [String] -> String
svgElements func elements styles = concat $ zipWith func elements styles


-- Função principal que gera arquivo com imagem SVG

main :: IO ()
main = do
  writeFile "figs.svg" $ svgstrs
  where svgstrs = svgBody w h svgfigs
        svgfigs = svgElements svgRect rects (map svgStyle palette)
        --rects   = genRectsInLine nrects
        rects   = genRectsGrid 11 5 230 300 (-5)
        palette = genPalette (mss 6 123456) (mss 6 654321) (mss 6 987654)
        --palette = greenPalette nrects
        nrects  = 10
        (w,h)   = (1500,500) -- width,height da imagem SVG
