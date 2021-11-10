-- 1

enumFromTo :: Int -> Int -> [Int]
enumFromTo a b 
 | a == b = [a]
 | a > b = []
 | otherwise = [a] ++ enumFromTo (a+1) b

-- 2 

enumFromThenTo :: Int -> Int -> Int -> [Int]
enumFromThenTo x y z
 | x == z = [x] 
 | x < z && x == y = []
 | (x > z && y >= z ) || (x < z && y <= z ) = x:myenumFromThenTo (y) (2*y-x) z 
 | otherwise = []

-- 3 

adicionalista :: [a] -> [a] -> [a]
adicionalista [] l = l
adicionalista (h:t) l = h : adicionalista t l

-- 4

posicao :: [a] -> Int -> a
posicao (a:b) 0 = a 
posicao (a:b) x = posicao b (x-1)

-- 5

reverse :: [a] -> [a]
reverse [] = []
reverse (a:b) = (reverse b) ++ [a]

-- 6

take :: Int -> [a] -> [a]
take _ [] = []
take 0 (h:t) = t
take x (h:t) = h : take x t

-- 7 

drop :: Int -> [a] -> [a]
drop _ [] = []
drop 0 l = l
drop x (h:t) = drop (x-1) t

-- 8

zip :: [a] -> [b] -> [(a,b)]
zip _ [] = []
zip [] _ = []
zip (h1:t1) (h2:t2) = [(h1,h2)] ++ zip t1 t2

-- 9

replicate :: Int -> a -> [a]
replicate 0 l = [] 
replicate n x = x : replicate (n-1) x

-- 10 

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [a] = [a]
intersperse x (h:t) = h:x: intersperse x t

-- 11

group :: Eq a => [a] -> [[a]] 
group [] = []
group [a] = [[a]]  
group (a,b)
 | a == head (c1) = (a:c1) : c2
 | otherwise = [a] : (c1,c2)
	where (c1,c2) = group b

-- 12 

concat :: [[a]] -> [a]
concat [[]] = []
concat (h:t) = h ++ concat t

-- 13

inits :: [a] -> [[a]]
inits [] = []
inits (h:t) = inits (init t) ++ [h]

-- 14

tails :: [a] -> [[a]]
tails [] = []
tails l = l: tails (tail l) 

-- 15

heads :: [[a]] -> [a]
heads [] = []
heads ([]:t) = heads t
heads ((x:y):t) = x : heads t

-- 16

total :: [[a]] -> Int 
total [] = 0
total ([]:t) = total t 
total ((x:y):t) = 1 + total (y:t)

-- 17

fun :: [(a,b,c)] -> [(a,c)] 
fun [] = []
fun ((x,y,z):t) = (x,z) : fun t

-- 18

cola :: [(String,b,c)] -> String
cola [] = []
cola ((x,y,z):t) = x ++ cola t 

-- 19

idade :: Int -> Int -> [(String,Int)] -> [String]
idade 0 _ _ = []
idade _ 0 _ = []
idade _ _ [] = []
idade x y ((a,b):xs) = if (x-b) >= y then a : idade x y xs else idade x y xs

-- 20

powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom n 1 = [1]
powerEnumFrom n m
    | m > 1 = powerEnumFrom n (m - 1) ++ [n^(m-1)]
    | otherwise = []

-- 21

isPrime :: Int -> Bool
isPrime n
    | n >= 2 = primeCheck n 2
    | otherwise = False

primeCheck :: Int -> Int -> Bool
primeCheck n m
    | m * m > n = True 
    | mod n m == 0 = False
    | otherwise = primeCheck n (m + 1)

-- 22

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf (h:t) (h1:t1)
 | h == h1 = isPrefixOf t t1
 | otherwise = False

-- 23

isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf [] _ = True
isSuffixOf l l2
 | length l > length l2 = False
 | last l ==  last l2 = isSuffixOf (init l) (init l2)
 | otherwise = False

-- 24

isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (h:t) (h1:t1)
 | h /= h1 = isSubsequenceOf (h:t) t1 
 | otherwise = isSubsequenceOf t t1

-- 25

elemIndices :: Eq a => a -> [a] -> [Int] 
elemIndices _ [] = True
elemIndices x l
 | last l == x = elemIndices x (init l) ++ [(length l)-1]
 | otherwise = elemIndices x (init l)

-- 26

nub :: Eq a => [a] -> [a]
nub [] = []
nub (h:t)
 | elem h t = nub t
 | otherwise = h : nub t

-- 27

delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete x (h:t) 
 | x == h = t
 | otherwise = h: delete x t

-- 28

remove :: Eq a => [a] -> [a] -> [a]
remove [] _ = []
remove x [] = x
remove (h:t) (h1:t1) = remove (delete h1 (h:t)) t1 

-- 29

intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect _ [] = []
intersect (h:t) l
 | elem h l = intersect t l
 | otherwise = intersect t l

-- 30 

insert :: Ord a => a -> [a] -> [a] 
insert x [] = [x]
insert x (h:t)
 | x <= h = x:h:t
 | otherwise = h : insert x t 


-- 35  

lookup :: Eq a => a -> [(a,b)] -> Maybe b
lookup [] = Nothing
lookup n (Nothing:t) = lookup n t
lookup n ((x,y):t)
 | n == x = Just y
 | otherwise = lookup n t

-- 36

preCrescente :: Ord a => [a] -> [a] 
preCrescente [] = []
preCrescente (h:h2:t) 
 | h >= h2 = h
 | otherwise = h: preCrescente (h2:t)

-- 37

iSort :: Ord a => [a] -> [a]
iSort [] = [] 
iSort (h:t) = insere h (iSort t)

insere :: Ord a => a -> [a] -> [a]
insere x [] = [x]
insere x (h:t) = if x <= h then (x:h:t) 
                 else h:insere x t 

-- 38

menor :: String -> String -> Bool
menor (x:xs) "" = False
menor "" (x:xs) = True
menor (x:xs) (a:as)
    | x < a = True
    | x > a = False
    | x == a = menor xs as

-- 39

elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet _ [] = False
elemMSet n ((x,y):t) 
 | n /= x = elemMSet n t
 | otherwise = True

-- 40

converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((x,y):t) 
 | y /= 0 = x:converteMSet ((x,y-1):t)
 | otherwise = converteMSet t

-- 41

insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet n [] = [(n,1)]
insereMSet n ((x,y):t)
 | n == x = ((x,y+1):t)
 | otherwise = insereMSet n t

-- 42

removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet n [] = []
removeMSet n ((x,y):t) 
 | n == x = if y == 0 then t else ((x,y-1):t)
 | otherwise = removeMSet n t

-- 43

constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet []    = []
constroiMSet (h:t) = (h,c):constroiMSet (drop c (h:t))
    where c = constroiMSetAux (h:t)

constroiMSetAux ::Eq a => [a] -> Int 
constroiMSetAux []       = 0
constroiMSetAux [a]      = 1
constroiMSetAux (h:ht:t) = if h == ht then 1 + constroiMSetAux (ht:t) else 1

-- 45

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:t) = Nothing
catMaybes (Just h:t) = h: catMaybes t

-- 46

data Movimento = Norte | Sul | Este | Oeste
		deriving Show

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x,y) (x2,y2)
    | x < x2 = Este : caminho (x+1,y) (x2,y2)
    | x > x2 = Oeste: caminho (x-1,y) (x2,y2)
    | y < y2 = Norte: caminho (x,y+1) (x2,y2)
    | y > y2 = Sul  : caminho (x,y-1) (x2,y2)
    | otherwise = []

-- 47 

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) []    = (x,y)
posicao (x,y) (h:t) = posicao (posicaox (x,y) h) t

posicaox (x,y) Norte = (x+1,y)
posicaox (x,y) Sul   = (x-1,y)
posicaox (x,y) Este  = (x,y+1)
posicaox (x,y) Oeste = (x,y-1)

hasLoops :: (Int,Int) -> [Movimento] -> Bool
hasLoops _ [] = False
hasLoops pi ms = pi == posicao pi ms || hasLoops pi (init ms)

-- 48

contaQuadrados :: [Rectangulo] -> Int
contaQuadrados [] = 0
contaQuadrados (h:t) 
    | eQuadrado h = 1 + contaQuadrados t
    | otherwise = contaQuadrados t

eQuadrado :: Rectangulo -> Bool
eQuadrado (Rect (x1,y1) (x2,y2)) = abs (y2 - y1) == abs (x2 - x1)


-- 49

type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto

areaTotal :: [Rectangulo] -> Float
areaTotal [] = 0
areaTotal ((Rect (x1,y1) (x2,y2)):t) = abs (x2 - x1) * abs (y2 - y1) + areaTotal t

-- 50

data Equipamento = Bom | Razoavel | Avariado
    deriving Show

naoReparar :: [Equipamento] -> Int
naoReparar [] = 0
naoReparar (Bom:t) = 1 + naoReparar t
naoReparar (Razoavel:t) = 1 + naoReparar t
naoReparar (Avariado:t) = naoReparar t




