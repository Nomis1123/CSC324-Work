
addMe :: Int -> Int -> Int

addMe x y = x + y

addTuple :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuple (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

data Bucket = MyBucket [Candy]
data Candy = Kitkat | Aero | Twix deriving (Show, Eq)





myLen :: [a] -> Int
myLen [] = 0
myLen (_:xs) = 1 + myLen xs

factorial :: Int -> Int
factorial 0 = 1
factorial n = fHelper [1..n] 1

fHelper [] acc = acc
fHelper (x:xs) acc = fHelper xs (x * acc)

data Shape = Circle Float | Rectangle Float Float deriving (Show, Eq)
area :: Shape -> Float
area (Circle r) = 3.12 * (r * r)
area (Rectangle s1 s2) = s1 * s2






g::Bucket -> Int
g (MyBucket candies) = gHelper candies 0

gHelper :: [Candy] ->Int -> Int
gHelper [] acc = acc
gHelper (Aero:xs) acc = gHelper xs (acc + 1)
gHelper (_:xs) acc = gHelper xs acc