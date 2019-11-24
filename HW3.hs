module HW3 where

-- 패턴 매칭 (설명 예시 코드)

f :: Integer -> Bool
f 3  =  True
f 5  =  False

f' :: Integer -> Bool
f' 3  =  True
f' n  =  False

f'' :: Integer -> Bool
f'' n  =  False
-- f'' 3  =  True
-- 계속 reload 과정에서 warning이 출력되서 주석 처리함, 201811169 김재현, 191109.19:29


fac :: Integer -> Integer
-- 윗줄의 주석을 풀고, 여기에 fac 함수를 정의하셔요.
fac 0 = 1
fac b = b * fac (b-1)

rev :: [a] -> [a]
-- 윗줄의 주석을 풀고, 여기에 rev 함수를 정의하셔요.
rev [] = []
rev (x:xs) = rev xs ++ [x]

-- 타입 정의 (설명 예시 코드)

data List t  =  EmptyList | ListCons t (List t)

data Nat  =  Z
          |  S Nat
    deriving Show

toNat :: Integral i => i -> Nat
toNat 0          =  Z
toNat n | n > 0  =  S (toNat (n-1))



fromNat :: Integral i => Nat -> i
-- 윗줄의 주석을 풀고, 여기에 fromNat 함수를 정의하셔요.
fromNat Z = 0
fromNat (S x) = 1 + (fromNat x)

addNat :: Nat -> Nat -> Nat
-- 윗줄의 주석을 풀고, 여기에 addNat 함수를 정의하셔요.
addNat Z n = n
addNat n Z = n
addNat (S x) (S y) = (S (S (addNat x y)))

mulNat :: Nat -> Nat -> Nat
-- 윗줄의 주석을 풀고, 여기에 mulNat 함수를 정의하셔요.
mulNat Z n = Z
mulNat n Z = Z
mulNat (S x) (S y) = addNat (addNat x y) (addNat (S Z) (mulNat x y))

facNat :: Nat -> Nat
-- 윗줄의 주석을 풀고, 여기에 facNat 함수를 정의하셔요.
facNat Z = (S Z)
facNat (S x) = mulNat (S x) (facNat x)