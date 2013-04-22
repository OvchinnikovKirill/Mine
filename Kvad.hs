module Kvad where
import Prelude
--import Data.Map as Map
import Data.List(foldl')
--data Kord =(Float,Float)
--class Summ a where
--(+) :: a -> a -> a


data Kord = Kord {absc::Float,ord::Float,name::String}


(+.) :: Kord -> Kord -> Kord
(Kord a b a1) +. (Kord c d a2) = (Kord (a + c) (b + d) (a1++a2))

(*.) :: Kord-> Float -> Kord
(Kord a b a1) *. (c) = (Kord (a*c) (b*c) (a1++"*"++(show c)))

--instance Summ Kord where
--(Kord a b) + (Kord c d) = (Kord (a + c) (b + d)) 
 
--summaKord (a,b) (c,d) = ((a+c),(b+d))
summaAllKord a = foldl' (+.) (Kord 0 0 "0") a
--delenKord (a,b) c = ((a/c),(b/c))
srednArifm a = (summaAllKord a) *. (1/5)



data KD a = Leaf [Kord] | Node Kord (KD a) (KD a) (KD a) (KD a) 



func (Leaf k) = (foldl iKD (Node (srednArifm k) e e e e) k) where e = Leaf []
--func = undefined

--delen (a,b) c = ((a/c),(b/c))
--func1 a [x1,x2,x3,x4,x5] = (iKD(iKD(iKD(iKD (iKD a x1) x2) x3) x4) x5)
--func1 a [] = a
--func1 a (x:xs) = (func1 (iKD a x) xs) 
 

--iKD1 (x:xs) a = (Pair x a):(iKD1 xs a)
--Up Down Right Left

iKD (Leaf k) (Kord a b c) =  if (((length k)+1)<5) then (Leaf ((Kord a b c) : k)) 
												   else (func (Leaf ((Kord a b c) : k)))
																   	
iKD (Node (Kord x y a1) c1 c2 c3 c4 ) (Kord x1 y1 a2) = if ((x1<x)&&(y1>y)) then (Node(Kord x y a1) (iKD c1 (Kord x1 y1 a2)) c2 c3 c4 )
															 else (if ((x1>x)&&(y1>y)) then (Node(Kord x y a1) c1 (iKD c2 (Kord x1 y1 a2)) c3 c4 )
																					   else (if ((x1<x)&&(y1<y)) then (Node(Kord x y a1) c1 c2 (iKD c3 (Kord x1 y1 a2))c4 )
																												 else (Node(Kord x y a1) c1 c2 c3 (iKD c4 (Kord x1 y1 a2))))) 

 


