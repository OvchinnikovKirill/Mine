import System.Random
import System.IO
import System.Environment	
import Prelude
import Kvad
import Text.ParserCombinators.Parsec
import Data.List.Split
--import Monstupar

--type Parser a = String->a
--data Token = String

--float :: Parser Double
--float = do
		--let a = read a :: Float
		--return a
		
--koord :: Parser Token
--koord = do
		--a<-float
		--space
		--b<-float
		--space
		--c<-String
		--return(Kord a b c)
data Bomb = Bomb {absc::Float,ord::Float,rad::Float}
		
makeKord ::  Kord
makeKord = (Kord (read "0.12131" :: Float) (read "123123.1231" :: Float) "Gorog 1123")

funct ::IO Float
funct = getStdRandom (randomR (-100.0,100.0))

--funct1 :: Koord -> Kord
--funct1 (a++" "++b++" "++c) = Kord ((a::Float) (b::Float) c)
--funct1 (a,b,c) = (Kord a b c) 
 


setgor 101 outh = hPutStrLn outh ("0.0"++" "++"0.0"++" Gorod"++"101")
setgor i outh = do 
		xs <- funct
		x1 <- funct
		let res = show xs
		let res1 = show x1
		let ii = show i
		hPutStrLn outh (res++" "++res1++" Gorod"++ii)
		setgor (i+1) outh
		
--infas "0.0 0.0 Gorod101" a = (iKD (Leaf []) (Kord 0.0 0.0 "Gorod101"))
--infas x b = do
		--let a = (Leaf [])
		--let a = iKD (b) (Kord (read x :: Float) (read x :: Float) (read x :: String))
		--return a

--infa :: IO () -> KD a -> Int -> KD a
--readThat (absc,ord,name) = Kord (read absc :: Float) 
                                  --    (read ord :: Float)
									--  (read name :: String)

  
--kordM ::[((Float, Float), String)] -> Kord
--kordM [((a,b),c)] = Kord a b c
--char2float :: Char -> Float
--char2float n = fromInteger (read [n])
--parseKord :: String -> Parse Token
--parseKord x2 = do
		--a<-parsefloat x2
		--char2float a
		--space
		--b<-parsefloat x2
		--char2float b
		--space
		--c<-parsestring x2
		--return (Kord (read a::Float) (read b::Float) c)

--parsestring x2= do
		--c<-letters
		--cs<-many letters
		--return (c:cs)
--parsefloat x2= do
		--c<-numbers
		--cs<- many numbers
		--return (c:cs)
		
--letters = oneOf(['a'..'z'] ++ ['A'..'Z']++['0'..'9'])
--numbers = oneOf ['0'..'9'] ++ ['.']
listToBomb (a:b:c:[]) = (Bomb (read a::Float) (read b::Float) (read c::Float))

listToKord [] = (Kord 9999.9999 9999.9999 "ololo")
listToKord (a:b:c:[]) = (Kord (read a::Float) (read b::Float) c)

infa (x:xs) a 101 = (iKD (a) (Kord 0.0 0.0 "Gorod101"))
infa [] a i = a		
infa (x:xs) a i = do
		--let x2 = x
		let e = splitOn " " x
		let f = listToKord e
		--let f = parseKord x2
		--let b = (iKD (a) (f))
		infa xs (iKD (a) (f)) (i+1)


printKord [] = putStrLn (" Leaf")
printKord ((Kord a b c):xs) = do
			putStr((show a)++" "++(show b)++" "++c++" ")
			printKord xs
		
printKD (Leaf k) = do
		--putStr(show(length k))
		printKord k
printKD (Node (Kord x y a1) c1 c2 c3 c4 ) = do
		putStrLn("Node "++(show x)++" "++(show y)++" "++a1)
		printKD c1
		printKD c2
		printKD c3
		printKD c4
--infas x2 a i xs = do
		
		--infa b (i+1) xs
		--b <- infa x a
		--let a = (Leaf [])
		--iKD (a) (Kord (read x :: Float) (read x :: Float) (read x :: String))
		--infa inh
		--case(parse koord "some text" x) of
		--Left err -> putStr "Parse error: " >> print err
		--Right b  -> iKD (a) (b)
--bomb :: IO String -> Kord		
bomb a = do
		let d1 = splitOn " " a
		listToBomb d1
--poiskDestr [] = 
poiskBomb (Leaf []) b = putStr("")
poiskBomb (Leaf ((Kord a b c):xs)) (Bomb a1 a2 a3)= do
		if (sqrt(abs(a-a1)^2+(abs(b-a2)^2))<a3) then (putStrLn(c++" DESTROYED ON "++(show ((100-(100*sqrt(abs(a-a1)^2+(abs(b-a2)^2)))/a3)))++"%")) else (putStr(""))
		poiskBomb (Leaf xs) (Bomb a1 a2 a3)
poiskBomb (Node (Kord x y a1) c1 c2 c3 c4)  b = do
		poiskBomb c1 b
		poiskBomb c2 b
		poiskBomb c3 b
		poiskBomb c4 b
		
main::IO () 
main = do 
		outh <- openFile "file.out" WriteMode
		setgor 1 outh
		--hClose inh
		hClose outh
		run
		

run = do
		inh <- readFile "file.out"
		let (x:xs) = lines inh
		let a = (Leaf [])
		--print(parseLine x)
		--print(splitOn " " x)
		let c = infa (x:xs) a 1
		--printKD c
		line <- getLine
		--let d3 = bomb d
		--let c1 = iKD (c) d3
		--printKD (iKD (c) (bomb line))
		poiskBomb (c) (bomb line)
		--return ()

--operate :: String
--operate = "aaa"

 


--inttostr :: Int -> String
--inttostr a = show a
