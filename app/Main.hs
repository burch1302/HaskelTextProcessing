module Main where

import Data.Char (isAlpha, isSpace, toLower)
import Data.List (sort)
import System.IO (withFile, IOMode(ReadMode), hSetEncoding, stdout, utf8, hGetContents)

type Symbol = Char

newtype WordT = WordT String 
    deriving (Show, Eq)

newtype Punctuation = Punctuation Char 
    deriving (Show, Eq)

data TextElement = EWord WordT | EPunct Punctuation 
    deriving (Show, Eq)

type Sentence = [TextElement]
type Textbook = [Sentence] 


instance Ord WordT where
    compare (WordT a) (WordT b) = compare (map toLower a) (map toLower b)

vowels :: String
vowels = "aeiouyAEIOUYаеєиіїоуюяАЕЄИІЇОУЮЯ"

isVowelStart :: WordT -> Bool
isVowelStart (WordT "") = False
isVowelStart (WordT (x:_)) = x `elem` vowels

normalizeWhitespace :: String -> String
normalizeWhitespace = unwords . words

tokenize :: String -> [TextElement]
tokenize [] = []
tokenize (c:cs)
    | isSpace c = tokenize cs
    | isAlpha c = 
        let (wordStr, rest) = span isAlpha (c:cs)
        in EWord (WordT wordStr) : tokenize rest
    | otherwise = EPunct (Punctuation c) : tokenize cs

unwrapWord :: WordT -> String
unwrapWord (WordT s) = s

solveTask1 :: String -> [String]
solveTask1 content = 
    let 
        cleanText = normalizeWhitespace content
        tokens = tokenize cleanText
        allWords = [w | EWord w <- tokens]
        vowelWords = filter isVowelStart allWords
        sortedWords = sort vowelWords
    in 
        map unwrapWord sortedWords


main :: IO ()
main = do
    hSetEncoding stdout utf8

    putStrLn "Зчитування файлу textbook.txt..."
    
    withFile "textbook.txt" ReadMode $ \handle -> do
        hSetEncoding handle utf8
        content <- hGetContents handle
        
        
        putStrLn "\n--- Оригінальний текст (нормалізований) ---"
        putStrLn (normalizeWhitespace content)
        
        putStrLn "\n--- Завдання 1: Слова на голосну літеру (відсортовані) ---"
        let result = solveTask1 content
        
        print result 
        
        putStrLn "\nГотово!"