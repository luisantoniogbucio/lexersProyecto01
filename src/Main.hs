module Main where

import Token
import RegExp
import NFA
import DFA
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

-- Patrones para tokens del lenguaje IMP
impTokens :: [(RegExp, Token)]
impTokens = 
  [ (cadena "skip", TSkip)
  , (cadena "if", TIf)
  , (cadena "then", TThen)
  , (cadena "else", TElse)
  , (cadena "while", TWhile)
  , (cadena "do", TDo)
  , (cadena "true", TTrue)
  , (cadena "false", TFalse)
  , (cadena "not", TNot)
  , (cadena "and", TAnd)
  , (Concat letra (Star (Union letra digito)), TId "")
  , (unoOMas digito, TNum 0)
  , (Char '+', TPlus)
  , (Char '-', TMinus)
  , (Char '*', TTimes)
  , (Char '=', TEq)
  , (cadena "<=", TLeq)
  , (cadena ":=", TAssign)
  , (Char ';', TSemi)
  , (Char '(', TLParen)
  , (Char ')', TRParen)
  ]

-- Convierte string a DFA para reconocimiento
stringToDFA :: String -> DFA
stringToDFA str = nfaADFA (construirNFA (cadena str))

-- Reconoce si un string pertenece al lenguaje IMP
reconocerIMP :: String -> Maybe Token
reconocerIMP input = 
  case filter (reconoce input . fst) impTokens of
    [] -> Nothing
    ((_, token):_) -> Just token
  where
    reconoce str regex = 
      let dfa = nfaADFA (construirNFA regex)
      in aceptaDFA dfa str

-- Verifica si DFA acepta una cadena
aceptaDFA :: DFA -> String -> Bool
aceptaDFA dfa input = 
  case foldl (transicion dfa) (Just (estadoInicialDFA dfa)) input of
    Just estado -> estado `Set.member` estadosFinalesDFA dfa
    Nothing -> False
  where
    transicion dfa (Just estado) c = 
      Map.lookup (estado, c) (transicionesDFA dfa)
    transicion _ Nothing _ = Nothing

main :: IO ()
main = do
  putStrLn "IMP Language Lexer"
  let testStrings = ["if", "while", "123", "variable", "+", ":="]
  mapM_ (\s -> do
    putStrLn $ "Input: " ++ s
    case reconocerIMP s of
      Just token -> putStrLn $ "Token: " ++ show token
      Nothing -> putStrLn "No reconocido"
    let dfa = stringToDFA s
    putStrLn $ "DFA estados: " ++ show (Set.size (estadosDFA dfa))
    putStrLn ""
    ) testStrings
