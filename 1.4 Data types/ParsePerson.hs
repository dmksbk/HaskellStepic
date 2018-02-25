module ParsePerson where

import Data.Char (isDigit)

data Error = ParsingError | IncompleteDataError | IncorrectDataError String
	deriving Show

data Person = Person { firstName :: String, lastName :: String, age :: Int }
	deriving Show

parsePerson :: String -> Either Error Person
parsePerson str = case lines str of
	 [str1, str2, str3] -> case words str3 of
	 	["age", "=", a] -> if all isDigit a
	 		then case words str1 of
	 			["firstName", "=", fn] -> case words str2 of
	 				["lastName","=", ln] -> Right $ Person fn ln $ read a
	 				_				   -> Left $ ParsingError
	 			_					   -> Left $ ParsingError
	 		else Left $ IncorrectDataError a
	 	_				-> Left $ ParsingError 
	 _					-> Left $ IncompleteDataError

exString = "firstName = John\nlastName = Connor\nage = 30"