# Chapter 12: Signaling adversity

## Smart constructors

A way to ensure custom datatypes are created only when argumets make
sense:

    type Name = String
	type Age = Integer

	data Person = Person Name Age deriving Show

	mkPerson :: Name -> Age -> Maybe Person
	mkperson name age
	  | name /= "" && age >= 0 = Just $ Person name age
	  | otherwise = Nothing
