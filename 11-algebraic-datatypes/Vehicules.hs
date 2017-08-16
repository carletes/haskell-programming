module Vehicules where

data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini
                  | Mazda
                  | Tata
                  deriving (Eq, Show)

data Airline = PapuAir
             | CatapultsR'Us
             | TakeYourChancesunited
             deriving (Eq, Show)

data Vehicle = Car Manufacturer
             | Plane Airline
             deriving (Eq, Show)

isCar :: Vehicle -> Bool
isCar (Car _) = True
isCar _       = False

isPlane :: Vehicle -> Bool
isPlane (Plane _) = True
isPlane _         = False

areCars :: [Vehicle] -> Bool
areCars = all isCar
