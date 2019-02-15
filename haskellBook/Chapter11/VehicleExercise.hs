module Vehicle where

data Price = Price Integer deriving (Eq, Show)
data Size = Size Integer deriving (Eq, Show)
data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)
data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)
data Vehicle = Car Manufacturer Price | Plane Airline Size deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Plane PapuAir

-- | 1) What is type of myCar?
-- Vehicle

-- | 2)
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False
 
isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

-- | 3)
getManu :: Vehicle -> Maybe Manufacturer
getManu (Car a _) = Just a
getManu _ = Nothing 

-- | 4) 
-- Bottom

-- | 5)
