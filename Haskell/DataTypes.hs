--a is a TYPE PARAMETER, NOT A VARIABLE
--a defines the type used, in this case any type you want, but just the one you use, not multiple types
data Punkt a = Punkt a a
data Strecke a = Strecke (Punkt a) (Punkt a) 
data Vektor a = Vektor a a 
data Polygon a = Polygon [Punkt a]

p :: Punkt a -> a
p (Punkt a _) = a


 