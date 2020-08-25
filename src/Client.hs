module Client where

data Client = GovOrg     String
            | Company    String Integer Person String
            | Individual Person Bool
            deriving Show

data Person = Person String String Gender
            deriving Show

data Gender = Male | Female | Unknown
            deriving Show
            
data GenderHistogram = GenderHistogram Int Int 
            deriving (Show, Eq)
            
genderHistogram :: [Client] -> GenderHistogram
genderHistogram [] = GenderHistogram 0 0
genderHistogram (client:clients) =
  case client of
    Individual (Person _ _ gender) _ ->
      case gender of
         Male -> GenderHistogram (males+1) females
         Female -> GenderHistogram males (females+1)
         _ -> genderHistogram clients
      where
          GenderHistogram males females = genderHistogram clients
    _ -> genderHistogram clients