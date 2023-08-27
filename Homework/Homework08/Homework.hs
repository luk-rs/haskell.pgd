-- This homework is around creating Haskell types that represent wines from over the world.

-- Question 1
-- Different wines are made from different grapes, there are around 10000 varieties over the world!
-- Create a type synonym called "Grape" for the different grape names as strings.
-- Additionally, use this type synonym for the grapes: "Sangiovese", "Cabernet-sauvignon", "Merlot" and "Garnacha".

type Grape = String

grapeSamples :: [Grape]
grapeSamples = ["Sangiovese", "Cabernet-sauvignon", "Merlot", "Garnacha"]

-- Question 2
-- The most famous regions that export wine are located in France, Italy and Spain.
-- Each of these countries is divided up in smaller regions.
-- These smaller regions are known for a certain style, for example the Champagne region in France
-- Create a type synonym called "Region" for wine region given their country and region as a tuple of strings.
-- Additionally, use this type synonym for the regions: Bordeaux in France, Tuscany in Italy and Rioja in Spain.

type Region = (String, String)

regionSamples :: [Region]
regionSamples = [("Bordeaux", "France"), ("Tuscany", "Italy"), ("Rioja", "Spain")]

-- Question 3
-- A wine is either one of three kinds, these are red, white or rose wine.
-- Besides its kind, each wine also has a given alcohol level.
-- Create a data type called "Kind" that represents these three kinds, with each capturing the level of alcohol.
-- Additionally, use this data type for the examples: red wine with 14.5% alcohol, white wine with 13% alcohol
-- and Rose wine with 12% alcohol.

data Kind a = Red a | White a | Rose a

wineSamples :: [Kind Float]
wineSamples = [Red 14.5, White 13, Rose 12]

-- Question 4
-- In the world of wines, bottles display all of the above information for the consumer on its label.
-- Create a record type called "Label" that captures the grapes that are in a whine, the region its from,
-- and it's kind. Notice that some wines are a blended combination of multiple grapes!
-- Additionally, create for each of the described wine below a label.

data Label = Label
  { grapes :: [Grape],
    region :: Region,
    kind :: Kind Float
  }

-- Larrosa Rose is a rose wine from the region Rioja. It is made from the Garnacha grape and
-- has a alcohol level of 14%.

larrosaRose :: Label
larrosaRose = Label ["Garnacha"] ("Rioja", "Spain") $ Red 14

-- Castiglioni is a red wine from the region of Tuscany. It is made from the grape Sangiovese and
-- has an alcohol level of 12.5%.

castiglioni :: Label
castiglioni = Label [head grapeSamples] (regionSamples !! 1) (Red 12.5)

-- Bordeaux is known for its red wine, these are mainly a blend between Cabernet-sauvignon and Merlot.
-- Create a Label for the wine "Le Petit Haut Lafitte" that has an alcohol percentage 13.5%.

lePetitHaitLafitte :: Label
lePetitHaitLafitte = Label ["Merlot", grapeSamples !! 1] (head regionSamples) (Red 13.5)

-- Question 5
-- Write a function `containsGrape` that takes a list of Labels and a Grape and returns a boolean.
-- The function should check if the there exists a wine in the Label that contains this Grape.

-- This is a test list for the `containsGrape` function with an grape that is not in the list.

grapeList :: [Label]
grapeList = [larrosaRose, castiglioni, lePetitHaitLafitte]

newGrape :: Grape
newGrape = "Pinot Noir"

contains :: [Label] -> Grape -> Bool
contains labels grape = foldr containsGrape False labels -- (\l acc -> acc || grape `elem` grapes l)
  where
    containsGrape Label {grapes} acc = acc || grape `elem` grapes
