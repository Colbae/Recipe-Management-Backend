{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (Result (..), Value (..), fromJSON, object, (.=))
import Data.Either (isLeft, isRight)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (fromGregorian)
import Domain.Food (FoodReference (..), Provider (USDA))
import Domain.Ingredients (combineIngredients)
import Domain.IngredientsFilter (filterRecipes)
import qualified Domain.MenuPlan as M
import qualified Domain.Nutrition as N
import Domain.Pantry (subtractIngredients)
import qualified Domain.Recipe as R
import Domain.RecipeParser (recipeParser)
import Domain.RecipeRenderer (renderRecipe)
import qualified Domain.Units as U
import Test.Hspec
import Text.Megaparsec (parse)

valid :: (Show e) => Either e a -> a
valid = either (error . show) id

ingredient :: Text -> Double -> U.Unit -> R.Ingredient
ingredient label quantity measure = valid (R.mkIngredients label quantity measure Nothing)

grams :: U.Unit
grams = U.WeightUnit U.Gram

recipe :: Int64 -> Int -> [R.Ingredient] -> R.Recipe
recipe rid portions items =
  R.mkRecipe rid $
    valid $
      R.mkRecipeCreateInput 1 $
        valid (R.mkRecipeInput "Example" portions items ["Cook"])

main :: IO ()
main = hspec $ do
  describe "Recipe text parsing" $ do
    let input = "Title: Oatmeal\nServings: 2\n\nIngredients:\n100 g oats @usda:123\n200 ml milk\n\nInstructions:\nCook: simmer gently.\nServe warm."
    it "parses multiple ingredients and plain instruction lines, including a USDA reference" $ do
      let parsed = valid (parse recipeParser "example" input)
      R.title parsed `shouldBe` "Oatmeal"
      R.servings parsed `shouldBe` 2
      map R.amount (R.ingredients parsed) `shouldBe` [100, 200]
      R.food (head (R.ingredients parsed)) `shouldBe` Just (FoodReference USDA 123)
      R.instructions parsed `shouldBe` ["Cook: simmer gently.", "Serve warm."]
    it "accepts Windows line endings and a trailing newline" $
      parse recipeParser "example" (T.replace "\n" "\r\n" input <> "\r\n") `shouldSatisfy` isRight
    it "rejects invalid units, malformed references, and duplicate sections" $ do
      mapM_
        (\bad -> parse recipeParser "example" bad `shouldSatisfy` isLeft)
        [T.replace "100 g" "100 xyz" input, T.replace "usda:123" "usda:abc" input, input <> "\nServings: 3"]

  describe "Recipe text export" $ do
    it "round-trips a recipe with decimal amounts, units and a food reference" $ do
      let linked = valid (R.mkIngredients "oats" 12.5 grams (Just (FoodReference USDA 123)))
          input = valid (R.mkRecipeInput "Oatmeal" 2 [linked, ingredient "milk" 200 (U.VolumeUnit U.Mililiter)] ["Cook: simmer gently.", "Serve."])
      parse recipeParser "export" (valid (renderRecipe input)) `shouldBe` Right input
    it "rejects reserved characters, multiline values and whitespace loss" $ do
      let input label steps = valid (R.mkRecipeInput "Example" 1 [ingredient label 100 grams] steps)
      mapM_
        (\r -> renderRecipe r `shouldSatisfy` isLeft)
        [ input "flour@home" ["Cook"],
          input "flour\nwater" ["Cook"],
          input " flour " ["Cook"],
          input "flour" ["Instructions: surprise"],
          input "flour" ["Cook\nServe"]
        ]

  describe "Shopping requirements" $ do
    it "merges compatible quantities before subtracting the pantry once" $ do
      let entries = [recipe 1 1 [ingredient "flour" 100 grams], recipe 2 1 [ingredient "flour" 0.2 (U.WeightUnit U.Kilogram)]]
          shopping =
            combineIngredients (R.extractIngredientsFromList entries)
              >>= (\required -> subtractIngredients required [ingredient "flour" 50 grams])
      fmap (map R.amount) shopping `shouldBe` Right [250]
    it "normalizes each recipe separately and counts repeated meals" $ do
      let meal = recipe 1 4 [ingredient "flour" 200 grams]
      map R.amount (R.extractNormalizedIngredientsFromList [meal, meal]) `shouldBe` [50, 50]
    it "does not merge incompatible dimensions or different food references" $ do
      let linked fid = valid (R.mkIngredients "flour" 10 grams (Just (FoodReference USDA fid)))
      fmap length (combineIngredients [linked 1, linked 2, ingredient "flour" 1 (U.VolumeUnit U.Liter)]) `shouldBe` Right 3

  describe "Ingredient subtraction" $ do
    it "matches names case-insensitively, converts units, and consumes across duplicate entries" $ do
      let stock = [ingredient "Flour" 100 grams, ingredient "flour" 200 grams]
      fmap (map R.amount) (subtractIngredients stock [ingredient " flour " 0.15 (U.WeightUnit U.Kilogram)]) `shouldBe` Right [150]
    it "removes exhausted ingredients without producing negative amounts" $
      subtractIngredients [ingredient "flour" 100 grams] [ingredient "flour" 200 grams] `shouldBe` Right []
    it "leaves unrelated ingredients and incompatible units unchanged" $ do
      let stock = [ingredient "flour" 100 grams]
      subtractIngredients stock [ingredient "sugar" 10 grams, ingredient "flour" 1 (U.VolumeUnit U.Liter)] `shouldBe` Right stock

  describe "Nutrition addition" $ do
    let protein = N.Nutrient 1003 "Protein"
        g = N.NutrientWeightUnit N.Gram
    it "adds by nutrient ID, converts units, and keeps different nutrients separate" $ do
      let fat = N.Nutrient 1004 "Fat" g (Just 2)
          result = N.sumNutrition [[protein g (Just 10), fat], [protein (N.NutrientWeightUnit N.Milligram) (Just 1000)]]
      fmap (map N.nutrientAmount . N.reportNutrients) result `shouldBe` Right [Just 11, Just 2]
    it "treats missing amounts and absent nutrient entries as zero" $
      fmap
        (map N.nutrientAmount . N.reportNutrients)
        (N.sumNutrition [[protein g (Just 10)], [protein g Nothing], []])
        `shouldBe` Right [Just 10]
    it "rejects invalid or duplicate source amounts" $ do
      N.sumNutrition [[protein g (Just (-1))]] `shouldSatisfy` isLeft
      N.sumNutrition [[protein g (Just 1), protein g (Just 2)]] `shouldSatisfy` isLeft
    it "combines an empty list of reports" $
      N.combineReports [] `shouldBe` Right (N.NutritionReport [])

  describe "Recipe filtering" $ do
    let meal = recipe 1 1 [ingredient "flour" 100 grams, ingredient "flour" 50 grams]
    it "requires enough for all duplicate ingredients and converts compatible units" $
      filterRecipes [ingredient "FLOUR" 0.15 (U.WeightUnit U.Kilogram)] [meal] `shouldBe` [meal]
    it "rejects insufficient stock and incompatible units" $ do
      filterRecipes [ingredient "flour" 100 grams] [meal] `shouldBe` []
      filterRecipes [ingredient "flour" 1 (U.VolumeUnit U.Liter)] [meal] `shouldBe` []

  describe "Validation" $ do
    it "collects errors across fields and multiple ingredients" $ do
      let bad =
            object
              [ "title" .= String "",
                "servings" .= (0 :: Int),
                "ingredients" .= [object ["name" .= String "", "amount" .= (-1 :: Int), "unit" .= String "invalid"], object []],
                "instructions" .= [String ""]
              ]
      case R.validateRecipeJSON bad of
        Right _ -> expectationFailure "Accepted an invalid recipe"
        Left errors ->
          mapM_
            (\label -> T.intercalate "\n" errors `shouldSatisfy` T.isInfixOf label)
            ["title", "servings", "ingredients[0]: name", "ingredients[0]: amount", "ingredients[0]: unit", "ingredients[1]", "instructions[0]"]
    it "rejects non-positive and non-finite ingredient quantities" $
      mapM_ (\quantity -> R.mkIngredients "flour" quantity grams Nothing `shouldSatisfy` isLeft) [0, -1, 0 / 0, 1 / 0]
    it "rejects reversed menu-plan dates through the constructor and JSON" $ do
      M.mkMenuPlanInput (fromGregorian 2026 11 20) (fromGregorian 2026 11 19) `shouldSatisfy` isLeft
      case fromJSON (object ["startDate" .= String "2026-11-20", "endDate" .= String "2026-11-19"]) :: Result M.MenuPlanInput of
        Error _ -> pure ()
        Success _ -> expectationFailure "JSON accepted reversed menu-plan dates"
