module Main where

main :: IO ()
main = do
    putStrLn "Test"



-- var1 :: QuantityDict
-- var1 = mkQuantityDict S.Integer (mkUid "var1") "a1" "b1"

-- var2 :: QuantityDict
-- var2 = mkQuantityDict S.Boolean (mkUid "var2") "a2" "b2"

-- func1 :: QuantityDict
-- func1 = mkQuantityDict (S.Function (S.Integer NE.:| []) S.Integer) (mkUid "func1") "c1" "d1"

-- func1_dummy_var :: QuantityDict
-- func1_dummy_var = mkQuantityDict S.Integer (mkUid "func1_dummy_var") "input parameter" "input parameter"

-- func2 :: QuantityDict
-- func2 = mkQuantityDict (S.Function (NE.fromList [S.Boolean, S.Integer, S.Integer]) S.Integer) (mkUid "func2") "c2" "d2"

-- varDef1 :: QDefinition Expr
-- varDef1 = mkQDefinition (mkUid "varDef1") var1 (int 1) "e1"

-- varDef2 :: QDefinition Expr
-- varDef2 = mkQDefinition (mkUid "varDef2") var2 (not_ $ bool False) "e2"

-- funcDef1 :: QDefinition Expr
-- funcDef1 = mkFuncDefinition (mkUid "funcDef1") func1 [func1_dummy_var] (add [int 1, sy func1]) "explanation"

-- -- | If this chunk below is evaluated anywhere, it will cause an error.
-- funcDef1_BAD :: QDefinition Expr
-- funcDef1_BAD = mkFuncDefinition (mkUid "funcDef1") func1 [func1_dummy_var] (bool True) "explanation"

-- cdb :: ChunkDB
-- cdb =
--   insertAll' [varDef1, varDef2, funcDef1] $
--     insertAll' [var1, var2, func1, func1_dummy_var, func2] empty

-- cdbWithBadChunks :: ChunkDB 
-- cdbWithBadChunks = insert cdb funcDef1_BAD

-- test1 :: IO ()
-- test1 = do
--   putStrLn "Hello world!"
--   print $ uid (findOrErr (uid func1_dummy_var) cdb :: QuantityDict)
--   putStrLn "Expect an error to follow this message:"
--   print $ uid (findOrErr (uid func1_dummy_var) cdbWithBadChunks :: QuantityDict)

