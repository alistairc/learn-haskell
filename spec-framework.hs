data Failure = 
    Failure { message :: String }
    deriving (Show , Eq)

type Outcome = Maybe Failure

data Specification = 
    Specification {
        name :: String,
        result :: Outcome
    } deriving (Show , Eq)


shouldEqual :: (Eq a, Show a) => a -> a -> Outcome
shouldEqual actual expected
    | actual == expected = Nothing
    | otherwise = 
        Just $ Failure ("Expected <" ++ show expected ++ "> but was <" ++ 
        show actual ++ ">")

specify :: String -> Outcome -> Specification
specify name result = Specification name result

reportLine :: Specification -> String
reportLine (Specification name Nothing) = name ++ ": passed"
reportLine (Specification name (Just (Failure reason))) = name ++ ": failed: " ++ reason

specReport :: [Specification] -> String
specReport specs = unlines (map reportLine specs)

runSpecs :: [Specification] -> IO()
runSpecs specs = putStr $ specReport specs

-- temporary helper to get started with specs
assert :: String -> Bool -> IO()
assert name True = return()
assert name False = error (name ++ " failed!")


main = do
    runSpecs [
            specify "shouldEqual failure" $
                (10 `shouldEqual` 5) `shouldEqual` Just (Failure "Expected <5> but was <10>"),

            specify "shouldEqual success" $
                ("yes" `shouldEqual` "yes") `shouldEqual` Nothing,

            specify "reportLine for success" $
                (reportLine $ Specification "example spec" Nothing) `shouldEqual` "example spec: passed",

            specify "reportLine for failure" $
                (reportLine $ Specification "example spec" $ Just $ Failure "failure reason") 
                `shouldEqual` "example spec: failed: failure reason",

            specify "specReport (integration test)" $
                (
                    specReport [
                           specify "addition" ((1+2) `shouldEqual` 3),
                           specify "addition wrong" ((1+2) `shouldEqual` 5),
                           specify "division" ((6/2) `shouldEqual` 3),
                           specify "division wrong" ((6/2) `shouldEqual` 2)
                       ]
                )
                `shouldEqual` "addition: passed\n\
                   \addition wrong: failed: Expected <5> but was <3>\n\
                   \division: passed\n\
                   \division wrong: failed: Expected <2.0> but was <3.0>\n"
        ]    


