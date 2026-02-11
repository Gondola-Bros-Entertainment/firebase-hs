{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Firebase.Auth
import Network.HTTP.Types.Header (hCacheControl)
import System.Exit (exitFailure)

main :: IO ()
main = do
  putStrLn "firebase-hs tests"
  putStrLn "=================="
  results <-
    sequence
      [ test "defaultFirebaseConfig sets project ID" testDefaultConfigProjectId,
        test "defaultFirebaseConfig sets 300s clock skew" testDefaultConfigClockSkew,
        test "parseCacheMaxAge parses valid header" testParseCacheMaxAgeValid,
        test "parseCacheMaxAge handles missing header" testParseCacheMaxAgeMissing,
        test "parseCacheMaxAge handles malformed value" testParseCacheMaxAgeMalformed,
        test "parseCacheMaxAge rejects zero" testParseCacheMaxAgeZero,
        test "parseCacheMaxAge rejects negative" testParseCacheMaxAgeNegative,
        test "FirebaseUser Eq instance" testFirebaseUserEq,
        test "AuthError constructors" testAuthErrorConstructors
      ]
  let failures = length (filter not results)
  if failures > 0
    then do
      putStrLn ("\n" ++ show failures ++ " test(s) FAILED")
      exitFailure
    else putStrLn "\nAll tests passed."

-- ---------------------------------------------------------------------------
-- Test runner
-- ---------------------------------------------------------------------------

test :: String -> IO Bool -> IO Bool
test name action = do
  result <- action
  putStrLn (indicator result ++ " " ++ name)
  pure result
  where
    indicator True = "  PASS"
    indicator False = "  FAIL"

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO Bool
assertEqual label expected actual
  | expected == actual = pure True
  | otherwise = do
      putStrLn ("    " ++ label ++ ": expected " ++ show expected ++ ", got " ++ show actual)
      pure False

-- ---------------------------------------------------------------------------
-- Config tests
-- ---------------------------------------------------------------------------

testDefaultConfigProjectId :: IO Bool
testDefaultConfigProjectId =
  assertEqual "fcProjectId" "test-project" (fcProjectId (defaultFirebaseConfig "test-project"))

testDefaultConfigClockSkew :: IO Bool
testDefaultConfigClockSkew =
  assertEqual "fcClockSkew" 300 (fcClockSkew (defaultFirebaseConfig "test-project"))

-- ---------------------------------------------------------------------------
-- Cache-Control parsing tests
-- ---------------------------------------------------------------------------

testParseCacheMaxAgeValid :: IO Bool
testParseCacheMaxAgeValid =
  assertEqual
    "max-age=19845"
    (Just 19845)
    (parseCacheMaxAge [(hCacheControl, "public, max-age=19845, must-revalidate, no-transform")])

testParseCacheMaxAgeMissing :: IO Bool
testParseCacheMaxAgeMissing =
  assertEqual
    "no Cache-Control header"
    Nothing
    (parseCacheMaxAge [("content-type", "application/json")])

testParseCacheMaxAgeMalformed :: IO Bool
testParseCacheMaxAgeMalformed =
  assertEqual
    "max-age=abc"
    Nothing
    (parseCacheMaxAge [(hCacheControl, "max-age=abc")])

testParseCacheMaxAgeZero :: IO Bool
testParseCacheMaxAgeZero =
  assertEqual
    "max-age=0"
    Nothing
    (parseCacheMaxAge [(hCacheControl, "max-age=0")])

testParseCacheMaxAgeNegative :: IO Bool
testParseCacheMaxAgeNegative =
  assertEqual
    "max-age=-1"
    Nothing
    (parseCacheMaxAge [(hCacheControl, "max-age=-1")])

-- ---------------------------------------------------------------------------
-- Type tests (compilation + basic Eq/Show)
-- ---------------------------------------------------------------------------

testFirebaseUserEq :: IO Bool
testFirebaseUserEq =
  let u1 = FirebaseUser "uid1" (Just "a@b.com") (Just "Alice")
      u2 = FirebaseUser "uid1" (Just "a@b.com") (Just "Alice")
   in assertEqual "same users" u1 u2

testAuthErrorConstructors :: IO Bool
testAuthErrorConstructors = do
  let errors =
        [ KeyFetchError "network error",
          InvalidSignature,
          TokenExpired,
          InvalidClaims "bad aud",
          MalformedToken "not a jwt"
        ]
  assertEqual "5 constructors" 5 (length errors)
