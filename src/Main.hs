{-# LANGUAGE OverloadedStrings #-}
module Main where

import Java
import Jdbc

-- Connect to Apache Derby embedded by path
connectDB :: String -> IO Jdbc.Connection
connectDB path = Jdbc.connect ("jdbc:derby:" ++ path) "" ""

-- Define a record type that holds our result row
data CitiesInCountry = CitiesInCountry { country :: String, numberOfCities :: Int} deriving Show

-- Define how this result is read from a JDBC ResultSet
instance ResultRow CitiesInCountry where
  parseResultRow rs = do
    country <- getString rs "country"
    cities <- getInt rs "cities"
    return $ CitiesInCountry { country = country, numberOfCities = cities }

-- Print out a country and its city count
showCountry (CitiesInCountry { country = c, numberOfCities = n }) = do
  putStrLn $ "Country " ++ c ++ " has " ++ (show n) ++ " cities."


main = do
  con <- connectDB "/Users/tatuta/Downloads/db-derby-10.13.1.1-bin/demo/databases/toursdb"
  countries <- query con
            "SELECT COUNT(*) as cities, country FROM cities WHERE country IN (SELECT country FROM countries WHERE region = ?) GROUP BY country"
            [stringParam "Asia"]
  mapM_ showCountry countries
  let sum = foldl (+) 0 (map (\(CitiesInCountry {numberOfCities = num }) -> num) countries)
  putStrLn $ "Total cities in list:  " ++ (show sum)
