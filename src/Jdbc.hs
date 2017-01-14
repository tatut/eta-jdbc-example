{-# LANGUAGE MagicHash, TypeFamilies, DataKinds #-}
module Jdbc where

import Java

-- Define the Java classes we need from java.sql package:
-- Connection, PreparedStatement and ResultSet

data {-# CLASS "java.sql.Connection" #-} Connection = Connection (Object# Connection)
  deriving (Class, Show, Eq)

data {-# CLASS "java.sql.PreparedStatement" #-} PreparedStatement = PreparedStatement (Object# PreparedStatement)
  deriving (Class, Show)

data {-# CLASS "java.sql.ResultSet" #-} ResultSet = ResultSet (Object# ResultSet)
  deriving (Class, Show)

-- Import Java methods we need

foreign import java unsafe "@static DB.connect"
  connect :: String -> String -> String -> IO Connection

foreign import java unsafe "@interface prepareStatement"
  prepareStatement :: Connection -> String -> IO PreparedStatement

foreign import java unsafe "@interface setInt"
  setIntParameter :: PreparedStatement -> Int -> Int -> IO ()
foreign import java unsafe "@interface setString"
  setStringParameter :: PreparedStatement -> Int -> String -> IO ()
foreign import java unsafe "@interface setBoolean"
  setBoolParameter :: PreparedStatement -> Int -> Bool -> IO ()

foreign import java unsafe "@interface executeQuery"
  executeQuery :: PreparedStatement -> IO ResultSet

foreign import java unsafe "@interface next"
  next :: ResultSet -> IO Bool

foreign import java unsafe "@interface getInt"
  getInt :: ResultSet -> String -> IO Int

foreign import java unsafe "@interface getString"
  getString :: ResultSet -> String -> IO String

foreign import java unsafe "@interface close"
  closeResultSet :: ResultSet -> IO ()
foreign import java unsafe "@interface close"
  closePreparedStatement :: PreparedStatement -> IO ()

-- Define different types of query parameters and helpers
data QueryParameter = StringQueryParameter String | IntQueryParameter Int | BoolQueryParameter Bool

intParam = IntQueryParameter
stringParam = StringQueryParameter
boolParam = BoolQueryParameter


setPreparedStatementParams :: PreparedStatement -> Int -> [QueryParameter] -> IO PreparedStatement
setPreparedStatementParams ps i [] = do return ps
setPreparedStatementParams ps i (param:params) = do
  case param of
    (IntQueryParameter p) -> setIntParameter ps i p
    (StringQueryParameter p) -> setStringParameter ps i p
    (BoolQueryParameter p) -> setBoolParameter ps i p
  setPreparedStatementParams ps (i + 1) params

-- Define an interface how a row in the ResultSet is turned into a value
class ResultRow a where
  parseResultRow :: ResultSet -> IO a

-- Read the resultset into a list of values
parseRows :: ResultRow a => ResultSet -> [a] -> IO [a]
parseRows rs acc = do
  more <- next rs
  if more
    then do row <- parseResultRow rs
            parseRows rs (row:acc)
    else return (reverse acc)

-- Execute a SQL query: Takes a connection, a SQL string and list of parameters
-- Returns a list of result values
query :: ResultRow a => Connection -> String ->  [QueryParameter] -> IO [a]
query c q params = do
  ps <- prepareStatement c q
  ps <- setPreparedStatementParams ps 1 params
  rs <- executeQuery ps
  result <- parseRows rs []
  closeResultSet rs
  closePreparedStatement ps
  return result
