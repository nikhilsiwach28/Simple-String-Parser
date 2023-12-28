import Data.Aeson(encode, toJSON, ToJSON)  -- external Imports 
import Network.Wai.Handler.Warp (run)


import ParserService  -- internal Imports
import Server

main :: IO ()   --Example function without API
main = do
  let encodedString = "#00date|1997-02-06#02name|bob#01age|20#03hasPassport|Y#13access|y,n,F,f"
  let maybeJson = parseToJSON encodedString
  case maybeJson of
    Just json -> do 
      print (encode (toJSON json))
    Nothing -> putStrLn "Invalid encoding format"
    


main :: IO ()
main = run 8080 app

