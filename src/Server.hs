module Server where


-- import Data.HashMap.Strict (HashMap)
-- import Data.Text (Text)

-- import ErrorHandler
-- import ParserService




-- type MyAPI = "parse" :> QueryParam "data" String :> Get '[JSON] (Either (Int,Text) (HashMap Text Value))

-- parseData :: Maybe String -> Handler (Either Text (HashMap Text Value))
-- parseData maybeEncodedString = case maybeEncodedString of
--   Just encodedString -> case parseToJSON encodedString of
--     Just json -> return $ Right json
--     Nothing -> throwError $ toError InternalServerError
--   Nothing -> throwError $ toError InvalidDataError
--   where
--     toError err = T.pack . show . toHttpStatusCode $ err


-- server :: Server MyAPI
-- server = parseData

-- app :: Application
-- app = serve (Proxy :: Proxy MyAPI) server

