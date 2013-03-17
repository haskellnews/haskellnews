module HN.Curl where

import Network.Curl

-- | Download a string from a URI.
downloadString :: String -> IO (Either (CurlCode,String) String)
downloadString uri = do
  withCurlDo $ do
    (code,resp) <- curlGetString_ uri []
    case code of
      CurlOK -> return (Right resp)
      _ -> return (Left (code,resp))
