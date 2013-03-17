module HN.Curl where

import Network.Curl

-- | Download a string from a URI.
downloadString :: String -> IO (Either (CurlCode,String) String)
downloadString uri = do
  withCurlDo $ do

    (code,resp) <- curlGetString_ uri opts

    case code of
      CurlOK -> return (Right resp)
      _ -> return (Left (code,resp))

  -- Some silly servers think they're super smart by disallowing the
  -- "Curl" user-agent. Aw. ^_^
  where opts = [CurlUserAgent "Chrome"]
