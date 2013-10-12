module Yesod.S3 where

import Data.ByteString.Lazy
import Yesod.Core.Types
import Data.Conduit
import Data.Conduit.Binary
import Network.URI
import Network.AWS.AWSConnection
import Network.AWS.AWSResult
import Network.AWS.S3Object
import qualified Data.Text as T

extractFile :: FileInfo -> IO ByteString
extractFile f = runResourceT $ fileSourceRaw f $$ sinkLbs

uploadFile :: AWSConnection
           -> FileInfo
           -> String -- ^ The Bucket Name
           -> String -- ^ The Object Name
           -> IO (AWSResult ())
uploadFile conn fi bucket name = do
  bs <- extractFile fi
  let obj = S3Object bucket name (T.unpack $ fileContentType fi) [] bs
  sendObjectMIC conn obj

getLink :: AWSConnection
        -> String -- ^ The Bucket Name
        -> String -- ^ The Object Name
        -> IO URI -- ^ IO is only used to get the current Time
getLink conn bucket name = do
  let obj = S3Object bucket name "" [] empty
  publicUriForSeconds conn obj 60
