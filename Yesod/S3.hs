module Yesod.S3 (
  uploadImage,
  uploadFile,
  getLink) where

import Data.ByteString.Lazy
import Yesod.Core.Types
import Data.Conduit
import Data.Conduit.Binary
import Network.URI
import Network.AWS.AWSConnection
import Network.AWS.AWSResult
import Network.AWS.S3Object
import qualified Data.Text as T
import Graphics.GD.ByteString.Lazy
import Data.Ratio
import Control.Monad.Error

extractFile :: FileInfo -> IO ByteString
extractFile f = runResourceT $ fileSourceRaw f $$ sinkLbs

data UploadError
  = InvalidContentType
  | ReqError ReqError
  | StringError String

instance Error UploadError where
  strMsg = StringError

uploadImage :: AWSConnection
            -> FileInfo
            -> String -- ^ The Bucket Name
            -> String -- ^ The Base Name
            -> [((Int,Int), Int, String)] -- ^ Styles for resizing, the ints are upper borders
            -> ErrorT UploadError IO [(String, String)] -- ^ Style and Object name
uploadImage conn fi bucket name styles = do
  bs <- liftIO $ extractFile fi
  let ft = T.unpack $ fileContentType fi
  img <- case ft of
    "image/png" -> liftIO $ loadPngByteString bs
    "image/jpg" -> liftIO $ loadJpegByteString bs
    "image/gif" -> liftIO $ loadGifByteString bs
    _           -> throwError InvalidContentType
  (x,y) <- liftIO $ imageSize img
  flip mapM styles $ \((x',y'),qual,style) -> do
    let xscale = x' % x
        yscale = y' % y
        (x'',y'') = if xscale<yscale
                      then (x', floor $ fromIntegral y * xscale)
                      else (floor $ fromIntegral x * yscale, y')
    img' <- liftIO $ resizeImage x'' y'' img
    bs' <- liftIO $ saveJpegByteString qual img'
    let name' = name ++ "-" ++ style
        obj = S3Object bucket name' "image/jpg" [] bs'
    res <- liftIO $ sendObjectMIC conn obj
    case res of
      Right () -> return (style, name')
      Left err -> throwError $ ReqError err

uploadFile :: AWSConnection
           -> FileInfo
           -> String -- ^ The Bucket Name
           -> String -- ^ The Object Name
           -> ErrorT UploadError IO ()
uploadFile conn fi bucket name = do
  bs <- liftIO $ extractFile fi
  let obj = S3Object bucket name (T.unpack $ fileContentType fi) [] bs
  res <- liftIO $ sendObjectMIC conn obj
  case res of
    Right () -> return ()
    Left err -> throwError $ ReqError err

getLink :: AWSConnection
        -> String -- ^ The Bucket Name
        -> String -- ^ The Object Name
        -> Integer -- ^ Seconds the Link should be valid
        -> IO URI -- ^ IO is only used to get the current Time
getLink conn bucket name t = do
  let obj = S3Object bucket name "" [] empty
  publicUriForSeconds conn obj t
