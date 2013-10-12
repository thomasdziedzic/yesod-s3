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

extractFile :: FileInfo -> IO ByteString
extractFile f = runResourceT $ fileSourceRaw f $$ sinkLbs

uploadImage :: AWSConnection
            -> FileInfo
            -> String -- ^ The Bucket Name
            -> String -- ^ The Base Name
            -> [((Int,Int), Int, String)] -- ^ Styles for resizing, the ints are upper borders
            -> IO [(String, String)] -- ^ Style and Object name
uploadImage conn fi bucket name styles = do
  bs <- extractFile fi
  let ft = T.unpack $ fileContentType fi
  img <- case ft of
    "image/png" -> loadPngByteString bs
    "image/jpg" -> loadJpegByteString bs
    "image/gif" -> loadGifByteString bs
  (x,y) <- imageSize img
  flip mapM styles $ \((x',y'),qual,style) -> do
    let xscale = x' % x
        yscale = y' % y
        (x'',y'') = if xscale<yscale
                      then (x', floor $ fromIntegral y * xscale)
                      else (floor $ fromIntegral x * yscale, y')
    img' <- resizeImage x'' y'' img
    bs' <- saveJpegByteString qual img'
    let name' = name ++ "-" ++ style
        obj = S3Object bucket name' "image/jpg" [] bs'
    sendObjectMIC conn obj
    return (style, name')

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
        -> Integer -- ^ Seconds the Link should be valid
        -> IO URI -- ^ IO is only used to get the current Time
getLink conn bucket name t = do
  let obj = S3Object bucket name "" [] empty
  publicUriForSeconds conn obj t
