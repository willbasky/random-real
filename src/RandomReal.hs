{-# LANGUAGE TypeApplications #-}


module RandomReal
  (
    makeRandom
  ) where

import           Codec.ByteString.Builder
import           Codec.Wav
import           Control.Concurrent             (threadDelay)
import qualified Control.Concurrent.Async       as A
import           Control.Concurrent.STM.TBQueue (TBQueue, lengthTBQueue,
                                                 newTBQueueIO, readTBQueue, writeTBQueue, isFullTBQueue)
import           Control.Exception              (bracket_)
import           Control.Monad                  (forM_, forever)
import           Control.Monad.Extra            (whenM)
import           Control.Monad.STM
import           Crypto.Hash
import qualified Data.Array.IArray              as IA
import           Data.Audio                     (Audio, SampleData, sampleData)
import qualified Data.ByteString                as B
import qualified Data.Foldable                  as F
import           Data.Int                       (Int64)
import qualified Data.List                      as L
import           Data.Text                      (Text)
import qualified Data.UUID                      as UUID (toString)
import qualified Data.UUID.V4                   as UUID (nextRandom)
import           Data.Word                      (Word8)
import           System.CPU
import qualified Data.Binary as Bin
import           System.Directory               (createDirectoryIfMissing,
                                                 doesFileExist,
                                                 removeDirectoryRecursive,
                                                 removeFile)
import           System.Exit                    (ExitCode (..))
import           System.Process                 (ProcessHandle,
                                                 getProcessExitCode, runCommand)
import qualified System.Random                  as R

-- TODO add die on count
checkRecord :: ProcessHandle -> IO ExitCode
checkRecord hand = do
  mCode <- getProcessExitCode hand
  case mCode of
    Just code -> pure code
    Nothing -> do
      threadDelay 20000
      checkRecord hand

genName :: IO String
genName = UUID.toString <$> UUID.nextRandom

tmpDir :: FilePath
tmpDir = "tmp"

recordPaths :: TBQueue FilePath -> IO ()
recordPaths tbq = bracket_ (createDirectoryIfMissing False "tmp") (removeDirectoryRecursive "tmp") $ do
  threadDelay 1000000
  -- print 0
  forever $ do
    -- print 1
    name <- genName
    -- dur :: Int <- R.randomRIO (1,3)
    let path = tmpDir <> "/" <> "test-mic-" <> name <> ".wav"
    ph <- runCommand $
      "arecord -f S32_LE -d 1 -r 44100 -c 2 -q " <> path
    code <- checkRecord ph
    -- print code
    case code of
      ExitFailure n -> error $ "Record exit failure with code: " <> show n
      ExitSuccess -> do
        -- (atomically $ isFullTBQueue tbq) >>= print
        atomically $ writeTBQueue tbq path


loadAudio :: TBQueue FilePath -> IO (SampleData Int64)
loadAudio tbq = do
  path <- atomically $ readTBQueue tbq
  wav :: Either String (Audio Int64) <- importFile path
  case wav of
    Left err -> error $ "Import error: " <> err
    Right resp -> do
      print path
      whenM (doesFileExist path) $ removeFile path
      -- putStrLn "Success!"
      -- putStrLn $ "sampleRate: " <> show (sampleRate resp)
      -- putStrLn $ "channelNumber: " <> show (channelNumber resp)
      pure $ sampleData resp
      -- let (low, high) = IA.bounds sd


makeRandom :: IO ()
makeRandom = do
  tbq <- newTBQueueIO 10
  A.withAsync (recordPaths tbq) $ \_ -> do
  -- putStrLn "tbq"
  -- A.withAsync (A.replicateConcurrently_ 10 $ threadDelay 1000000 >> recordPaths tbq) $ \ _ -> do
      -- putStrLn "Prepare data..." >> threadDelay 5000000
      print 10
      forever $ do
        print 11
        numLen <- atomically $ lengthTBQueue tbq
        putStrLn $ "queque length: " <> show numLen
        forM_ [(1 :: Int)..50] $ \_ -> do
          sd :: SampleData Int64 <- loadAudio tbq
          -- putStrLn "load samples"
          -- print $ Bin.encode sd
          print $ hashlazy @SHA256 $ Bin.encode sd
          -- let (low, high) = IA.bounds sd
          -- print $ IA.elems sd
          -- print low
          -- print high
          -- prn1 <- R.randomIO
          -- putStr "prn1: " >> print prn1
          -- prn2 <- R.randomIO
          -- putStr "prn2: " >> print prn2
          -- let pureGen1 = R.mkStdGen prn1
          -- putStr "pureGen1: " >> print pureGen1
          -- let pureGen2 = R.mkStdGen prn2
          -- putStr "pureGen2: " >> print pureGen2
          -- let (indexRandom1, _) = R.randomR (low, high) pureGen1
          -- let (indexRandom2, _) = R.randomR (low, high) pureGen2
          -- let val1 = sd IA.! indexRandom1
          -- let val2 = sd IA.! indexRandom2
          -- let randomNum1 = abs $ round $ (fromIntegral val1) * (fromIntegral indexRandom1)
          -- putStr "randomNum1: " >> print randomNum1
          -- let randomNum2 = abs $ round $ (fromIntegral val2) / (fromIntegral indexRandom2)
          -- putStr "randomNum2: " >> print randomNum2
          -- if randomNum1 == 0 || randomNum2 == 0 then pure () else do
          --   let minNum = min randomNum1 randomNum2
          --   let maxNum = max randomNum1 randomNum2
          --   prn3 <- R.randomIO
            -- putStr "prn3: " >> print prn3
            -- let pureGen3 = R.mkStdGen prn3
            -- let (pseudoRandomNum1 :: Integer, _) = R.randomR (minNum, maxNum) pureGen3
            -- prn4 <- R.randomIO
            -- let pureGen4 = R.mkStdGen prn4
            -- let (pseudoRandomNum2 :: Integer, _) = R.randomR (0, pseudoRandomNum1) pureGen4
            -- putStrLn $ "random data1: " <> show pseudoRandomNum1
            -- putStrLn $ "random data2: " <> show pseudoRandomNum2
          -- threadDelay 100000

      -- let b = buildWav resp
      -- let bsl = toLazyByteString b
      -- print bsl


mul (x,y) (acc, summa) = if m /= 0 then (m : acc, summa + 1) else (acc, summa)
  where
    m = (fromIntegral x) * (fromIntegral y) :: Integer

