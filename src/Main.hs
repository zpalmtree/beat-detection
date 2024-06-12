{-# LANGUAGE BangPatterns #-}

module Main
(
    main
)
where

import System.Environment (getArgs)
import Data.List (genericLength)
import Control.Concurrent (threadDelay, forkIO)
import Sound.Sox.Play (simple)
import Sound.Sox.Frame (C)
import Sound.Sox.Option.Format (numberOfChannels)
import Sound.File.Sndfile (Info, samplerate, channels, duration)
import Foreign.Storable (Storable)
import Sound.File.Sndfile.Buffer.StorableVector (fromBuffer)
import qualified Sound.File.Sndfile.Buffer as Snd (readFile)
import qualified Data.StorableVector as V (Vector, unpack)

import qualified Data.StorableVector.Lazy as LazyV 
    (Vector, pack, defaultChunkSize, splitAt, map, foldr, take, null, empty,
     length, cons, hPut)

type Buf = LazyV.Vector

main :: IO ()
main = do
    args <- getArgs

    let input | null args = "tamborine.ogg"
              | otherwise = head args

    result <- Snd.readFile input
    case result of
        Left err -> putStrLn $ "Error reading file: " ++ err
        Right (info, Just test) -> do
            let vector = toLazy $ fromBuffer test :: Buf Double
                (initialBuf, rest) = takeHistoryBuf vector
                (initialSample, initialSong) = takeSample rest
                initialLocalEnergyBuf = mkInitialBuf initialBuf
                beatArray = calcBeatLoop initialSong initialLocalEnergyBuf initialSample
                delay = round $ 1e6 * duration info / genericLength beatArray
            forkIO $ printBeats beatArray 1 delay
            play rest info (fromIntegral delay)
        Right (_, Nothing) -> putStrLn "No samples found in the file."


toLazy :: (Storable a) => V.Vector a -> Buf a
toLazy xs = LazyV.pack LazyV.defaultChunkSize $ V.unpack xs

takeSample :: (Storable a) => Buf a -> (Buf a, Buf a)
takeSample = LazyV.splitAt (round windowLen)

takeHistoryBuf :: (Storable a) => Buf a -> (Buf a, Buf a)
takeHistoryBuf = LazyV.splitAt (round memoryLen)

windowLen :: (Fractional a) => a
windowLen = 2048.0

memoryLen :: (Fractional a) => a
memoryLen = 88200.0

instantEnergy :: (Storable a, Fractional a) => Buf a -> a
instantEnergy = vectorSum . LazyV.map (^^2)

vectorSum :: (Storable a, Fractional a) => Buf a -> a
vectorSum = LazyV.foldr (+) 0

calcBeatLoop :: (Fractional a, Ord a, Storable a, Floating a) => 
                 Buf a -> Buf a -> Buf a -> [Bool]
calcBeatLoop wholeSong localEnergyBuf sample
    | LazyV.null wholeSong = []
    | instantE > energyThreshold = True : loop
    | otherwise = False : loop
    where instantE = instantEnergy sample
          averageE = localAvgEnergy localEnergyBuf
          varianceE = energyVariance localEnergyBuf averageE
          dynamicEnergyConstant = (-0.0025714 * varianceE) + 1.5142857
          energyThreshold = dynamicEnergyConstant * averageE
          newLocalEnergyBuf = instantE `LazyV.cons` vectorInit localEnergyBuf
          (newSample, newAll) = takeSample wholeSong
          loop = calcBeatLoop newAll newLocalEnergyBuf newSample

localAvgEnergy :: (Storable a, Fractional a, Floating a) => Buf a -> a
localAvgEnergy buf = sqrt $ divisor * vectorSum (LazyV.map (^^2) buf)
    where divisor = windowLen / memoryLen

energyVariance :: (Storable a, Fractional a, Floating a) => Buf a -> a -> a
energyVariance buf avg = sqrt $ divisor * vectorSum (LazyV.map var buf)
    where divisor = windowLen / memoryLen
          var x = (x-avg)^^2

vectorInit :: (Storable a) => Buf a -> Buf a
vectorInit xs = LazyV.take (len - 1) xs
    where len = LazyV.length xs

mkInitialBuf :: (Fractional a, Storable a) => Buf a -> Buf a
mkInitialBuf xs
    | LazyV.null xs = LazyV.empty
    | otherwise = instantEnergy sample `LazyV.cons` mkInitialBuf rest
    where (sample, rest) = takeSample xs

--evaluate delay so both the bang thread and the play thread start at once
play :: (Storable a, C a, Show a) => Buf a -> Info -> a -> IO ()
play file info n = do
    let !_ = n
    simple LazyV.hPut (numberOfChannels numChannels) 
                      (samplerate info * numChannels) file

    return ()
    where numChannels = channels info

printBeats :: [Bool] -> Integer -> Int -> IO ()
printBeats [] _ _ = putStrLn "done"
printBeats (True:xs) n delay = do
    putStrLn $ "BANG! " ++ show n
    threadDelay delay
    printBeats xs (n+1) delay

printBeats (_:xs) n delay = do
    threadDelay delay
    printBeats xs n delay
