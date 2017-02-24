module Lib
    ( someFunc, startMonitoring, insertPoint, Entry(CephEntry), test
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Maybe

data Entry = CephEntry {riops :: Int, wiops :: Int}

line = "2017-02-24 15:38:01.120369 mon.0 192.168.1.235:6789/0 1023653 : cluster [INF] pgmap v16853855: 832 pgs: 832 active+clean; 780 GB data, 1616 GB used, 9556 GB / 11172 GB avail; 0 B/s rd, 1496 kB/s wr, 353 op/s"

someFunc :: IO ()
someFunc = putStrLn "someFunc"

parseLine :: String -> Entry
parseLine theLine = do
    let pgpos = B.findSubstring (C.pack "pgmap") (C.pack theLine)
    CephEntry 0 (fromMaybe 0 pgpos)

test = do
    let theEntry = parseLine line
    insertPoint theEntry

insertPoint :: Entry -> IO ()
insertPoint (CephEntry r w) = do
    putStrLn "Inserting"
    print r
    print w

startMonitoring :: IO ()
startMonitoring = do
    putStrLn "Starting cepher"
