module Lib
    ( someFunc, startMonitoring, insertPoint, Entry(CephEntry), test
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Maybe
import Data.List.Split
import Data.Char

data Entry = CephEntry {pgstotal, pgactive,used, available, rbs, wbs, ops :: Int}

line = "2017-02-24 15:38:01.120369 mon.0 192.168.1.235:6789/0 1023653 : cluster [INF] pgmap v16853855: 832 pgs: 832 active+clean; 780 GB data, 1616 GB used, 9556 GB / 11172 GB avail; 0 B/s rd, 1496 kB/s wr, 353 op/s"

someFunc :: IO ()
someFunc = putStrLn "someFunc"

splitExclusive :: String -> String -> (String, String)
splitExclusive on input = do
    let (left, right) = B.breakSubstring (C.pack on) (C.pack input)
    let rout = drop (length on) (C.unpack right)
    (C.unpack left, rout)

parseUnit :: String -> Int
parseUnit input = do
    let (num, unit) = splitExclusive " " input
    let fnum = read num :: Int
    case toLower (head unit) of
        'k' -> fnum * 1000
        'm' -> fnum * 1000000
        'g' -> fnum * 1000000000
        't' -> fnum * 1000000000000
        'p' -> fnum * 1000000000000000
        _ -> fnum


parseLine :: String -> Entry
parseLine theLine = do
    let (_, datas) = splitExclusive "pgmap " theLine
    let parts = splitOn "; " datas
    let (pgstid, pgsrest) = splitExclusive " pgs: " (head parts)
    let (_, pgstotal) = splitExclusive " " pgstid
    let (pgsactive, _) = splitExclusive " " pgsrest
    let (drest, dtotal) = splitExclusive " / " (parts!!1)
    let davail = last (splitOn ", " drest)
    let [dr, dw, iops] = splitOn ", " (parts!!2)
    CephEntry (read pgstotal :: Int) (read pgsactive :: Int) (parseUnit davail) (parseUnit dtotal) (parseUnit dr) (parseUnit dw) (parseUnit iops)

test = do
    let theEntry = parseLine line
    insertPoint theEntry

insertPoint :: Entry -> IO ()
insertPoint (CephEntry pt pa da dt dr dw iops) = do
    putStrLn "Inserting"
    print pt
    print pa
    print da
    print dt
    print dr
    print dw
    print iops

startMonitoring :: IO ()
startMonitoring = do
    putStrLn "Starting cepher"
