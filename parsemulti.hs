{-# LANGUAGE QuasiQuotes #-}

import Text.Hamlet
import Text.Blaze.Renderer.String -- TODO: choose better instance, UTF8 and stuff...
import Text.XML.Light
import Data.List (transpose)
import Data.List.Split (chunksOf, splitOn)
import Data.Maybe (fromJust, catMaybes)
import Debug.Trace (trace)

data Event = Event { sname    :: String
                   , sweekday :: String
                   , sstart   :: String
                   , send     :: String
                   , sroom    :: String
                   , steacher :: String
                   }
             deriving Eq
type Slot = Maybe Event
type Day = [Slot]
type Timeslot = [Slot]
-- TODO [Slot] ambigous -> newtype

germanweekdays = ["Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag"]
starttimes = ["7:30", "9:20",  "11:10", "13:00", "14:50", "16:40", "18:30"]
endtimes   = ["9:00", "10:50", "12:40", "14:30", "16:20", "18:10", "20:00"]
timespans = zipWith (\a b -> a++" - "++b) starttimes endtimes

main = readFile "/home/sjm/programming/stundenplaene/clean.txt" >>= writeFile "output.htm" . concatMap (getPlanAsHtml . parseInputText) . tail . splitOn "INF 1"
       -- readFile "/home/sjm/programming/stundenplaene/input.htm" >>= writeFile "/tmp/output.htm" . concatMap ppContent . concatMap getPlanAsTable . parseInputHtml

getPlanAsHtml :: (String, [Day]) -> String
getPlanAsHtml (pname, p) = renderMarkup
                         $ let slotlist = zip [1..] $ transpose p
                           in -- transpose because tables are read horizontally
    [shamlet|
        <h2>#{pname}
        <table border=2>
           <tr>
               <td .plan_leer>#{pname}
               $forall day <- germanweekdays
                   <td .plan_tage colspan=2>#{day}
           $forall (nr, slots) <- slotlist
               <tr>
                   <td .plan_stunden> #{nr}. DS
                   $forall slot <- slots
                       <td .plan_name colspan=2>
                           $maybe e <- slot
                               #{sname e}
               <tr>
                   <td .plan_uhrzeit> #{timespans !! (nr - 1)}
                   $forall slot <- slots
                       <td .plan_dozent>
                           $maybe e <- slot
                               #{steacher e}
                       <td .plan_raum>
                           $maybe e <- slot
                               #{sroom e}
        <br>
        <br>
    |]

parseInputHtml :: String -> [(String, [Day])]
parseInputHtml = (:[]) . parseTable . head
               . chunksOf 2 -- each plan contains two tables
               . findElements (QName "table" (Just "http://www.w3.org/1999/xhtml") Nothing)
               . fromJust
               . parseXMLDoc
    where
        parseTable :: [Element] -> (String, [Day])
        parseTable [headtable, contenttable] = ( getCaption
                                               . head
                                               . elContent
                                               . firstChild
                                               . lastChild
                                               . lastChild
                                               . lastChild
                                               . lastChild
                                               $ headtable, sorteventsintoplan
                                                          . (\o -> trace (show $ length o) o)
                                                          . catMaybes
                                                          . concatMap (uncurry getEventsPerTimeslot)
                                                          . zip [0..]
                                                          . map ( zip [0..]
                                                                . map head     -- drop every 
                                                                . chunksOf 2   -- second (useless), after we
                                                                . repeatEvents -- repeat proper 2-hour cells once
                                                                . tail
                                                                . elChildren)
                                                          . tail
                                                          . elChildren
                                                          $ contenttable )
            where
                getCaption (Text (CData _ s _)) = drop 3 . last $ splitOn " " s
                firstChild = head . elChildren
                lastChild = last . elChildren
                getEventsPerTimeslot :: Int -> [(Int, Element)] -> [Slot]
                getEventsPerTimeslot slotnr = map (getSlot slotnr)
                getSlot :: Int -> (Int, Element) -> Slot
                getSlot slotnr (daynr, e) = if (max slotnr daynr > 6) then error (show . head . tail $ elContent e) else case tail $ elContent e of
                                                 [] -> Nothing
                                                 (Elem table : _)  -> Just $ Event "Foo"
                                                                                   (germanweekdays !! daynr)
                                                                                   (starttimes !! slotnr)
                                                                                   (endtimes !! slotnr)
                                                                                   "y"
                                                                                   "x"
                                                 _ -> Nothing
                repeatEvents :: [Element] -> [Element]
                repeatEvents [] = []
                repeatEvents (x : xs) = if findAttr (QName "colspan" (Just "http://www.w3.org/1999/xhtml") Nothing) x == Nothing
                                        then x : repeatEvents xs
                                        else x : x : repeatEvents xs

parseInputText :: String -> (String, [Day])
parseInputText fullplan = (last . splitOn " " . head . lines $ fullplan, createplan . tail . lines $ fullplan)
    where
        createplan = sorteventsintoplan . map createeventfromstring
        createeventfromstring = createeventfromlist . splitOn "\t"
        createeventfromlist sl = if length sl < 7 then (error $ "Too short line: " ++ (show sl)) else
                                    Event (sl!!1) (sl!!2) (sl!!3) (sl!!4) (sl!!5) (niceteacher $ sl!!6)
        niceteacher = clearifunannounced . last . splitOn " "
        clearifunannounced name =  if length name < 2 then error $ "Too short name: " ++ name else
                                if name!!0 == 'N' && name!!1 == '.' && name!!2 == 'N' && name!!3 == '.' then "" else
                                if name!!0 == 'Z' && name!!1 == 'Z' then "" else name

sorteventsintoplan :: [Event] -> [Day]
sorteventsintoplan ellist = map (\w -> createday . filter ((==) w . sweekday) $ ellist) germanweekdays
    where createday els = map (\t -> if t `elem` (map sstart els) then Just (head . filter ((==) t . sstart) $ els) else Nothing) starttimes
