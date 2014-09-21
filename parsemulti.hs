import Text.XML.Light
import Data.List (transpose)
import Data.List.Split (chunksOf, splitOn)
import Data.Maybe (fromJust, catMaybes)
import Debug.Trace (traceShow, trace)

data Event = Event { sname    :: String
                   , sweekday :: String
                   , sstart   :: String
                   , send     :: String
                   , sroom    :: String
                   , steacher :: String
                   }
             deriving (Eq, Show)
type Slot = Maybe Event -- deriving Show
type Day = [Slot] -- deriving Show
type Timeslot = [Slot] -- deriving Show
-- TODO [Slot] ambigous -> newtype
--(String, [Day])
germanweekdays = ["Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag"]
starttimes = ["7:30", "9:20",  "11:10", "13:00", "14:50", "16:40", "18:30"]
endtimes   = ["9:00", "10:50", "12:40", "14:30", "16:20", "18:10", "20:00"]
timespans = zipWith (\a b -> a++" - "++b) starttimes endtimes

main = 
--readFile "/home/sjm/programming/stundenplaene/input.htm" >>= writeFile "output.htm" . concatMap ppContent . concatMap getPlanAsTable . (:[]) . parseInputText
      readFile "input.htm"  >>= writeFile "output.htm" .  concatMap ppContent . concatMap getPlanAsTable . (\o -> trace (show  o) o) . parseInputHtml
--  . parseInputHtml
getPlanAsTable :: (String, [Day]) -> [Content]
getPlanAsTable (pname, p) =  map Elem [ unode "h2" pname
                                     ,  add_attr (Attr ( unqual "border") "2")
                                                (unode "table" $ weekdaysRow
                                                                 : ( concatMap getTimeslotRows
                                                                   $ zip [1..]
                                                                   $ transpose  p ))
                                                               -- transpose because tables are read horizontally
                                     , unode "br" ()
                                     , unode "br" ()
                                     ]
    where
        weekdaysRow :: Element
        weekdaysRow = unode "tr"
                    $ giveClass "plan_leer" (unode "td" pname)
                      : map ( add_attr (Attr (unqual "colspan") "2")
                            . giveClass "plan_tage"
                            . unode "td"
                            )
                          germanweekdays
        getTimeslotRows :: (Int, Timeslot) -> [Element]
        getTimeslotRows (nr, slots) = [ unode "tr"
                                      $ giveClass "plan_stunden" (unode "td" $ show nr ++ ". DS")
                                        : (map getSlotName slots)
                                      , unode "tr"
                                      $ giveClass "plan_uhrzeit" (unode "td" $   timespans !! (nr - 1))
                                        : (concatMap getSlotDetails slots)
                                      ]
        getSlotName :: Slot -> Element
        getSlotName slot = add_attr (Attr (unqual "colspan") "2")
                         . giveClass "plan_name"
                         . unode "td"
                         $ case slot of
                               Nothing -> ""
                               Just e  -> sname e
        getSlotDetails :: Slot -> [Element]
        getSlotDetails slot = [ giveClass "plan_dozent"
                              . unode "td"
                              $ case slot of
                                    Nothing -> ""
                                    Just e  -> steacher e
                              , giveClass "plan_raum"
                              . unode "td"
                              $ case slot of
                                    Nothing -> ""
                                    Just e  -> sroom e
                              ]
        giveClass :: String -> Element -> Element
        giveClass str = add_attr (Attr (unqual "class") str)

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
               $ headtable, 
           sorteventsintoplan
              . (\o -> trace (show   o) o)
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
                getName (Text (CData _ s _)) = s
                firstChild = head . elChildren
                lastChild = last . elChildren
                getEventsPerTimeslot :: Int -> [(Int, Element)] -> [Slot]
                getEventsPerTimeslot slotnr = map (getSlot slotnr)
                getSlot :: Int -> (Int, Element) -> Slot
                getSlot slotnr (daynr, e) = if (max slotnr daynr > 6) then error (show . head . tail $ elContent e) else case tail $ elContent e of
                                                 [] -> Nothing
                                                 (Elem table : _)  -> Just $ Event ( getName
                                                    . head
                                                        . elContent 
                                                        . head
                                                        . elChildren
                                                        . head 
                                                        . elChildren  
                                                        . head 
                                                        . tail
                                                        . tail 
                                                        . elChildren 
                                                        . head 
                                                        . tail
                                                        . elChildren 
                                                        $ e )
                                                       (germanweekdays !! daynr)
                                                       (starttimes !! slotnr)
                                                       (endtimes !! slotnr)
                                                       (getName
                                                            . head
                                                            . elContent 
                                                            . head 
                                                            . elChildren
                                                            . last 
                                                            . elChildren  
                                                            . last
                                                            . elChildren 
                                                            . last 
                                                            . elChildren 
                                                            $ e )
                                                        (getName
                                                            . head
                                                            . elContent 
                                                            . head
                                                            . elChildren
                                                            . head 
                                                            . elChildren  
                                                            . head 
                                                            . tail 
                                                            . elChildren 
                                                            . head 
                                                            . elChildren 
                                                            $ e )
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
