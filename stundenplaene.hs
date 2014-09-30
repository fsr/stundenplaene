{-# LANGUAGE QuasiQuotes #-}

-- CAUTION: this parser will discard the second row in a weekday,
-- so make sure your document does not store any information
-- exclusively in this second pointless row!

-- Also, the current version requires fixing the MINF03 plan (colspan='4').

-- Oh and it assumes there are exactly 4 INF-B plans, see the yay-hardcoding-comment below.

import Text.Blaze.Renderer.String -- TODO: choose better instance, UTF8 and stuff...
import Text.Hamlet
import Text.HTML.TagSoup
import Text.Parsec
import Text.XML.Light
import Control.Applicative ((*>), (<*))
import Control.Arrow (second)
import Control.Monad (when, void, liftM)
import Data.Char (isSpace)
import Data.List (transpose, isInfixOf)
import Data.List.Split (chunksOf, splitOn)
import Data.Maybe (fromJust, catMaybes)
import Debug.Trace (trace)

data Event = Event { sname    :: String
                   , stype    :: String
                   , sweekday :: String
                   , sstart   :: String
                   , send     :: String
                   , sroom    :: String
                   , steacher :: String
                   }
             deriving (Eq, Show, Read)
type Slot = Maybe Event
newtype Day = Day [Slot] deriving (Show, Read)
newtype Timeslot = Timeslot [Slot]
type TimeState = (Int, Int) -- Day and timeslot number, zero-indexed

germanweekdays = ["Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag"]
starttimes = ["7:30", "9:20",  "11:10", "13:00", "14:50", "16:40", "18:30"]
endtimes   = ["9:00", "10:50", "12:40", "14:30", "16:20", "18:10", "20:00"]
timespans = zipWith (\a b -> a++" - "++b) starttimes endtimes

main = do
       allPlans <- liftM parseInputHtml $ readFile "input.htm"
       let htmlout = concatMap (getPlanAsHtml . second shortenNames) allPlans
       writeFile "output.htm" htmlout
       -- And now for something completely different.
       anotherPlan <- liftM (parseInputBinary . lines) $ readFile "another.txt"
       let anotherout = concatMap getPlanAsHtml $ map (uncurry overlayPlans) $ zip allPlans (repeat anotherPlan)
       writeFile "overlay.htm" anotherout

parseInputBinary :: [[Char]] -> [Day]
parseInputBinary = map (Day . parseDay) . transpose
    where
        parseDay :: [Char] -> [Slot]
        parseDay = map parseSlot . zip [0..]
        parseSlot :: (Int, Char) -> Slot
        parseSlot (n, '#') = Just $ Event "" "" "" (starttimes !! n) (endtimes !! n) "" ""
        parseSlot (_, ' ') = Nothing

getPlanAsHtml :: (String, [Day]) -> String
getPlanAsHtml (pname, p) = renderMarkup $ -- TODO: Umlauts -> Entities
    [shamlet|
        <h2>#{prettifyPlanName pname}
        <table border=2>
           <tr>
               <td .plan_leer>#{pname}
               $forall day <- germanweekdays
                   <td .plan_tage colspan=2>#{day}
           $forall (nr, Timeslot slots) <- slotlist
               <tr>
                   <td .plan_stunden> #{nr}. DS
                   $forall slot <- slots
                       <td .plan_name colspan=2>
                           $maybe e <- slot
                               #{sname e}
                               #{stype e}
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
    where
        slotlist = zip [1..] $ transpose' p
        -- transpose' because tables are written horizontally
        transpose' :: [Day] -> [Timeslot]
        transpose' = map Timeslot . transpose . map (\(Day d) -> d)
        prettifyPlanName :: String -> String
        prettifyPlanName ('M':'I':'N':'F':'-':'B':'/':cs) = "INF-B/" ++ (zeroPrint . (+4) . read $ cs) -- TODO: yay hardcoding
        prettifyPlanName n = n
        zeroPrint :: Int -> String
        zeroPrint n | n < 10 = '0' : (show n) | True = show n

parseInputHtml :: String -> [(String, [Day])]
parseInputHtml = map parseTable
               . chunksOf 2 -- each plan contains two tables
               . findElements (QName "table" (Just "http://www.w3.org/1999/xhtml") Nothing)
               . fromJust
               . parseXMLDoc
    where
        parseTable :: [Element] -> (String, [Day])
        parseTable [headtable, contenttable] = ( (\x -> trace x x) $ getPlanName headtable
                                               , getDayList $ parseTags . showElement $ contenttable) -- out of one HTML library into the next. This can't be right. TODO maybe?
        getPlanName = getCaption
                    . head
                    . elContent
                    . firstChild
                    . lastChild
                    . lastChild
                    . lastChild
                    . lastChild
        getDayList ts = let usefultags = filter (\t -> if isTagText t
                                                       then not . all isSpace $ fromTagText t
                                                       else True) ts in
                        case runParser tableParser (-1,undefined) "plans" usefultags of
                            Left err -> error $ show err
                            Right dl -> dl
        getCaption (Text (CData _ s _)) = drop 3 . last $ splitOn " " s
        firstChild = head . elChildren
        lastChild = last . elChildren
        -- TagSoup Parsec functions
        tsPred :: (Tag String -> Bool) -> Parsec [Tag String] u (Tag String)
        tsPred pred = tokenPrim (show) (\pos x xs -> incSourceColumn pos 1) (\x -> if pred x then Just x else Nothing)
        tsExactTag :: (Tag String) -> Parsec [Tag String] u (Tag String)
        tsExactTag t = tsPred (\x -> t == x)
        tsTag :: (Tag String) -> Parsec [Tag String] u (Tag String)
        tsTag t = tsPred (~== t)
        tsInsideTag :: String -> Parsec [Tag String] u a -> Parsec [Tag String] u a
        tsInsideTag name = between (tsTag $ TagOpen name []) (tsTag $ TagClose name)
        tsNoClosing :: String -> Parsec [Tag String] u (Tag String)
        tsNoClosing s = tsPred (\t -> case t of TagClose n -> n /= s; _ -> True)
        tsOpenClose :: String -> Parsec [Tag String] u ()
        tsOpenClose s = tsTag (TagOpen s []) >> tsTag (TagClose s) >> return ()
        -- The actual parsing
        tableParser :: Parsec [Tag String] TimeState [Day]
        tableParser = do
                      tsTag (TagOpen "table" [])
                      many $ tsNoClosing "tr"
                      tsTag (TagClose "tr")
                      count 5 $ dayParser
        dayParser :: Parsec [Tag String] TimeState Day
        dayParser = do
                    modifyState (\(daynr,_) -> (daynr + 1, 0))
                    tsTag (TagOpen "tr" [])
                    (TagOpen _ weekdayAttribs) <- tsTag (TagOpen "td" []) <* anyToken <* tsTag (TagClose "td")
                    slots <- count 7 $ slotParser
                    tsTag (TagClose "tr")
                    when (lookup "rowspan" weekdayAttribs == Just "2") $ void $ tsInsideTag "tr" $ count 7 $ slotParser
                    return $ Day slots
        slotParser :: Parsec [Tag String] TimeState Slot
        slotParser = (try filledSlot <|> emptySlot) <* (modifyState $ second (+1))
        filledSlot = do
                     tsTag (TagOpen "td" [("colspan","2")])
                     (teacher,_) <- tsInsideTag "table" $ objectCellContents
                     (name, eventtype) <- tsInsideTag "table" $ objectCellContents
                     (time, place) <- tsInsideTag "table" $ objectCellContents
                     tsTag (TagClose "td")
                     (daynr, slotnr) <- getState
                     return $ Just $ Event name
                                           eventtype
                                           (germanweekdays !! daynr)
                                           (starttimes !! slotnr)
                                           (endtimes !! slotnr)
                                           place
                                           teacher
        objectCellContents :: Parsec [Tag String] TimeState (String, String)
        objectCellContents = do
                             tsOpenClose "col"
                             optional $ tsOpenClose "col"
                             tsTag (TagOpen "tr" [])
                             let ex = tsInsideTag "td" $ tsInsideTag "font" $ option (TagText "") $ tsTag (TagText "")
                             TagText s1 <- ex
                             TagText s2 <- option (TagText "") $ ex
                             tsTag (TagClose "tr")
                             many $ tsNoClosing "table"
                             return (s1, s2)
        emptySlot = do
                    tsOpenClose "td"
                    tsOpenClose "td"
                    return Nothing
        piep x = getState >>= (\s -> trace (show s) x)

shortenNames :: [Day] -> [Day]
shortenNames = map (\(Day sl) -> Day $ map go sl)
    where
        go Nothing = Nothing
        go (Just (Event    name eventtype weekday start end room    teacher)) =
           (Just (Event newname eventtype weekday start end room newteacher))
            where newname = fromJust $ lookup name prettyNames
                  newteacher = if "N.N" `isInfixOf` teacher then "" else teacher
        prettyNames = [ ("Mathematik 1 für Informatiker: Diskrete Strukturen & Lineare Algebra", "DIS+LAG")
                      , ("Einführung in die Medieninformatik", "EMI")
                      , ("Algorithmen und Datenstrukturen", "AuD")
                      , ("Technische Grundlagen der Informatik", "TGI")
                      , ("Rechnerarchitektur I", "RA")
                      ]

overlayPlans :: (String, [Day]) -> [Day] -> (String, [Day])
overlayPlans (name, p1) p2 = (name, map overlayDays $ zip p1 p2)
    where
        overlayDays (Day d1, Day d2) = Day $ map overlaySlots $ zip d1 d2
        overlaySlots (Nothing, Nothing) = Nothing
        overlaySlots (Just e, Nothing) = Just $ e {sname = "/////////ERSTI\\\\\\\\\\\\\\\\", stype = "", sroom = "//////////", steacher = "\\\\\\\\\\\\\\\\\\\\\\\\"}
        overlaySlots (Nothing, Just e) = Just $ e {sname = "//////////////\\\\\\\\\\\\\\\\\\\\\\", stype = "", sroom = "//////////", steacher = "\\\\\\\\\\\\\\\\\\\\\\\\"}
        overlaySlots (Just e, Just _)  = Just $ e {sname = "/////////ERSTI\\\\\\\\\\\\\\\\", stype = "", sroom = "//////////", steacher = "\\\\\\\\\\\\\\\\\\\\\\\\"}