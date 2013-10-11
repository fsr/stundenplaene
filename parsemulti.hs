import Data.List (intercalate, transpose)
import Data.List.Split (splitOn)
replace old new = intercalate new . splitOn old

data Slot = Element {sname::String, sweekday::String, sstart::String, send::String, sroom::String, steacher::String} | Nil deriving Eq
type Day = [Slot]
type Time = [Slot]
type Plan = [Day]

germanweekdays = ["Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag"]
starttimes = ["7:30", "9:20", "11:10", "13:00", "14:50", "16:40", "18:30"]
endtimes = ["9:00", "10:50", "12:40", "14:30", "16:20", "18:10", "20:00"]

main = do
	readFile "studsets_ws1314.txt" >>= writeFile "clean.txt" . cleanup
	readFile "clean.txt" >>= writeFile "index.htm" . concat . map (getPlanAsTable . parseInputText) . tail . splitOn "INF 1"

getPlanAsTable :: (String, Plan) -> String
getPlanAsTable (pname,p) = "\n<h2>"++pname++"</h2>\n<table border='2'><tbody>\n" ++ weekdays
								++ (printtimeslotlist . transpose $ p) ++ "</tbody></table><br /><br />\n"
	where
		weekdays = "\t<tr>\n\t\t<td class='plan_leer'>"++pname++"</td>\n" ++
						(concat $ map (\s -> "\t\t<td colspan='2' class='plan_tage'>" ++ s ++ "</td>\n") germanweekdays) ++ "\t</tr>\n"
		printtimeslotlist = concat . map printtimeslot . zip [1..]
		printtimeslot (nr, ts) = "\t<tr>\n\t\t<td class='plan_stunden'>"++(show nr)++". DS</td>\n" ++ (concat . map printslotname $ ts)
								++ "\t</tr>\n\t<tr>\n\t\t<td class='plan_uhrzeit'>"++(timefor nr)++"</td>\n"
								++ (concat . map printslotdetails $ ts) ++ "\t\t</tr>\n"
		printslotname Nil = "\t\t<td colspan='2' class='plan_name'></td>\n"
		printslotname (Element name _ _ _ _ _) = "\t\t<td colspan='2' class='plan_name'>"++name++"</td>\n"
		printslotdetails Nil = "\t\t<td class='plan_dozent'></td>\n\t\t<td class='plan_raum'></td>\n"
		printslotdetails (Element _ _ _ _ room teacher) = "\t\t<td class='plan_dozent'>"++teacher++
															"</td>\n\t\t<td class='plan_raum'>"++room++"</td>\n"
		timefor i = nicetimespans!!(i-1)
		nicetimespans = zipWith (\a b -> a++" - "++b) starttimes endtimes

parseInputText :: String -> (String, Plan)
parseInputText fullplan = (last . splitOn " " . head . lines $ fullplan, createplan . tail . lines $ fullplan)
	where
		createplan = sorteventsintoplan . map createeventfromstring
		createeventfromstring = createeventfromlist . splitOn "\t"
		createeventfromlist sl = if length sl < 7 then (error $ "Too short line: " ++ (show sl)) else
									Element (sl!!1) (sl!!2) (sl!!3) (sl!!4) (sl!!5) (niceteacher $ sl!!6)
		niceteacher = clearifunannounced . last . splitOn " "
		clearifunannounced name =  if length name < 2 then error $ "Too short name: " ++ name else
								if name!!0 == 'N' && name!!1 == '.' && name!!2 == 'N' && name!!3 == '.' then "" else
								if name!!0 == 'Z' && name!!1 == 'Z' then "" else name

sorteventsintoplan :: [Slot] -> Plan
sorteventsintoplan ellist = map (\w -> createday . filter ((==) w . sweekday) $ ellist) germanweekdays
	where createday els = map (\t -> if t `elem` (map sstart els) then (head . filter ((==) t . sstart) $ els) else Nil) starttimes

-- I really hope we get a nice machine-readable file next time so stuff like this won't be necessary. Sigh.

cleanup :: String -> String
cleanup =     replace "65 INF" "65\tINF"
			. replace "40 INF" "40\tINF"
			. replace "\tV\tAlgorithmen und Datenstrukturen " "\tAuD VL\t"
			. replace "\tV\tAlgorithmen und Datenstrukturen\t" "\tAuD VL\t"
			. replace " U\tMathematik 1 für Informatiker: Lineare Algebra " "\tLAG Ü\t"
			. replace " U\tMathematik 1 für Informatiker: Diskrete Strukturen " "\tDIS Ü\t"
			. replace " U\tMathematik 1 für Informatiker: Lineare Algebra\t" "\tLAG Ü\t"
			. replace " U\tMathematik 1 für Informatiker: Diskrete Strukturen\t" "\tDIS Ü\t"
			. replace " U\tEinführung in die Medieninformatik " "\tEMI Ü\t"
			. replace " V\tEinführung in die Medieninformatik " "\tEMI VL\t"
			. replace " U\tAlgorithmen und Datenstrukturen " "\tAuD Ü\t"
			. replace " U\tMathematik 1 für Informatiker: Diskrete Strukturen & Lineare Algebra " "\tDIS+LAG Ü\t"
			. replace " V\tMathematik 1 für Informatiker: Diskrete Strukturen & Lineare Algebra " "\tDIS+LAG VL\t"
			. replace "\tV\tMathematik 1 für Informatiker: Diskrete Strukturen & Lineare Algebra " "\tDIS+LAG VL\t" 
			. replace " U\tEinführung in die Medieninformatik\t" "\tEMI Ü\t"
			. replace "\tU\tMathematik 1 für Informatiker: Diskrete Strukturen\t" "\tDIS Ü\t"
			. replace "\tU\tEinführung in die Medieninformatik " "\tEMI Ü\t"
			. replace " U\tTechnische Grundlagen der Informatik " "\tTGI Ü\t"
			. replace " V\tTechnische Grundlagen der Informatik " "\tTGI VL\t"
			. replace "\tV\tTechnische Grundlagen der Informatik " "\tTGI VL\t"
			. replace " U\tRechnerarchitektur I " "\tRA Ü\t"
			. replace "\tV\tRechnerarchitektur I " "\tRA VL\t"
			. replace "\tU\tAlgorithmen und Datenstrukturen " "\tAuD Ü\t"
			. replace " U\tMathematik 1 für Informatiker: Diskrete Strukturen & Lineare Algebra\t" "\tDIS+LAG Ü\t"
			. replace "\n\n" "\n" . replace "\n\n" "\n" . replace "\n\n" "\n"