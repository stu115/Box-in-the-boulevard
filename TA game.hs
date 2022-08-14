-- =================================
-- Box in the boulevard text adventure
-- =================================


data Entrance = Entrance String String -- two strings for two pieces of info displayed separately


data Place = Place String String [Entrance]

data playBuilding = playBuilding [Place]

--  study - toilet - sitting room - acting room-dance hall 
--   |     /    |    /         \    /      |     /
--  library - dining area - gaming room - piano room



thisTyping :: String -> Place -> playBuilding -> IO ()
thisTyping pC cR pB= do
    putStr ("Please decide what you want to do " ++ pN ++)
    typing <- getLine
   


    if (command == "come out" || command == "leave game") then
        putStrLn "You are now leaving..."
    else if (command == "see") then do
        putStrLn (bringRoomInfo cR)
        solicitCommand pN cR pB 
    else if (isDirectionInRoom command cR) then do
        let nR = getRoomFromGameWorld (bringPlaceThroughEntrance (getDoorwayFromRoom command cR)) pB
        putStrLn (bringRoomInfo nR)
        solicitCommand pN nR pB
    else do
        putStrLn ("This " ++ command ++ " you have typed is not recognized. Try another")
        solicitCommand pN cR pB


bringPlaceInfo :: Room -> String
bringPlaceInfo (Room liw linfo le) = "\n  " ++ linfo ++ "\n  " ++ "This place has " ++ (show (length le)) ++ " ways to leave: " ++ (printDirectionNames le) ++ "\n"


bringEntranceInPlace :: String -> Room -> Doorway
bringEntranceInPlace dir (Room liw lway xdws) = head [(Doorway xstr xrm) | (Doorway xstr xrm) <- xdws, dir == xstr]


bringEntrancesInPlace :: Room -> [Doorway]
bringEntrancesInPlace (Room liw lway xdws) = xdws


bringWayInEntrance :: Doorway -> String
bringWayInEntrance(Doorway lway xroom) = lway


showTheWays :: [Doorway] -> String
showTheWays [] = ""
showTheWays (x:xs) = (bringWayInEntrance x) ++ " " ++ (showWays xs)


theWayInPlace :: String -> Room -> Bool
theWayInPlace dir (Room liw lway xdws) = elem dir [bringWayInEntrance xdw | xdw <- xdws]


bringPlaceThroughEntrance :: Doorway -> String
bringPlaceThroughEntrance (Doorway lway xroom) = xroom


bringInfoOfPlace :: Room -> String
bringInfoOfPlace (Room id descr doorws) = descr

placeInPlayBuilding :: String -> GameWorld -> Room
placeInPlayBuilding liw (GameWorld xgws) = head [(Room rid rdesc rdw) | (Room rid rdesc rdw) <- xgws, rid == liw]


start :: IO ()
start = do
-- make the building game is played in--   

    let study = Place "study" "You are in the study. It has chairs, desks, computers and books." [(Entrance "south" "library"  ),(Entrance "east" "toilet")]
    let library = Place "library" "This is the library. There are shelves, full of books, computers, telephone and tables to sit at. Maybe the box of vinyl lps are here?" [(Entrance "north" "study"), (Entrance "north-east" "toilet"),(Entrance "east" "dining area")]
    let toilet = Place "toilet" "The box of lps can't be in here." [(Entrance "west" "study"),(Entrance "south-west" "library"),(Entrance "south" "dining area"),(Entrance "east" "sitting room")]
    let dining area = Place "dining area" "There is a candle holder on the dining table, a cabinet, pictures and a sideboard." [(Entrance "west" "library"),(Entrance "north" "toilet"), (Entrance "north-east" "sitting room")]
	let sitting room = Place "sitting room" "For sitting, relaxing, deciding or waiting." [(Entrance "south-west" "dining area"),(Entrance "west" "toilet"),(Entrance "south-east" "gaming room"),(Entrance "east" "acting room")]
	let gaming room = Place "gaming room" "Play all sorts of games here. You have found the lps." [(Entrance "west" "dining area"),(Entrance "north-west" "sitting room"),(Entrance "north-east" "acting room"),(Entrance "east" "piano room")]
	let acting room = Place "acting room" "Acting lessons take place here before the live performance." [(Entrance "west" "sitting room"),(Entrance "south-west" "gaming room"),(Entrance "south" "piano room"),(Entrance "east" "dance hall")]
	let piano room = Place "piano room" "You can chill out here to music being played on the piano. Is the box of lps in here?" [(Entrance "west" "gaming room"),(Entrance "north" "acting room"),(Entrance "north-east" "dance hall")]
	let dance hall = Place "dance hall" "The dancing takes place here if it is a wedding, dance class, competition or party." [(Entrance "west" "acting room"),(Entrance "south-west" "piano room")]
	
    let building = playBuilding [study, library, toilet, dining area, sitting room, gaming room, acting room, piano room, dance hall]

    -- start of game where player types in --
    putStrLn "Please type in the name you would like to be called: "	 
    playingCharacter <- getLine
    putStrLn ("Hi" ++ playingCharacter ++ ". Welcome to this boulevard. You have today to find the box of LPs to be sold in the record shop you work in. Are you ready?")

    let startingArea = study
    
    solicitCommand playerName startingArea building
