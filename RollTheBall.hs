{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}
module RollTheBall where
import Pipes
import ProblemState

import qualified Data.Array as A
import Data.List
import Data.Maybe

{-
    Direcțiile în care se poate mișca o piesa pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc
-}

type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea celulelor tablei de joc
-}
--data Cell = Cell --TODO
data Cell = HorPipe | VerPipe | TopLeft | BotLeft | BotRight | TopRight
            | EmptySpace | EmptyCell
            | WinDown | WinLeft | WinRight | WinUp  
            | StartDown | StartLeft | StartRight | StartUp
            
        deriving (Eq, Ord)

instance Show Cell where
    show cell = case cell of
        HorPipe         -> [horPipe] 
        VerPipe         -> [verPipe]
        TopLeft         -> [topLeft] 
        BotLeft         -> [botLeft]
        BotRight        -> [botRight] 
        TopRight        -> [topRight]
        EmptySpace      -> [emptySpace]
        EmptyCell       -> [emptyCell]
        StartDown       -> [startDown]
        StartLeft       -> [startLeft] 
        StartRight      -> [startRight]
        StartUp         -> [startUp]
        WinDown         -> [winDown]
        WinLeft         -> [winLeft]
        WinRight        -> [winRight]
        WinUp           -> [winUp]

{-
    Tip de date pentru reprezentarea nivelului curent
    retinut pos blocului, si celeulel ca pe un array??
-}
data Level = EmptyLevel | Lv {  lastposMap  :: Position,
                                lvlCells    :: (A.Array Position Cell) }
    deriving (Eq, Ord)
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Level.
    În cazul acesta, eliminați deriving (Eq, Ord) din Level.
-}

{-
    *** TODO ***

    Instanțiati Level pe Show. 
    Atenție! Fiecare linie este urmată de \n (endl in Pipes).
-}
-- showCell:: (Position, Cell)->Position->String
-- showCell mapCell block
--   | fst mapCell == block    = [block]
--   | otherwise               = show (snd mapCell)


instance Show Level 
    where 
        show EmptyLevel = ""
        show lvl@(Lv lastPos cells) = foldl printCell "" cellList ++ [endl]
            where 
                cellList = A.assocs cells
                printCell out crtCell --(int,int) char
                    | snd (fst crtCell) == 0        = out ++ [endl] ++ cellToString --daca sunt pe col 0
                    | otherwise                     = out ++ cellToString --restul de linii
                        where
                            cellToString = show (snd crtCell) 


{-
    *** TODO ***
    Primește coordonatele colțului din dreapta jos a hărții.
    Intoarce un obiect de tip Level în care tabla este populată
    cu EmptySpace. Implicit, colțul din stânga sus este (0,0)
-}

emptyLevel :: Position -> Level
emptyLevel (l, c) = Lv lastPos cells
    where
        lastPos = (l,c)
        cells = A.array ((0, 0), (l, c)) [((i, j), EmptySpace) | i <- [0..l], j <- [0..c]]
      --  justABlock = cells --A.// [(start, BlockCell)]

{-
    *** TODO ***

    Adaugă o celulă de tip Pipe în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        verPipe -> pipe vertical
        horPipe -> pipe orizontal
        topLeft, botLeft, topRight, botRight -> pipe de tip colt
        startUp, startDown, startLeft, startRight -> pipe de tip initial
        winUp, winDown, winLeft, winRight -> pipe de tip final
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugată
    celula, dacă aceasta este liberă (emptySpace).
-}

addCell :: (Char, Position) -> Level -> Level
addCell _ EmptyLevel                        = EmptyLevel
addCell (cellType, cellPos) (Lv pos cells)  
    | outofLimits ||  not(currentCell == EmptySpace)            = Lv pos cells
    | otherwise                                                 = Lv pos newCells
        where
            outofLimits = fst cellPos > fst pos || snd cellPos > snd pos || fst cellPos < 0 || snd cellPos < 0
            -- lastPoint = snd ( A.bounds cells) --asta e stanga jos
            currentCell = cells A.! cellPos
            newCells = cells A.//[(cellPos, newCell)]
            -- newCell = WinDown
            newCell
                | cellType ==  startRight      = StartRight
                | cellType ==  startUp         = StartUp
                | cellType ==  startLeft       = StartLeft
                | cellType ==  startDown       = StartDown
                | cellType ==  topRight        = TopRight
                | cellType ==  botRight        = BotRight
                | cellType ==  botLeft         = BotLeft
                | cellType ==  topLeft         = TopLeft
                | cellType ==  verPipe         = VerPipe
                | cellType ==  horPipe         = HorPipe
                | cellType ==  winRight        = WinRight
                | cellType ==  winLeft         = WinLeft
                | cellType ==  winDown         = WinDown
                | cellType ==  winUp           = WinUp
                | cellType ==  emptySpace      = EmptySpace
                | cellType ==  emptyCell       = EmptyCell
                | otherwise                    = EmptySpace
                -- '░'             -> EmptySpace
                -- '▓'             -> EmptyCell
                -- '═'             -> HorPipe
                -- '║'             -> VerPipe
                -- '╔'             -> TopLeft 
                -- '╚'             -> BotLeft
                -- '╝'             -> BotRight 
                -- '╗'             -> TopRight
                -- '┬'             -> StartDown
                -- '┤'             -> StartLeft 
                -- '├'             -> StartRight
                -- '┴'             -> StartUp
                -- '╥'             -> WinDown
                -- '╡'             -> WinLeft
                -- '╞'             -> WinRight
                -- '╨'             -> WinUp
    



{-
    *** TODO *** 

    Primește coordonatele colțului din dreapta jos al hărții și o listă de 
    perechi de tipul (caracter_celulă, poziția_celulei).
    Întoarce un obiect de tip Level cu toate celeule din listă agăugate pe
    hartă.
    Observatie: Lista primită ca parametru trebuie parcursă de la dreapta 
    la stanga.
-}
 
createLevel :: Position -> [(Char, Position)] -> Level
createLevel lastPos cells = foldr addCell (emptyLevel lastPos) cells
 
{-
    *** TODO ***

    Mișcarea unei celule în una din cele 4 direcții 
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.

    Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.
-}

moveCell :: Position -> Directions -> Level -> Level
moveCell _ _ EmptyLevel                        = EmptyLevel
moveCell currentPosition direction lvl@(Lv lastPos cells)
    |condition                      = Lv lastPos ( cells A.//[(newPosition,celltoMove),(currentPosition,cellinDirection)] )
    |otherwise                      = lvl
        where --trebuie sa verific si daca pozitia viitoare e ok si nu depaseste limitele
            condition               = not (isImmutable celltoMove) && inlimits && cellinDirection == EmptySpace
            isImmutable c           = isStart c || isWin c
            isStart c               = elem c [StartDown, StartLeft, StartRight, StartUp]
            isWin c                 = elem c [WinDown, WinLeft, WinRight, WinUp ]
            inlimits                = limitsCurrent && limitsMove
            limitsCurrent           = currentL  <= (fst lastPos) && currentC <= (snd lastPos) && currentL > -1 && currentC > -1
            limitsMove              = (fst newPosition)   <= (fst lastPos) && (snd newPosition) <= (snd lastPos) && (fst newPosition) > -1 && (snd newPosition) > -1
            cellinDirection         = cells A.! newPosition
            celltoMove              = cells A.! currentPosition
            newPosition             = (currentL + moveL,currentC+ moveC)::Position
            currentL                = fst currentPosition
            currentC                = snd currentPosition
            moveL                   = fst movePosition
            moveC                   = snd movePosition 
            movePosition = case direction of
                North   -> (-1,0)
                South   -> (1,0)
                East    -> (0,1)
                West    -> (0,-1)
{-
  *** HELPER ***

    Verifică dacă două celule se pot conecta.
    Atenție: Direcția indică ce vecin este a
    doua celulă pentru prima.

    ex: connection botLeft horPipe East = True (╚═)
        connection horPipe botLeft East = False (═╚)
-}
connection :: Cell -> Cell -> Directions -> Bool
connection c1 c2 direction
    | needsRight c1 && needsLeft c2         = True
    | needsDown c1 && needsUp c2            = True
    | otherwise                             = False
        where
            needsRight c    = elem c [HorPipe, BotLeft, TopLeft, StartRight, WinRight]
            needsLeft  c    = elem c [HorPipe, BotRight, TopRight, StartLeft, WinLeft]
            needsDown c     = elem c [VerPipe, TopLeft, TopRight, StartDown, WinDown]
            needsUp c       = elem c [VerPipe, BotLeft, BotRight, StartUp, WinUp]

{-
    *** TODO ***

    Va returna True dacă jocul este câștigat, False dacă nu.
    Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
    de tip inițial la cea de tip final.
    Este folosită în cadrul Interactive.
-}
allPipesConnection:: Position -> Level -> Bool
allPipesConnection _ EmptyLevel                             = False
allPipesConnection startPos lvl@(Lv lastPos cells)
    | torightinLimits && (connection startCell rightCell East)   = allPipesConnection rightPos lvl
    | todowninLimits && (connection startCell downCell South)     = allPipesConnection downPos lvl
    | isaWin                                                = True
    | otherwise                                             = False
        where
            startCell               = cells A.! startPos
            rightCell               = cells A.! rightPos
            downCell                = cells A.! downPos
            rightPos                = ((fst startPos), (snd startPos)+ 1)::Position
            downPos                 = ((fst startPos)+1, (snd startPos))::Position
            torightinLimits         = (fst rightPos) <= (fst lastPos) && (snd rightPos) <= (snd lastPos) && (fst rightPos) >= 0 && (snd rightPos) >= 0
            todowninLimits          = (fst downPos) <= (fst lastPos) && (snd downPos) <= (snd lastPos) && (fst downPos) >= 0 && (snd downPos) >= 0
            isaWin                  = elem startCell [WinDown, WinLeft, WinRight, WinUp]


wonLevel :: Level -> Bool
wonLevel EmptyLevel = True
wonLevel lvl@(Lv lastPos cells)
    | allPipesConnection start lvl          = True
    | otherwise                             = False
        where
           start       = (0,0)::Position


positionNotConnected:: Position -> Level -> Position
positionNotConnected _ EmptyLevel                             = originalStart
    where
        originalStart           = (0,0)::Position
positionNotConnected startPos lvl@(Lv lastPos cells)
    | torightinLimits && (connection startCell rightCell East)   = positionNotConnected rightPos lvl
    | todowninLimits && (connection startCell downCell South)    = positionNotConnected downPos lvl
    | isaWin                                                = startPos
    | otherwise                                             = startPos
        where
            originalStart           = (0,0)::Position
            startCell               = cells A.! startPos
            rightCell               = cells A.! rightPos
            downCell                = cells A.! downPos
            rightPos                = ((fst startPos), (snd startPos)+ 1)::Position
            downPos                 = ((fst startPos)+1, (snd startPos))::Position
            torightinLimits         = (fst rightPos) <= (fst lastPos) && (snd rightPos) <= (snd lastPos) && (fst rightPos) >= 0 && (snd rightPos) >= 0
            todowninLimits          = (fst downPos) <= (fst lastPos) && (snd downPos) <= (snd lastPos) && (fst downPos) >= 0 && (snd downPos) >= 0
            isaWin                  = elem startCell [WinDown, WinLeft, WinRight, WinUp]

-- possiblePos lastokPos lvl = [] ++ toRight ++ toLeft ++ toUp ++ toDown
--     where
--         toRight = 
instance ProblemState Level (Position, Directions) where
    -- successors = undefined
    successors lvl@(Lv lastPos cells)
        | isGoal lvl = []
        | otherwise = dirs

        --sa gasesc ultima pozitie? ok 
        --sa gasesc cu ce sa o lipsesc in continuare? dar cum
        
        -- | otherwise = filter (\(pos,dir),level)-> isinLimits pos dir) dirs
            where
                -- isinLimits p d = 
                cellList = A.assocs cells
                lastokPos = positionNotConnected originalStart lvl
                -- possiblePos = )
                pos = (fst lastokPos,(snd lastokPos)+1)::Position
                originalStart = (0,0)::Position
                northLevel = ((pos,North), moveCell pos North lvl)
                southLevel = ((pos,South), moveCell pos South lvl)
                eastLevel  = ((pos,East), moveCell pos East lvl)
                westLevel  = ((pos,West), moveCell pos West lvl)
                dirs = [northLevel, southLevel, eastLevel, westLevel]
-- -- -- moveCell currentPosition direction lvl@(Lv lastPos cells)
-- sa caut pentru urmatoarea pozitie in toate partile posibile
--dar cum? caut care e urmatoarea pozitie posibila
--caut ce mutari se poate face 
-- nu ??
-- checkSuccessors :: Level -> [(Position, Directions)] -> Bool
-- checkSuccessors lvl lst = checkSet (succ1, succ2) where
--         succ1 = successors lvl
--         succ2 = L.map (\(pos,dir) -> ((pos,dir), moveCell pos dir lvl)) lst

-- succLevel1 :: [(Position, Directions)]
-- succLevel1 = [((3,2),West),((3,0),East),((2,1),South)]


   
    isGoal EmptyLevel = False
    isGoal lvl@(Lv lastPos cells) = wonLevel lvl

    reverseAction = undefined
