{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}

module Basics where
{-
    Expune funcțiile necesare reprezentării jocului.
-}

import ProblemState
import Data.List
import Data.Maybe (fromMaybe)

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc.
    Colțul stânga-sus este (0, 0).
-}
type Position = (Int, Int) 

{-
    Tip de date pentru reprezentarea Target-urilor.
    Acestea conțin informații atât despre poziția curentă a
    Target-ului cât și despre comportamentul acestuia.
    Tipul Behavior este definit mai jos.
-}
data Target = Target {
    position :: Position,
    behavior :: Behavior
}


instance Eq Target where
    Target p1 _ == Target p2 _ = p1 == p2

instance Ord Target where
    Target p1 _ <= Target p2 _ = p1 <= p2

{-
    Tip de date pentru reprezentarea comportamentului unui Target.
    Tipul Behavior este utilizat pentru a modela tranziția Target-urilor
    din starea curentă în starea următoare. Primul parametru este poziția
    actuală a target-ului, iar al doilea, starea curentă a jocului.
    Tipul Game este definit mai jos.
    
    Observați că, din moment ce un Behavior produce un Target nou,
    acesta din urmă ar putea fi caracterizat de un alt Behavior
    decât cel anterior.
-}
type Behavior = Position -> Game -> Target

{-
    Direcțiile de deplasare pe tablă
-}
data Direction = North | South | West | East
    deriving (Eq, Show)
{-
    *** TODO ***
    
    Tip de date pentru reprezentarea stării jocului, la un anumit
    moment. Completați-l cu orice informație aveți nevoie pentru
    stocarea stării jocului (hunter, target, obstacole, gateways).
-}  
data Game = G {
    rows :: Int,
    cols :: Int,
    hunter :: Position,
    target :: [Target],
    obstacol :: [Position],
    gateway :: [(Position , Position)]
} deriving (Eq, Ord)
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Game.
    În cazul acesta, eliminați deriving (Eq, Ord) din Game.
-}

{-
    *** TODO ***

    Reprezentați starea jocului ca șir de caractere, pentru afișarea
    la consolă.
    
    Atenție! Fiecare linie, mai puțin ultima, este urmată de \n.
    Celule goale vor fi reprezentate ca ' '.
    Hunter-ul va fi reprezentat ca '!'.
    Target-urile vor fi reprezentate ca '*'
    Gateways-urile vor fi reprezentate ca '#'.
    Obstacolele vor fi reprezentate de '@'.

    Hint: S-ar putea să vă fie utile list comprehensions,
    precum și funcțiile elem, any și intercalate din Data.List.
-}
is_hunter :: Position -> Int -> Int -> Bool
is_hunter hunter x y =  (x, y) == hunter

list_pos :: [Target] -> [Position]
list_pos target = foldl (\acc x -> (position x) : acc) [] target

is_target :: [Target] -> Int -> Int -> Bool
is_target target x y = elem (x, y) (list_pos target)

is_obstacol :: [Position] -> Int -> Int -> Bool
is_obstacol obstacol x y = elem (x, y) obstacol

gateway_getX :: [(Position, Position)] -> [Position]
gateway_getX gateway = foldl (\acc x -> (fst x) : acc) [] gateway

gateway_getY :: [(Position, Position)] -> [Position]
gateway_getY gateway = foldl (\acc x -> (snd x) : acc) [] gateway

is_gateway :: [(Position, Position)] -> Int -> Int -> Bool
is_gateway gateway x y = elem (x, y) (gateway_getX gateway) || elem (x, y) (gateway_getY gateway)

element :: Game ->Int -> Int -> String
element (G rows cols hunter target obstacol gateway) x y
    | is_hunter hunter x y == True = "!"
    | is_target target x y == True = "*"
    | is_obstacol obstacol x y == True = "@"
    | is_gateway gateway x y == True = "#"
    | otherwise = " "

gameAsString :: Game -> String
gameAsString g = foldl (\ans x -> foldl(\acc y -> if y == (cols g) - 1 && x /= (rows g) - 1 then acc ++ (element g x y) ++ "\n" else acc ++ (element g x y)) ans [0..(cols g)-1]) "" [0..(rows g)-1]

instance Show Game where
    show = gameAsString

{-
    *** TODO ***
    
    Primește numărul de linii și numărul de coloane ale tablei de joc.
    Intoarce un obiect de tip Game în care tabla conține spații goale în interior, fiind
    împrejmuită de obstacole pe toate laturile. Implicit, colțul din stânga sus este (0,0),
    iar Hunterul se găsește pe poziția (1, 1).
-}
emptyGame :: Int -> Int -> Game
emptyGame row col = (G row col hunter target obstacol gateway)
    where
        hunter = (1, 1)
        target = []
        gateway = []
        obstacol = [(0, x) | x <- [0..col-1]] ++ [(row - 1, x) | x <- [0..col-1]] ++ [(x, 0) | x <- [0..row-1]] ++ [(x, col - 1) | x <- [0..row-1]]

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, cu Hunter-ul pus
    pe poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugat Hunter-ul
    Daca poziția este invalidă (ocupată sau în afara tablei de joc) se va întoarce
    același joc.
-}
addHunter :: Position -> Game -> Game
addHunter (x, y) (G row col hunter target obstacol gateway) = (G row col new_hunter target obstacol gateway)
    where
        new_hunter
            | is_target target x y == True || is_obstacol obstacol x y == True || is_gateway gateway x y == True = hunter
            | x >= row || x < 0 || y >= col || y < 0 = hunter
            | otherwise = (x, y)

{-
    *** TODO ***

    Primește un comportament, o poziție și un joc și întoarce un nou joc, în care a fost
    adăugat Target-ul descris de comportament și poziție.
    Parametrul Behavior reprezintă comportamentul Hunter-ului care va fi adăugat.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat Target-ul.
-}
addTarget :: Behavior -> Position -> Game -> Game
addTarget behavior (x, y) (G row col hunter target obstacol gateway) = (G row col hunter new_target obstacol gateway)
    where
        new_target = target ++ [(Target (x, y) behavior)]

{-
    *** TODO ***

    Primește o pereche de poziții și un joc și întoarce un nou joc, în care au fost adăugate
    cele două gateway-uri interconectate.
    Parametrul (Position, Position) reprezintă pozițiile de pe hartă la care vor fi adăugate 
    cele două gateway-uri interconectate printr-un canal bidirecțional.
-}
addGateway :: (Position, Position) -> Game -> Game
addGateway (pos1, pos2) (G row col hunter target obstacol gateway) = (G row col hunter target obstacol new_gateway)
    where
        new_gateway = gateway ++ [(pos1, pos2)]

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, în care a fost adăugat un obstacol
    la poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat obstacolul.
-}
addObstacle :: Position -> Game -> Game
addObstacle (x, y) (G row col hunter target obstacol gateway) = (G row col hunter target new_obstacol gateway)
    where
        new_obstacol = obstacol ++ [(x, y)]

{-
    *** TODO ***
    
    Primește o poziție destinație înspre care vrea să se deplaseze o entitate (Hunter sau Target)
    și verifică daca deplasarea este posibilă, întorcând noua poziție, luând în considerare
    și Gateway-urile.
    Avem următoarele cazuri:
    - dacă poziția corespunde unui spațiu gol, se întoarce acea poziție;
    - dacă poziția corespunde unui gateway, se întoarce poziția gateway-ului pereche;
    - dacă poziția corespunde unui obstacol, se întoarce Nothing.
    Parametrul Position reprezintă poziția destinație.
-}
gateway_pair :: Position -> [(Position, Position)] -> Position
gateway_pair pos1 gateway
    | elem pos1 (gateway_getX gateway) = foldl (\acc x -> if (fst x) == pos1 then (snd x) else acc) (-1, -1) gateway
    | otherwise = foldl (\acc x -> if (snd x) == pos1 then (fst x) else acc) (-1, -1) gateway

attemptMove :: Position -> Game -> Maybe Position
attemptMove (x, y) (G row col hunter target obstacol gateway)
    | is_obstacol obstacol x y == True || is_target target x y == True || is_hunter hunter x y == True = Nothing
    | is_gateway gateway x y == True = Just (gateway_pair (x, y) gateway)
    | otherwise = Just (x, y)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre est. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
    
    Conform definiției, tipul Behavior corespunde tipului funcție
    Position -> Game -> Target.
    
    Având în vedere că cele patru funcții definite în continuare (goEast, goWest,
    goNorth, goSouth) sunt foarte similare, încercați să implementați o funcție
    mai generală, pe baza căreia să le definiți apoi pe acestea patru.
-}
inInterval :: Position -> Int -> Int -> Bool
inInterval (x, y) rows cols
    | x >= 0 && x < rows && y >=0 && y < cols = True
    | otherwise = False


goEast :: Behavior
goEast (x, y) g@(G row col hunter target obstacol gateway)
    | pos == (x, y) && is_gateway gateway x y = (Target (gateway_pair (x, y) gateway) goEast)
    | inInterval (x, y + 1) row col == True = (Target pos goEast)
    where
        pos = fromMaybe (x, y) (attemptMove (x, y + 1) g)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre vest. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goWest :: Behavior
goWest (x, y) g@(G row col hunter target obstacol gateway)
    | pos == (x, y) && is_gateway gateway x y = (Target (gateway_pair (x, y) gateway) goWest)
    | inInterval(x, y - 1) row col == True = (Target pos goWest)
    where
        pos = fromMaybe (x, y) (attemptMove (x, y - 1) g)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre nord. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goNorth :: Behavior
goNorth (x, y) g@(G row col hunter target obstacol gateway)
    | inInterval(x - 1, y) row col == True = (Target pos goNorth)
    where
        pos = fromMaybe (x, y) (attemptMove (x - 1, y) g)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre sud. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goSouth :: Behavior
goSouth (x, y) g@(G row col hunter target obstacol gateway)
    | inInterval(x + 1, y) row col == True = (Target pos goSouth)
    where
        pos = fromMaybe (x, y) (attemptMove (x + 1, y) g)

{-
    *** TODO ***

    Comportamentul unui Target de a-și oscila mișcarea, când înspre nord, când înspre sud. 
    Mișcarea se poate face doar dacă poziția este validă (se află pe tablă de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul iși va schimba
    direcția de mers astfel:
    - daca mergea inspre nord, își va modifica direcția miscării înspre sud;
    - daca mergea inspre sud, își va continua mișcarea înspre nord.
    Daca Target-ul întâlneste un Gateway pe traseul său, va trece prin acesta,
    către Gateway-ul pereche conectat și își va continua mișcarea în același sens la ieșire
    din acesta.
    Puteți folosit parametrul Int pentru a surprinde deplasamentul Target-ului (de exemplu,
    1 pentru sud, -1 pentru nord).
-}

bounce :: Int -> Behavior
bounce deplasament (x, y) g@(G row col hunter target obstacol gateway)
    | deplasament == 1 = if (x, y) == (position (goSouth (x, y) g))
         then if (x, y) == (position (goNorth (x, y) g)) && is_gateway gateway x y 
            then (Target (gateway_pair (x, y) gateway) (bounce (-1)))
            else (Target (position (goNorth (x, y) g)) (bounce (-1)))
        else (Target (position (goSouth (x, y) g)) (bounce 1))
    | otherwise = if (x, y) == (position (goNorth (x, y) g))
         then if (x, y) == (position (goSouth (x, y) g)) && is_gateway gateway x y 
            then (Target (gateway_pair (x, y) gateway) (bounce 1))
            else (Target (position (goSouth (x, y) g)) (bounce 1))
        else (Target (position (goNorth (x, y) g)) (bounce (-1)))
      
{-
    *** TODO ***
    Funcție care mută toate Target-urile din Game-ul dat o poziție, în functie
    de behavior-ul fiecăreia și întoarce noul Game în care pozițiile Target-urilor
    sunt actualizate.
    
-}
moveTargets :: Game -> Game
moveTargets g@(G row col hunter target obstacol gateway) = (G row col hunter new_target obstacol gateway)
    where
        new_target = foldl (\acc x -> acc ++ [(behavior x) (position x) g]) [] target

{-
    *** TODO ***

    Verifică dacă Targetul va fi eliminat de Hunter.
    Un Target este eliminat de Hunter daca se află pe o poziție adiacentă
    cu acesta.
    Parametrul Position reprezintă poziția Hunterului pe tabla
    de joc.
    Parametrul Target reprezintă Targetul pentru care se face verificarea.
-}
isTargetKilled :: Position -> Target -> Bool
isTargetKilled (x, y) target
    | (position target) == (x + 1, y) || (position target) == (x - 1, y) || (position target) == (x, y - 1) || (position target) == (x, y + 1) = True
    | otherwise = False

{-
    *** TODO ***

    Avansează starea jocului curent, rezultând starea următoare a jocului.
    Parametrul Direction reprezintă direcția în care se va deplasa Hunter-ul.
    Parametrul Bool specifică dacă, după mutarea Hunter-ului, vor fi
    mutate și Target-urile sau nu, și dacă vor fi eliminate din joc sau nu.
    Este folosit pentru a distinge între desfășurarea reală a jocului (True)
    și planificarea „imaginată” de hunter (False) în partea a doua a temei.

    Avansarea stării jocului respectă următoarea ordine:
    1. Se deplasează Hunter-ul.
    2. În funcție de parametrul Bool, se elimină Target-urile omorâte de către Hunter.
    3. In funcție de parametrul Bool, se deplasează Target-urile rămase pe tablă.
    4. Se elimină Targeturile omorâte de către Hunter și după deplasarea acestora.
    
    Dubla verificare a anihilării Target-urilor, în pașii 2 și 4, îi oferă Hunter-ului
    un avantaj în prinderea lor.
-}
functPosition :: Position -> Direction -> Game -> Maybe Position
functPosition (x, y) dir g@(G row col hunter target obstacol gateway)
    | dir == North = attemptMove (x - 1, y) g
    | dir == South = attemptMove (x + 1, y) g
    | dir == West = attemptMove (x, y - 1) g
    | otherwise = attemptMove (x, y + 1) g

changeTarget :: Position -> [Target] -> [Target]
changeTarget hunter target = foldl (\acc x -> if isTargetKilled hunter x then acc else acc ++ [x]) [] target

advanceGameState :: Direction -> Bool -> Game -> Game
advanceGameState direction cond g@(G row col hunter tar obstacol gateway) = (G row col new_hunter new_target obstacol gateway)
    where
        new_hunter = fromMaybe hunter (functPosition hunter direction g)
        new_target
            | cond == True = changeTarget new_hunter (target (moveTargets (G row col new_hunter (changeTarget new_hunter tar) obstacol gateway)))
            | otherwise = tar
{-
    ***  TODO ***

    Verifică dacă mai există Target-uri pe table de joc.
-}
areTargetsLeft :: Game -> Bool
areTargetsLeft (G row col hunter target obstacol gateway) = null target

{-
    *** BONUS TODO ***

    Comportamentul unui Target de a se deplasa în cerc, în jurul unui Position, având
    o rază fixată.
    Primul parametru, Position, reprezintă centrul cercului.
    Parametrul Int reprezintă raza cercului.
    Puteți testa utilizând terenul circle.txt din directorul terrains, în conjuncție
    cu funcția interactive.
-}
circle :: Position -> Int -> Behavior
circle = undefined

auxGoal :: Game -> Position
auxGoal game = foldl (\acc x -> if isTargetKilled (hunter game) x then (position x) else acc) (0, 0) (target game)

instance ProblemState Game Direction where
    {-
        *** TODO ***
        
        Generează succesorii stării curente a jocului.
        Utilizați advanceGameState, cu parametrul Bool ales corespunzător.
    -}
    successors game = [(North, advanceGameState North False game)] ++ [(South, advanceGameState South False game)]
                            ++ [(West, advanceGameState West False game)] ++ [(East, advanceGameState East False game)]  

    {-
        *** TODO ***
        
        Verifică dacă starea curentă este un în care Hunter-ul poate anihila
        un Target. Puteți alege Target-ul cum doriți, în prezența mai multora.
    -}
    isGoal game = foldl (\acc x -> if isTargetKilled (hunter game) x then True else acc) False (target game)

    {-
        *** TODO ***
        
        Euristica euclidiană (vezi hEuclidian mai jos) până la Target-ul ales
        de isGoal.
    -}

    h game
        | isGoal game = hEuclidean (hunter game) (auxGoal game)
        | otherwise = 0.0

{-
     ** NU MODIFICATI **
-}
hEuclidean :: Position -> Position -> Float
hEuclidean (x1, y1) (x2, y2) = fromIntegral $ ((x1 - x2) ^ pow) + ((y1 - y2) ^ pow)
  where
    pow = 2 :: Int

{-
    *** BONUS ***

    Acesta reprezintă un artificiu necesar pentru testarea bonusului,
    deoarece nu pot exista două instanțe diferite ale aceleiași clase
    pentru același tip.

    OBSERVAȚIE: Testarea bonusului pentru Seach este făcută separat.
-}

newtype BonusGame = BonusGame Game
    deriving (Eq, Ord, Show)

{-
    *** BONUS TODO ***

    Folosind wrapper-ul peste tipul Game de mai sus instanțiați
    ProblemState astfel încât să fie folosită noua euristică. 
-}
instance ProblemState BonusGame Direction where
    {-
        *** BONUS TODO ***

        Pentru a ne asigura că toțî succesorii unei stări sunt de tipul
        BonusGame și folosesc noua euristică trebuie să aplicăm wrapper-ul
        definit mai sus peste toți succesorii unei stări.

        Hint: Puteți să folosiți funcția fmap pe perechi pentru acest lucru.
        https://wiki.haskell.org/Functor
    -}
    successors = undefined

    {-
        *** BONUS TODO ***

        Definiți funcția isGoal pentru BonusGame.

        Hint: Folosiți funcția isGoal deja implementată pentru tipul Game.
    -}
    isGoal = undefined

    {-
        *** BONUS TODO ***

        Definiți o funcție euristică care este capabilă să găsească un drum mai scurt
        comparativ cu cel găsit de euristica implementată pentru Game.

        ATENȚIE: Noua euristică NU trebuie să fie una trivială.

        OBSERVAȚIE: Pentru testare se va folosi fișierul terrains/game-6.txt.
    -}
    h = undefined