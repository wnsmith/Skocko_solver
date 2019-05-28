module Main (main) where


import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)
import Graphics.UI.Gtk.Buttons.Button

import Data.List hiding (on)
import Data.Ord hiding (on)
import Data.Function hiding (on)
import Data.Char hiding (on)



prec 0 0 = [3,3,4,4] :: [Int]
prec 0 1 = [1,3,3,4]
prec 0 2 = [3,4,2,1]
prec 0 3 = [1,3,2,0]
prec 0 4 = [1,2,0,0]
prec 1 0 = [0,3,4,5]
prec 2 0 = [0,3,1,4]
prec 3 0 = [0,3,1,2]
prec 1 1 = [0,3,0,4]
prec 1 2 = [0,3,2,1]
prec 1 3 = [0,1,2,0]
prec 2 1 = [0,3,0,2]
prec 2 2 = [0,1,0,2]
prec _ _ = []

ispis 0 = "♠"
ispis 1 = "♥"
ispis 2 = "♦"
ispis 3 = "♣"
ispis 4 = "☻"
ispis 5 = "★"
ispis _ = "wassup"


conv [] = []
conv lista = [] ++ [(take 4 lista)] ++ (conv (drop 4 lista))


 
 
count n xs = (length . filter (==n)) xs

yHelp l = [count x l| x<-[0..5]]

yellow l1 l2 = sum(zipWith min (yHelp l1) (yHelp l2))

red l1 l2 = count True (zipWith (==) l1 l2)

judge expec got = (r,y-r)
    where y = yellow expec got
          r = red expec got

nextComb oldCombs lastTry w = [x | x<- oldCombs, judge x lastTry == w]

nextCombs combos sol trry = nextComb combos trry w
    where w = judge sol trry

fitness combos comb = [length x | x <- [nextCombs combos sol comb | sol <- combos]]

sumsOf combos = [sum(fitness combos comb)| comb <- combos]

mini xs = minimumBy (comparing fst) (zip xs [0..])

makeZeMove combos = combos !! (snd (mini (sumsOf combos)))

mkBtn
  :: String              
  -> IO Button          

mkBtn label  = do
  btn <- buttonNew
  set btn [ buttonLabel := label ]
  return btn


 


main :: IO ()
main = do
  void initGUI
  window <- windowNew
  set window [ windowTitle         := "Skocko"
             , windowResizable     := True
             , windowDefaultWidth  := 300
             , windowDefaultHeight := 525 ]

  grid <- gridNew
  gridSetRowHomogeneous grid True
  gridSetColumnHomogeneous grid True

     

  

  bt1 <- mkBtn "♠"  
  gridAttach grid bt1 0 1 1 1

  bt2 <- mkBtn "♠"  
  gridAttach grid bt2 1 1 1 1

  bt3 <- mkBtn "♥"  
  gridAttach grid bt3 2 1 1 1

  bt4 <- mkBtn "♦"  
  gridAttach grid bt4 3 1 1 1

  bt5 <- mkBtn ""  
  gridAttach grid bt5 0 2 1 1

  bt6 <- mkBtn ""  
  gridAttach grid bt6 1 2 1 1

  bt7 <- mkBtn ""  
  gridAttach grid bt7 2 2 1 1

  bt8 <- mkBtn ""  
  gridAttach grid bt8 3 2 1 1

  bt9 <- mkBtn ""  
  gridAttach grid bt9 0 3 1 1

  bt10 <- mkBtn ""  
  gridAttach grid bt10 1 3 1 1

  bt11 <- mkBtn ""  
  gridAttach grid bt11 2 3 1 1

  bt12 <- mkBtn "" -- id
  gridAttach grid bt12 3 3 1 1

  bt13 <- mkBtn ""  
  gridAttach grid bt13 0 4 1 1

  bt14 <- mkBtn ""  
  gridAttach grid bt14 1 4 1 1

  bt15 <- mkBtn ""  
  gridAttach grid bt15 2 4 1 1

  bt16 <- mkBtn ""  
  gridAttach grid bt16 3 4 1 1

  bt17 <- mkBtn ""  
  gridAttach grid bt17 0 5 1 1

  bt18 <- mkBtn ""  
  gridAttach grid bt18 1 5 1 1

  bt19 <- mkBtn ""  
  gridAttach grid bt19 2 5 1 1

  bt20 <- mkBtn ""  
  gridAttach grid bt20 3 5 1 1

  bt21 <- mkBtn ""  
  gridAttach grid bt21 0 6 1 1

  bt22 <- mkBtn ""  
  gridAttach grid bt22 1 6 1 1

  bt23 <- mkBtn ""  
  gridAttach grid bt23 2 6 1 1

  bt24 <- mkBtn ""  
  gridAttach grid bt24 3 6 1 1

  

  let buttons = [bt1, bt2, bt3, bt4, bt5, bt6, bt7, bt8, bt9, bt10, bt11, bt12, bt13, bt14, bt15, bt16, bt17, bt18, bt19, bt20, bt21, bt22, bt23, bt24]
 
 
  go <- mkBtn "GO"  
  gridAttach grid go 0 7 2 1



  let combos = [x | x <- mapM (const [0..5]) [0..3]] :: [[Int]]
  
 
  combBtn <- mkBtn (show combos) 
  inicBtn <- mkBtn (show [0, 0, 1, 2]) 
  globalNBtn <- mkBtn "1" 

 
 
  redBtn <- mkBtn "0" 
  gridAttach grid redBtn 2 7 1 1

  ytlBtn <- mkBtn "0" 
  gridAttach grid ytlBtn 3 7 1 1


  





  on go buttonActivated  (do  
                            inicT <- buttonGetLabel inicBtn
                            let inic = map (\x -> read [x] :: Int) (filter (\x -> isDigit x) inicT)
                            combosT <- buttonGetLabel combBtn
                            let combo = map (\x -> read [x] :: Int) (filter (\x -> isDigit x) combosT)
                            let combos = conv combo
                            globalN <- buttonGetLabel globalNBtn
                            gr <- buttonGetLabel redBtn
                            gy <- buttonGetLabel ytlBtn
                            let globalR = read gr :: Int
                            let globalY = read gy :: Int
                            case globalN of
                              "1" -> do
                                      let inic = prec globalR globalY
                                      buttonSetLabel inicBtn (show inic)
                                      buttonSetLabel globalNBtn "2"
                                      buttonSetLabel (buttons !! 4) (ispis (inic !! 0 ))
                                      buttonSetLabel (buttons !! 5) (ispis (inic !! 1 ))
                                      buttonSetLabel (buttons !! 6) (ispis (inic !! 2 ))
                                      buttonSetLabel (buttons !! 7) (ispis (inic !! 3 ))

                                      let comboss = nextComb combos [0,0,1,2] (globalR, globalY)
                                      buttonSetLabel combBtn (show comboss)
                              "2" -> do
                                      let comboss = nextComb combos inic (globalR, globalY)
                                      let inic = makeZeMove comboss
                                      buttonSetLabel inicBtn (show inic)
                                      buttonSetLabel globalNBtn "3"
                                      buttonSetLabel (buttons !! 8) (ispis (inic !! 0 ))
                                      buttonSetLabel (buttons !! 9) (ispis (inic !! 1 ))
                                      buttonSetLabel (buttons !! 10) (ispis (inic !! 2 ))
                                      buttonSetLabel (buttons !! 11) (ispis (inic !! 3 ))
                                      buttonSetLabel combBtn (show comboss)
                              "3" -> do
                                      let comboss = nextComb combos inic (globalR, globalY)
                                      let inic = makeZeMove comboss
                                      buttonSetLabel inicBtn (show inic)
                                      buttonSetLabel globalNBtn "4"
                                      buttonSetLabel (buttons !! 12) (ispis (inic !! 0 ))
                                      buttonSetLabel (buttons !! 13) (ispis (inic !! 1 ))
                                      buttonSetLabel (buttons !! 14) (ispis (inic !! 2 ))
                                      buttonSetLabel (buttons !! 15) (ispis (inic !! 3 ))
                                      buttonSetLabel combBtn (show comboss)
                              "4" -> do
                                      let comboss = nextComb combos inic (globalR, globalY)
                                      let inic = makeZeMove comboss
                                      buttonSetLabel inicBtn (show inic)
                                      buttonSetLabel globalNBtn "5"
                                      buttonSetLabel (buttons !! 16) (ispis (inic !! 0 ))
                                      buttonSetLabel (buttons !! 17) (ispis (inic !! 1 ))
                                      buttonSetLabel (buttons !! 18) (ispis (inic !! 2 ))
                                      buttonSetLabel (buttons !! 19) (ispis (inic !! 3 ))
                                      buttonSetLabel combBtn (show comboss)
                              "5" -> do
                                      let comboss = nextComb combos inic (globalR, globalY)
                                      let inic = makeZeMove comboss
                                      buttonSetLabel inicBtn (show inic)
                                      buttonSetLabel globalNBtn "6"
                                      buttonSetLabel (buttons !! 20) (ispis (inic !! 0 ))
                                      buttonSetLabel (buttons !! 21) (ispis (inic !! 1 ))
                                      buttonSetLabel (buttons !! 22) (ispis (inic !! 2 ))
                                      buttonSetLabel (buttons !! 23) (ispis (inic !! 3 ))
                                      buttonSetLabel combBtn (show comboss)
                             )




  on redBtn buttonActivated (do 
                                xx <- buttonGetLabel redBtn
                                case xx of
                                  "4" -> buttonSetLabel redBtn "0"
                                  "0" -> buttonSetLabel redBtn "1" 
                                  "1" -> buttonSetLabel redBtn "2" 
                                  "2" -> buttonSetLabel redBtn "3"
                                  "3" -> buttonSetLabel redBtn "4" 
                              )
                                    
  on ytlBtn buttonActivated (do 
                                xx <- buttonGetLabel ytlBtn
                                case xx of
                                  "4" -> buttonSetLabel ytlBtn "0"
                                  "0" -> buttonSetLabel ytlBtn "1" 
                                  "1" -> buttonSetLabel ytlBtn "2" 
                                  "2" -> buttonSetLabel ytlBtn "3"
                                  "3" -> buttonSetLabel ytlBtn "4" 
                              )
                                    

  containerAdd window grid
  window `on` deleteEvent $ do
    liftIO mainQuit
    return False
  widgetShowAll window

  
  mainGUI


 
 
 
 

 


 