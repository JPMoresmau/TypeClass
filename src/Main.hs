{-# LANGUAGE ScopedTypeVariables  #-}
module Main where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF

import Reactive.Banana
import Reactive.Banana.SDL
import Reactive.Banana.SDL.Graphics

import Data.Char
import qualified Data.Map as M
import Data.Word
import System.Random
import System.FilePath

import Paths_TypeClass
import Control.Monad.IO.Class (liftIO)
import Reactive.Banana.Frameworks (actuate, Frameworks)
import Control.Monad (void)

main :: IO()
main = do
        sdlES<-getSDLEventSource
        gd<-liftIO initGraphics
        network <- compile $ setupNetwork sdlES gd
        actuate network
        runSDLPump sdlES
        endGraphics gd

setupNetwork :: Frameworks t=> SDLEventSource -> GraphicsData -> Moment t ()
setupNetwork es gd=do
        r<-liftIO getStdGen
        eTickDiff <- tickDiffEvent es
        esdl <- sdlEvent es
        let 
                -- | the initial gamestate 
                gsInitial :: GameState
                gsInitial=GameState 0 100 2000 M.empty r 0 0 lives
        
                -- | the empty screen
                startGraphic :: Graphic
                startGraphic=draw (Fill (Just $ SDL.Rect 0 0 width height) black) (Mask Nothing 0 0)
                        
                -- | we always use the same surface        
                bScreen :: Behavior t Screen
                bScreen = pure (gd_mainSurf gd)
                         
                -- | game state update event
                eGSChange= (updateGS <$> eTickDiff) `union` (updateGSOnKey <$> keyDownEvent esdl)
                
                -- | game state behavior
                bGameState=accumB gsInitial eGSChange
                
                -- | draw lives
                livesG GameState{gs_lives}=draw (Text ("Lives:" ++ show gs_lives ++ "/" ++ show lives) (gd_font gd) red) (Mask Nothing 0 0)
                -- | draw score
                scoreG GameState{gs_score}=draw (Text ("Score:" ++ show gs_score) (gd_font gd) red) (Mask Nothing (halfW+1) 0)
                -- | draw a character
                charG (c,(x,y))= draw (Text [c] (gd_font gd) white) (Mask Nothing x y)
                -- | draw characters      
                charsG GameState{gs_shown}=let
                        chars=map charG (M.assocs gs_shown)
                        in (Graphic $ \surface ->mapM_ (\(Graphic f)->void $ f surface) chars >> return Nothing)
                -- | game over
                gameOverG GameState{gs_score}=draw (Text "Game Over!" (gd_font gd) red) (Mask Nothing (halfW-40) (halfH-20))  
                                `over`
                                        draw (Text ("Score:" ++ show gs_score) (gd_font gd) red) (Mask Nothing (halfW-40) (halfH+10))
                -- | draw behavior                
                bG=(\g->if gs_lives g > 0 
                      then
                        scoreG g `over` livesG g `over` charsG g `over` startGraphic 
                      else
                       gameOverG g `over` startGraphic) <$> bGameState
        -- render       
        renderGraph bG bScreen
        return ()

-- | update game state on key press
updateGSOnKey :: SDL.Keysym -> GameState -> GameState
updateGSOnKey ks gs@GameState{gs_shown,gs_score}=
                let c=toUpper $ SDL.symUnicode ks
                in case c of 
                        '+'->speedup gs
                        _ ->if M.member c gs_shown
                                then gs{gs_shown=M.delete c gs_shown,gs_score=gs_score+1}
                                else gs

-- | update game state on tick
updateGS :: Word32 -> GameState -> GameState
updateGS d gs1=let
       mvs=gs_moves gs1 + fromIntegral d
       gsInc=changeif shouldSpeed  speedup  gs1{gs_moves=mvs}
       gsMoved=changeif ((0 ==) . mod mvs . gs_movespeed) moveDown gsInc
       alive=gs_lives gsMoved > 0
       gsNew=if alive 
             then changeif ((0==) . mod mvs . gs_newspeed) newChar gsMoved
             else gsMoved
       in gsNew

-- | graphics info
data GraphicsData = GraphicsData {
        gd_font :: TTF.Font
        , gd_mainSurf :: SDL.Surface
        }
 
-- | gamestate
data GameState = GameState {
        gs_moves :: Int
        , gs_movespeed :: Int
        , gs_newspeed :: Int
        , gs_shown :: M.Map Char (Int,Int)
        , gs_rand :: StdGen
        , gs_score :: Int
        , gs_score_beforespeed :: Int
        , gs_lives :: Int
        } 
 
-- | apply the function if the first argument evaluates to true 
changeif :: (a -> Bool) -> (a->a) -> a-> a 
changeif test change obj=if test obj then change obj else obj
 
-- | should we speed up the game?
shouldSpeed :: GameState -> Bool
shouldSpeed GameState{gs_score,gs_score_beforespeed}=(gs_score-gs_score_beforespeed)>5 -- && (mod gs_moves 1000)==0 
 
-- | speed up the rate characters scroll down
speedup :: GameState -> GameState
speedup gs@GameState{gs_score,gs_movespeed,gs_newspeed}=let
        ratio= 0.9::Double -- 1 - (1 / (log $ ((fromIntegral (max gs_score 2))::Double) ^ (2::Int)))
        in gs{gs_movespeed=round (fromIntegral gs_movespeed * ratio),gs_newspeed=round (fromIntegral gs_newspeed * ratio),gs_score_beforespeed=gs_score}

-- | generate a new character on screen
newChar :: GameState -> GameState
newChar gs@GameState{gs_rand,gs_shown}=let
        (c,r')=randomR ('A','Z') gs_rand
        (s',r'')=if not (M.member c gs_shown)
                then 
                        let (x,r2)=randomR (1,div width 10 - 2) r'
                        in (M.insert c (x*10,20) gs_shown,r2)
                else (gs_shown,r')
        in gs{gs_rand=r'',gs_shown=s'}

-- | move characters down    
moveDown :: GameState -> GameState
moveDown gs@GameState{gs_shown,gs_lives}=let
        (dead,s')=M.foldWithKey  (\c (x,y) (b,m)->
                let y'=y+1
                in if y' > (height - 20) then (True, m) else (b, M.insert c (x, y') m)
                ) (False,M.empty) gs_shown
        d'=if dead then gs_lives-1 else gs_lives
        in gs{gs_shown=s',gs_lives=d'}
      
        
width :: Int        
width = 640
    
height :: Int        
height =  480

halfW :: Int
halfW= div width 2

halfH :: Int 
halfH=div height 2

lives :: Int
lives = 3

red :: SDL.Color
red = SDL.Color 255 20 20

black :: SDL.Color
black = SDL.Color 0 0 0

white :: SDL.Color
white=SDL.Color 255 255 255

-- | SDL initialization
initGraphics :: IO GraphicsData
initGraphics = do
        SDL.init [SDL.InitEverything]
        TTF.init
        SDL.enableUnicode True
        SDL.setVideoMode width height 32 []
        SDL.setCaption "TypeClass: the typing game" "TypeClass"
        dd<-getDataDir
        realFont<- TTF.openFont (dd </> "font" </> "FreeSansBold.ttf") 24
        mainSurf <- SDL.getVideoSurface
        return $ GraphicsData realFont mainSurf

-- | SDL finalization
endGraphics :: GraphicsData -> IO()
endGraphics GraphicsData{gd_font=font}=do
        TTF.closeFont font
        TTF.quit
        SDL.quit