module Main where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF

import Control.Concurrent
import Control.Monad

import Data.Char
import qualified Data.Map as M
import Data.Word
import System.Random

main::IO()
main= do
      gd<-initGraphics
      r<-getStdGen
      gsVar<-newMVar $ newChar $ GameState False 0 100 2000 M.empty r 0 0 lives
      tcks<-SDL.getTicks 
      forkIO (drawloop gsVar tcks gd)
      eventLoop gsVar
      endGraphics gd
      
eventLoop :: MVar GameState -> IO ()
eventLoop gs = SDL.waitEvent >>= (checkEvent gs)

checkEvent :: MVar GameState -> SDL.Event -> IO ()      
checkEvent gsVar SDL.Quit = do
        modifyMVar_ gsVar (\gs->return gs{gs_shouldStop=True}) 
        return ()
checkEvent gsVar (SDL.KeyDown ks)    = do
        let c=toUpper $ SDL.symUnicode ks
        modifyMVar_ gsVar (\gs@GameState{gs_shown,gs_score}->
                return (if M.member c gs_shown
                        then gs{gs_shown=M.delete c gs_shown,gs_score=gs_score+1}
                        else gs)
                ) 
        eventLoop gsVar
checkEvent gs _    = eventLoop gs

drawloop :: MVar GameState
                                    -> Data.Word.Word32
                                    -> GraphicsData
                                    -> IO ()
drawloop gsVar oldTicks gd@GraphicsData{gd_mainSurf}=do
        gs<-readMVar gsVar
        if gs_shouldStop gs 
                then return ()
                else do
                        newTicks<-SDL.getTicks
                        let d=newTicks-oldTicks
                        -- update game state
                        modifyMVar_ gsVar (\gs1@GameState{gs_moves}->do
                                let mvs=gs_moves +(fromIntegral d)
                                let gsInc=speedup gs1{gs_moves=mvs}
                                let gsMoved=if (mod mvs (gs_movespeed gs )) == 0 then moveDown gsInc else gsInc
                                let alive=gs_lives gsMoved > 0
                                SDL.fillRect gd_mainSurf (Just (SDL.Rect 0 0 width height)) (SDL.Pixel 0)
                                let gsNew=if alive 
                                        then if (mod mvs (gs_newspeed gs )) == 0 then newChar gsMoved else gsMoved
                                        else gsMoved{gs_shouldStop=True}
                                return gsNew
                                )
                        -- draw screen
                        withMVar gsVar (\gs1->do
                                let alive=gs_lives gs1> 0
                                SDL.fillRect gd_mainSurf (Just (SDL.Rect 0 0 width height)) (SDL.Pixel 0)
                                if alive 
                                        then do
                                                mapM_ (drawChar gd) (M.assocs $ gs_shown gs)
                                                drawScore gd gs1
                                        else do
                                                gameOver gd gs1
                                SDL.flip gd_mainSurf                
                                )
                        newTicks'<-SDL.getTicks
                        let d'=newTicks'-oldTicks
                        when (d'<16) (threadDelay (fromIntegral d'))
                        drawloop gsVar newTicks gd    
             
        
data GraphicsData = GraphicsData {
        gd_font :: TTF.Font
        , gd_mainSurf :: SDL.Surface
        }
 
data GameState = GameState {
        gs_shouldStop :: Bool
        , gs_moves :: Int
        , gs_movespeed :: Int
        , gs_newspeed :: Int
        , gs_shown :: M.Map Char (Int,Int)
        , gs_rand :: StdGen
        , gs_score :: Int
        , gs_score_beforespeed :: Int
        , gs_lives :: Int
        } 
 
speedup :: GameState -> GameState
speedup gs@GameState{gs_moves,gs_score,gs_movespeed,gs_newspeed,gs_score_beforespeed}=let
        speed=1 - (1 / (log $ ((fromIntegral gs_score)::Double) ^ (2::Int)))
        (ratio,sc)=if (gs_score-gs_score_beforespeed)>5 && (mod gs_moves 1000)==0 then (speed,gs_score) else (1,gs_score_beforespeed)
        in gs{gs_movespeed=round ((fromIntegral gs_movespeed)*ratio),gs_newspeed=round ((fromIntegral gs_newspeed)*ratio),gs_score_beforespeed=sc}


newChar :: GameState -> GameState
newChar gs@GameState{gs_rand,gs_shown}=let
        (c,r')=randomR ('A','Z') gs_rand
        (s',r'')=if not (M.member c gs_shown)
                then 
                        let (x,r2)=randomR (1,(div width 10)-2) r'
                        in (M.insert c (x*10,20) gs_shown,r2)
                else (gs_shown,r')
        in gs{gs_rand=r'',gs_shown=s'}
    
moveDown :: GameState -> GameState
moveDown gs@GameState{gs_shown,gs_lives}=let
        (dead,s')=M.foldWithKey  (\c (x,y) (b,m)->
                let y'=y+1
                in if (y'>(height-20)) 
                        then (True,m)
                        else (b,M.insert c (x,y') m)
                ) (False,M.empty) gs_shown
        d'=if dead then gs_lives-1 else gs_lives
        in gs{gs_shown=s',gs_lives=d'}
      
drawChar :: GraphicsData -> (Char,(Int,Int)) -> IO()
drawChar GraphicsData{gd_font,gd_mainSurf} (c,(x,y))=do
         let r = Just (SDL.Rect x y 10 10)
         txtS<-TTF.renderUTF8Solid gd_font [c] (SDL.Color 255 255 255)
         SDL.blitSurface txtS Nothing gd_mainSurf r
         SDL.freeSurface txtS
   
drawScore :: GraphicsData -> GameState -> IO()
drawScore GraphicsData{gd_font,gd_mainSurf} GameState{gs_score,gs_lives}=do
        let half= (div width 2)
        let r1 = Just (SDL.Rect 0 0 half 10)
        let r2 = Just (SDL.Rect (half+1) 0 half 10)
        txtS1<-TTF.renderUTF8Solid gd_font ("Lives:" ++ (show gs_lives) ++ "/" ++ (show lives)) (SDL.Color 255 20 20)
        SDL.blitSurface txtS1 Nothing gd_mainSurf r1
        SDL.freeSurface txtS1
        txtS2<-TTF.renderUTF8Solid gd_font ("Score:" ++ show gs_score) (SDL.Color 255 20 20)
        SDL.blitSurface txtS2 Nothing gd_mainSurf r2
        SDL.freeSurface txtS2
     
gameOver :: GraphicsData -> GameState -> IO()
gameOver GraphicsData{gd_font,gd_mainSurf} GameState{gs_score}=do
        let halfH=(div height 2)
        let halfW=(div width 2)
        let x=halfW-40
        let r1 = Just (SDL.Rect x (halfH-20) 200 10)
        let r2 = Just (SDL.Rect x (halfH+10) 200 10)
        txtS1<-TTF.renderUTF8Solid gd_font ("Game Over!") (SDL.Color 255 20 20)
        SDL.blitSurface txtS1 Nothing gd_mainSurf r1
        SDL.freeSurface txtS1
        txtS2<-TTF.renderUTF8Solid gd_font ("Score:" ++ show gs_score) (SDL.Color 255 20 20)
        SDL.blitSurface txtS2 Nothing gd_mainSurf r2
        SDL.freeSurface txtS2
        
width :: Int        
width = 640
    
height :: Int        
height =  480

lives :: Int
lives = 3

initGraphics :: IO(GraphicsData)
initGraphics = do
        SDL.init [SDL.InitEverything]
        TTF.init
        SDL.enableUnicode True
        SDL.setVideoMode width height 32 []
        SDL.setCaption "TypeClass: the typing game" "TypeClass"
        font<-TTF.openFont "FreeSansBold.ttf" 24
        mainSurf <- SDL.getVideoSurface
        return $ GraphicsData font mainSurf

endGraphics :: GraphicsData -> IO()
endGraphics GraphicsData{gd_font=font}=do
        TTF.closeFont font
        TTF.quit
        SDL.quit