{-# LANGUAGE NamedFieldPuns #-}
module Main where


import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF

import Control.Concurrent
import Control.Monad

import Data.Char
import qualified Data.Map as M
import Data.Word
import System.Random

main :: IO ()
main= do
    gd <- initGraphics
    r  <- getStdGen
    let gs = newChar $ GameState 0 100 2000 M.empty r 0 0 lives
    tcks <- SDL.getTicks 
    drawloop gs tcks gd
    endGraphics gd


checkEvent :: GameState -> SDL.Event -> (GameState ,Bool)   
checkEvent gs@GameState{gs_shown,gs_score} sdlEvent = case sdlEvent of
    SDL.Quit       -> (gs, True)
    SDL.KeyDown ks -> let c   = toUpper $ SDL.symUnicode ks
                          gs' = if c `M.member` gs_shown 
                                    then gs { gs_shown = M.delete c gs_shown
                                            , gs_score = gs_score + 1 }
                                    else gs
                       in (gs', False)
    _              -> (gs, False)


drawloop :: GameState -> Word32 -> GraphicsData -> IO ()
drawloop gs oldTicks gd@GraphicsData{ gd_mainSurf } = do
    e <- SDL.pollEvent
    let (gs1, shouldStop) = checkEvent gs e
    -- fugliest hack EVAR, but pretty awesome ;)
    case () of
      _ | shouldStop        -> return ()
        | gs_lives gs1 >  0 -> do
            newTicks <- SDL.getTicks
            let d = newTicks - oldTicks
            -- update game state
            let mvs     = gs_moves gs1 + fromIntegral d
                gsInc   = speedup gs1{ gs_moves = mvs }
                zMove n = mvs `mod` n == 0
                gsMoved = if zMove (gs_movespeed gsInc)
                            then moveDown gsInc
                            else gsInc
                alive   = gs_lives gsMoved > 0
                gsNew   = if alive && zMove (gs_newspeed gsMoved)
                            then newChar gsMoved 
                            else gsMoved

            -- draw screen
            let jRect  = Just (SDL.Rect 0 0 width height)
            SDL.fillRect gd_mainSurf jRect (SDL.Pixel 0)

            if alive
                then do
                    mapM_ (drawChar gd) (M.assocs $ gs_shown gsNew)
                    drawScore gd gsNew
                else do
                    gameOver gd gsNew
            SDL.flip gd_mainSurf                
                 
            newTicks' <- SDL.getTicks
            let d' = newTicks' - oldTicks
            when (d' < 16) (threadDelay (fromIntegral d'))
            drawloop gsNew newTicks gd
        | otherwise -> drawloop gs oldTicks gd



data GraphicsData = GraphicsData
    { gd_font :: TTF.Font
    , gd_mainSurf :: SDL.Surface }
 
data GameState = GameState 
    { gs_moves :: Int
    , gs_movespeed :: Int
    , gs_newspeed :: Int
    , gs_shown :: M.Map Char (Int,Int)
    , gs_rand :: StdGen
    , gs_score :: Int
    , gs_score_beforespeed :: Int
    , gs_lives :: Int } 
 
speedup :: GameState -> GameState
speedup gs@GameState{ gs_moves, gs_score, gs_movespeed
                    , gs_newspeed, gs_score_beforespeed } =
    let speed       = 1 - (1 / (log $ ((fromIntegral gs_score)::Double) ^ (2::Int)))
        moves1000   = gs_moves `mod` 1000 == 0 
        scoreDiff5  = gs_score - gs_score_beforespeed > 5
        (ratio,sc)  = if scoreDiff5 && moves1000
                        then (speed, gs_score) 
                        else (    1, gs_score_beforespeed)
        adjust      = round . (*ratio) . fromIntegral
     in gs{ gs_movespeed         = adjust gs_movespeed
          , gs_newspeed          = adjust gs_newspeed
          , gs_score_beforespeed = sc }


newChar :: GameState -> GameState
newChar gs@GameState{ gs_rand, gs_shown } = 
    let (c ,r')  = randomR ('A','Z') gs_rand
        (s',r'') = if c `M.notMember` gs_shown
                    then let (x,r2) = randomR (1, (width `div` 10) - 2) r'
                          in (M.insert c (x*10,20) gs_shown,r2)
                    else     (gs_shown                     ,r')
     in gs{ gs_rand = r''
          , gs_shown=s'}
    
moveDown :: GameState -> GameState
moveDown gs@GameState{ gs_shown, gs_lives } =
    let (dead,s') = M.foldWithKey foldKeyFun (False, M.empty) gs_shown
        d'        = if dead then gs_lives - 1
                            else gs_lives
     in gs{ gs_shown = s'
          , gs_lives = d' }
  where
    foldKeyFun :: Char -> (Int,Int) -> (Bool, M.Map Char (Int,Int)) -> (Bool, M.Map Char (Int,Int))
    foldKeyFun c (x,y) (b,m)
        | y + 1 > height - 20 = (True,m)
        | otherwise           = (b   ,M.insert c (x,y + 1) m)


drawChar :: GraphicsData -> (Char, (Int,Int)) -> IO ()
drawChar GraphicsData{ gd_font, gd_mainSurf } (c,(x,y)) = do
    let r = Just (SDL.Rect x y 10 10)
    txtS <- TTF.renderUTF8Solid gd_font [c] (SDL.Color 255 255 255)
    SDL.blitSurface txtS Nothing gd_mainSurf r
    SDL.freeSurface txtS


drawScore :: GraphicsData -> GameState -> IO ()
drawScore GraphicsData{ gd_font, gd_mainSurf } 
          GameState{ gs_score, gs_lives } = do
    let half    = width `div` 2
        r1      = Just $ SDL.Rect 0 0 half 10
        r2      = Just $ SDL.Rect (half+1) 0 half 10
        lives   = "Lives:" ++ show gs_lives ++ "/" ++ show lives
        score   = "Score:" ++ show gs_score

    txtS1 <- TTF.renderUTF8Solid gd_font lives (SDL.Color 255 20 20)
    SDL.blitSurface txtS1 Nothing gd_mainSurf r1
    SDL.freeSurface txtS1

    txtS2 <- TTF.renderUTF8Solid gd_font score  (SDL.Color 255 20 20)
    SDL.blitSurface txtS2 Nothing gd_mainSurf r2
    SDL.freeSurface txtS2


gameOver :: GraphicsData -> GameState -> IO ()
gameOver GraphicsData{ gd_font, gd_mainSurf } GameState{ gs_score } = do
    let halfH = height `div` 2
        halfW =  width `div` 2
        x     = halfW - 40
        r1    = Just (SDL.Rect x (halfH-20) 200 10)
        r2    = Just (SDL.Rect x (halfH+10) 200 10)
        score = "Score:" ++ show gs_score

    txtS1 <- TTF.renderUTF8Solid gd_font "Game Over!" (SDL.Color 255 20 20)
    SDL.blitSurface txtS1 Nothing gd_mainSurf r1
    SDL.freeSurface txtS1

    txtS2<-TTF.renderUTF8Solid gd_font score (SDL.Color 255 20 20)
    SDL.blitSurface txtS2 Nothing gd_mainSurf r2
    SDL.freeSurface txtS2


width, height :: Int        
width  = 640
height = 480

lives :: Int
lives = 3


initGraphics :: IO GraphicsData
initGraphics = do
    SDL.init [ SDL.InitEverything ]
    TTF.init
    SDL.enableUnicode True
    SDL.setVideoMode width height 32 []
    SDL.setCaption "TypeClass: the typing game" "TypeClass"
    font <- TTF.openFont "FreeSansBold.ttf" 24
    mainSurf <- SDL.getVideoSurface
    return $ GraphicsData font mainSurf


endGraphics :: GraphicsData -> IO()
endGraphics GraphicsData{gd_font=font}=do
    TTF.closeFont font
    TTF.quit
    SDL.quit


