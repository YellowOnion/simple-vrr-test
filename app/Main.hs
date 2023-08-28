{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Data.Monoid
import Data.Maybe
import SDL.Vect
import SDL (($=))
import qualified SDL
import Control.Concurrent (threadDelay)

data Colour = Red | Green | Blue

low = 255 `div` 5
hi  = low * 4

toColor Red = V4 hi low low 0
toColor Green = V4 0 maxBound 0 0
toColor Blue = V4 0 0 maxBound 0

toDelay :: Colour -> Int
toDelay Red = hzToDelay 48
toDelay Green = hzToDelay 90
toDelay Blue = hzToDelay 9999

hzToDelay a = 1000000 `div` a

eventToColor (SDL.KeyboardEvent e)
  | kmotion == SDL.Released = kSym2Color
  | otherwise = mempty
  where
    ksym = SDL.keysymKeycode $ SDL.keyboardEventKeysym e
    kmotion = SDL.keyboardEventKeyMotion e
    kSym2Color
      | ksym == SDL.Keycode1 = Last $ Just Red
      | ksym == SDL.Keycode2 = Last $ Just Green
      | ksym == SDL.Keycode3 = Last $ Just Blue
      | otherwise = mempty
eventToColor _ = mempty

mapEventsToColour = getLast . foldMap eventToColor

main :: IO ()
main = do
  SDL.initialize [ SDL.InitVideo ]
  displays <- SDL.getDisplays
  let
    display     = head $ filter ((== (P $ V2 0 0)) . SDL.displayBoundsPosition) displays
    displaySize = SDL.displayModeSize . head $ SDL.displayModes display
    winConf     = SDL.defaultWindow
        { SDL.windowInitialSize = V2 300 200
        , SDL.windowPosition = SDL.Absolute (P $ V2 0 0)
        , SDL.windowMode = SDL.Windowed
        , SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL
        }
  window <- SDL.createWindow "SDL VRR Test" winConf

  putStrLn $ SDL.displayName display

  SDL.showWindow window
  renderer <-
    SDL.createRenderer
      window
      (-1)
      SDL.RendererConfig
         { SDL.rendererType = SDL.AcceleratedVSyncRenderer
         , SDL.rendererTargetTexture = False
         }

  SDL.rendererDrawColor renderer $= V4 0 0 0 0



  let
    loop color = do
      events <- SDL.pollEvents
      let
        events' = map SDL.eventPayload events
        quit = elem SDL.QuitEvent $ events'
        inputEvent = fromMaybe color $ mapEventsToColour events'
      unless quit $ do
        SDL.rendererDrawColor renderer $= toColor inputEvent
        SDL.fillRect renderer . Just $ SDL.Rectangle (P $ V2 0 0) displaySize
        SDL.present renderer
        threadDelay $ toDelay color
        loop inputEvent

  loop Red

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
