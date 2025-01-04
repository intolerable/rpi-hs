module Main where

import Foreign.C
import Foreign.C.ConstPtr

import LibGPIOD

main :: IO ()
main = do
  v <- gpiod_version_string >>= peekCString . unConstPtr
  putStrLn $ "libgpiod version: " <> v
