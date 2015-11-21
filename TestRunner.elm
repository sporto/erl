module Main where

import Signal exposing (Signal)
import Task

import Console
import ElmTest exposing (..)

import Tests

port runner : Signal (Task.Task x ())
port runner =
  Console.run (consoleRunner Tests.all)
