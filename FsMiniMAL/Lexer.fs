module Lexer
 
open System
open System.Text
open Parser
open FsMiniMAL.Misc
open FsMiniMAL.LexHelper

let private main_asciiAlphabetTable = [| 40us; 40us; 40us; 40us; 40us; 40us; 40us; 40us; 40us; 0us; 1us; 40us; 0us; 0us; 40us; 40us; 40us; 40us; 40us; 40us; 40us; 40us; 40us; 40us; 40us; 40us; 40us; 40us; 40us; 40us; 40us; 40us; 0us; 2us; 3us; 40us; 40us; 4us; 5us; 6us; 7us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 17us; 17us; 17us; 17us; 17us; 18us; 18us; 19us; 20us; 21us; 22us; 23us; 24us; 25us; 26us; 27us; 26us; 26us; 28us; 26us; 29us; 29us; 29us; 29us; 29us; 29us; 29us; 29us; 30us; 29us; 29us; 29us; 29us; 29us; 29us; 29us; 29us; 31us; 29us; 29us; 32us; 40us; 33us; 34us; 35us; 40us; 26us; 27us; 26us; 26us; 28us; 26us; 29us; 29us; 29us; 29us; 29us; 29us; 29us; 29us; 30us; 29us; 29us; 29us; 29us; 29us; 29us; 29us; 29us; 31us; 29us; 29us; 36us; 37us; 38us; 39us; 40us |]
let private main_nonAsciiCharRangeTable = [| 128us |]
let private main_nonAsciiAlphabetTable = [| 40us |]
let private main_transitionTable =
    [|
        [| 2s; 1s; 12s; 6s; 32s; 10s; 8s; 9s; 13s; 14s; 30s; 15s; 16s; 17s; 31s; 4s; 5s; 5s; 5s; 18s; 19s; 20s; 21s; 28s; 33s; 7s; 3s; 3s; 3s; 3s; 3s; 3s; 22s; 23s; 29s; 24s; 25s; 11s; 26s; 27s; 35s; 34s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| 36s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; 37s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 37s; 37s; 37s; 37s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 37s; 37s; 37s; 37s; 37s; 37s; -1s; -1s; -1s; 37s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 44s; -1s; 38s; 38s; 38s; 38s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 42s; 43s; -1s; 41s; 40s; -1s; -1s; -1s; 39s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 44s; -1s; 38s; 38s; 38s; 38s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 43s; -1s; -1s; -1s; -1s; -1s; -1s; 39s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; 45s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 46s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; 47s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 49s; -1s; -1s; -1s; 48s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 50s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 52s; -1s; -1s; -1s; 51s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 54s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 53s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 55s; -1s; -1s; 56s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 57s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 58s; 50s; 50s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 59s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 60s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 61s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 62s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 50s; 63s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 64s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 65s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 51s; 66s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| 36s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; 37s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 37s; 37s; 37s; 37s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 37s; 37s; 37s; 37s; 37s; 37s; -1s; -1s; -1s; 37s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 44s; -1s; 38s; 38s; 38s; 38s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 43s; -1s; -1s; -1s; -1s; -1s; -1s; 39s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 38s; 38s; 38s; 38s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 39s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 67s; 67s; 67s; 67s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 67s; 67s; 67s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 68s; 68s; 68s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 69s; 69s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 71s; -1s; 71s; -1s; -1s; 70s; 70s; 70s; 70s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 72s; 72s; 72s; 72s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 43s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; 65s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 65s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 51s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 73s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 73s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 51s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 65s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 74s; 74s; 74s; 74s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 74s; 74s; 74s; -1s; -1s; -1s; -1s; -1s; -1s; 75s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 76s; 76s; 76s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 77s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 78s; 78s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 79s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 80s; 80s; 80s; 80s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 81s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 70s; 70s; 70s; 70s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 82s; 82s; 82s; 82s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 43s; -1s; -1s; -1s; -1s; -1s; -1s; 83s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 74s; 74s; 74s; 74s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 74s; 74s; 74s; -1s; -1s; -1s; -1s; -1s; -1s; 75s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 74s; 74s; 74s; 74s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 74s; 74s; 74s; -1s; -1s; -1s; -1s; -1s; -1s; 75s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 76s; 76s; 76s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 77s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 76s; 76s; 76s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 77s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 78s; 78s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 79s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 78s; 78s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 79s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 80s; 80s; 80s; 80s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 81s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 80s; 80s; 80s; 80s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 81s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 82s; 82s; 82s; 82s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 43s; -1s; -1s; -1s; -1s; -1s; -1s; 83s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 82s; 82s; 82s; 82s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 83s; -1s; -1s; -1s; -1s; -1s; -1s |]
    |]
let private main_acceptTable = [| -1s; 0s; 1s; 2s; 3s; 3s; 5s; 37s; 7s; 12s; 44s; 31s; 11s; 13s; 14s; 15s; 34s; 17s; 18s; 20s; 36s; 24s; 26s; 28s; 29s; 30s; 33s; 44s; 36s; 37s; 38s; 39s; 39s; 42s; 43s; 44s; 1s; 2s; 3s; -1s; -1s; -1s; -1s; -1s; 4s; 6s; 8s; 9s; 10s; 32s; 36s; 39s; 40s; 16s; 34s; 19s; 22s; 21s; 23s; 25s; 27s; 35s; -1s; -1s; 37s; 38s; 41s; 3s; 3s; 3s; 4s; -1s; 4s; 35s; 3s; -1s; 3s; -1s; 3s; -1s; 4s; -1s; 4s; -1s |]
let private main_tables = FsLexYaccLite.Lexing.UnicodeTables(main_asciiAlphabetTable, main_nonAsciiCharRangeTable, main_nonAsciiAlphabetTable, main_transitionTable, main_acceptTable)
let private comment_asciiAlphabetTable = [| 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 0us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 1us; 2us; 3us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us |]
let private comment_nonAsciiCharRangeTable = [| 128us |]
let private comment_nonAsciiAlphabetTable = [| 4us |]
let private comment_transitionTable =
    [|
        [| 1s; 2s; 5s; 3s; 5s; 4s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; 6s; -1s; -1s |]
        [| -1s; -1s; 7s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s |]
    |]
let private comment_acceptTable = [| -1s; 0s; 4s; 4s; 3s; 4s; 1s; 2s |]
let private comment_tables = FsLexYaccLite.Lexing.UnicodeTables(comment_asciiAlphabetTable, comment_nonAsciiCharRangeTable, comment_nonAsciiAlphabetTable, comment_transitionTable, comment_acceptTable)
let private eol_comment_asciiAlphabetTable = [| 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 0us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us |]
let private eol_comment_nonAsciiCharRangeTable = [| 128us |]
let private eol_comment_nonAsciiAlphabetTable = [| 1us |]
let private eol_comment_transitionTable =
    [|
        [| 1s; 3s; 2s |]
        [| -1s; -1s; -1s |]
        [| -1s; -1s; -1s |]
        [| -1s; -1s; -1s |]
    |]
let private eol_comment_acceptTable = [| -1s; 0s; 1s; 2s |]
let private eol_comment_tables = FsLexYaccLite.Lexing.UnicodeTables(eol_comment_asciiAlphabetTable, eol_comment_nonAsciiCharRangeTable, eol_comment_nonAsciiAlphabetTable, eol_comment_transitionTable, eol_comment_acceptTable)
let private char_or_var_asciiAlphabetTable = [| 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 0us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 1us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 3us; 3us; 3us; 3us; 3us; 3us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 10us; 5us; 10us; 10us; 6us; 10us; 3us; 7us; 3us; 3us; 3us; 3us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 8us; 4us; 4us; 4us; 8us; 4us; 8us; 9us; 4us; 4us; 4us; 4us; 4us; 10us; 10us; 10us; 10us; 10us |]
let private char_or_var_nonAsciiCharRangeTable = [| 128us |]
let private char_or_var_nonAsciiAlphabetTable = [| 10us |]
let private char_or_var_transitionTable =
    [|
        [| 1s; 5s; 2s; 3s; 3s; 4s; 2s; 3s; 3s; 3s; 2s; 5s |]
        [| -1s; 6s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; 7s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; 8s; 9s; 9s; 9s; -1s; 9s; 9s; 9s; 9s; -1s; -1s |]
        [| -1s; 10s; 11s; -1s; -1s; 10s; -1s; 10s; 10s; 12s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; 9s; 9s; 9s; 9s; -1s; 9s; 9s; 9s; 9s; -1s; -1s |]
        [| -1s; 9s; 9s; 9s; 9s; -1s; 9s; 9s; 9s; 9s; -1s; -1s |]
        [| -1s; 13s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; 14s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; 15s; 15s; -1s; -1s; -1s; 15s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; 16s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; 17s; 17s; -1s; -1s; -1s; 17s; -1s; -1s; -1s; -1s |]
        [| -1s; 18s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; 19s; 19s; -1s; -1s; -1s; 19s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; 20s; 20s; -1s; -1s; -1s; 20s; -1s; -1s; -1s; -1s |]
        [| -1s; 21s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
    |]
let private char_or_var_acceptTable = [| -1s; 6s; 6s; 5s; 6s; 6s; 0s; 1s; 1s; 5s; -1s; -1s; -1s; 2s; -1s; -1s; -1s; -1s; 3s; -1s; -1s; 4s |]
let private char_or_var_tables = FsLexYaccLite.Lexing.UnicodeTables(char_or_var_asciiAlphabetTable, char_or_var_nonAsciiCharRangeTable, char_or_var_nonAsciiAlphabetTable, char_or_var_transitionTable, char_or_var_acceptTable)
let private string_asciiAlphabetTable = [| 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 0us; 1us; 10us; 10us; 2us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 0us; 10us; 3us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 5us; 5us; 5us; 5us; 5us; 5us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 6us; 10us; 10us; 10us; 10us; 5us; 7us; 5us; 5us; 5us; 5us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 8us; 10us; 10us; 10us; 8us; 10us; 8us; 9us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us |]
let private string_nonAsciiCharRangeTable = [| 128us |]
let private string_nonAsciiAlphabetTable = [| 10us |]
let private string_transitionTable =
    [|
        [| 5s; 1s; 5s; 2s; 5s; 5s; 3s; 5s; 5s; 5s; 5s; 4s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; 6s; 7s; 8s; 9s; -1s; 8s; 8s; 8s; 10s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| 11s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| 11s; 6s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; 12s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; 13s; 13s; -1s; 13s; -1s; -1s; -1s; -1s |]
        [| 11s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; 14s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; 15s; 15s; -1s; 15s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; 16s; 16s; -1s; 16s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; 17s; 17s; -1s; 17s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
    |]
let private string_acceptTable = [| -1s; 0s; 1s; 7s; 6s; 7s; 2s; 2s; 3s; -1s; -1s; 2s; -1s; -1s; 4s; -1s; -1s; 5s |]
let private string_tables = FsLexYaccLite.Lexing.UnicodeTables(string_asciiAlphabetTable, string_nonAsciiCharRangeTable, string_nonAsciiAlphabetTable, string_transitionTable, string_acceptTable)
let private verbatim_string_asciiAlphabetTable = [| 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 0us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 1us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us |]
let private verbatim_string_nonAsciiCharRangeTable = [| 128us |]
let private verbatim_string_nonAsciiAlphabetTable = [| 2us |]
let private verbatim_string_transitionTable =
    [|
        [| 1s; 2s; 4s; 3s |]
        [| -1s; -1s; -1s; -1s |]
        [| -1s; 5s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s |]
    |]
let private verbatim_string_acceptTable = [| -1s; 0s; 2s; 3s; 4s; 1s |]
let private verbatim_string_tables = FsLexYaccLite.Lexing.UnicodeTables(verbatim_string_asciiAlphabetTable, verbatim_string_nonAsciiCharRangeTable, verbatim_string_nonAsciiAlphabetTable, verbatim_string_transitionTable, verbatim_string_acceptTable)
let rec main lexbuf =
    match main_tables.Interpret(lexbuf) with
    | 0 ->
        lexbuf.NewLine()
        main lexbuf 
    | 1 ->
        main lexbuf 
    | 2 ->
        ident_or_keyword lexbuf 
    | 3 ->
        make_int lexbuf 
    | 4 ->
        make_float lexbuf 
    | 5 ->
        string lexbuf.StartPos (StringBuilder()) lexbuf 
    | 6 ->
        verbatim_string lexbuf.StartPos (StringBuilder()) lexbuf 
    | 7 ->
        char_or_var lexbuf.StartPos lexbuf 
    | 8 ->
        comment lexbuf.StartPos 1 lexbuf 
    | 9 ->
        AMPAMP 
    | 10 ->
        BARBAR 
    | 11 ->
        EXCLAMATION 
    | 12 ->
        LPAREN 
    | 13 ->
        RPAREN 
    | 14 ->
        STAR 
    | 15 ->
        COMMA 
    | 16 ->
        MINUSGREATER 
    | 17 ->
        DOT 
    | 18 ->
        COLON 
    | 19 ->
        COLONCOLON 
    | 20 ->
        SEMI 
    | 21 ->
        LESSMINUS 
    | 22 ->
        COLONEQUAL 
    | 23 ->
        LESSLESS 
    | 24 ->
        EQUAL 
    | 25 ->
        EQUALEQUAL 
    | 26 ->
        LBRACKET 
    | 27 ->
        LBRACKETBAR 
    | 28 ->
        RBRACKET 
    | 29 ->
        UNDERSCORE 
    | 30 ->
        LBRACE 
    | 31 ->
        BAR 
    | 32 ->
        BARRBRACKET 
    | 33 ->
        RBRACE 
    | 34 ->
        SUBTRACTIVE (lexeme_string lexbuf) 
    | 35 ->
        UNARY (lexeme_string lexbuf) 
    | 36 ->
        COMPARE (lexeme_string lexbuf) 
    | 37 ->
        CONCAT (lexeme_string lexbuf) 
    | 38 ->
        ADDITIVE (lexeme_string lexbuf) 
    | 39 ->
        MULTDIV (lexeme_string lexbuf) 
    | 40 ->
        STARSTAR 
    | 41 ->
        eol_comment lexbuf.StartPos lexbuf 
    | 42 ->
        QMARK 
    | 43 ->
        EOF 
    | 44 ->
        raise (Lexical_error Illegal_character) 
    | _ -> failwith "main"
and comment start_pos depth lexbuf =
    match comment_tables.Interpret(lexbuf) with
    | 0 ->
        lexbuf.NewLine()
        comment start_pos depth lexbuf 
    | 1 ->
        comment start_pos (depth + 1) lexbuf 
    | 2 ->
        if depth = 1 then
          mark_as_comments lexbuf start_pos lexbuf.EndPos
          main lexbuf
        else
          comment start_pos (depth - 1) lexbuf 
    | 3 ->
        mark_as_comments lexbuf start_pos lexbuf.StartPos
        raise(Lexical_error Unterminated_comment) 
    | 4 ->
        comment start_pos depth lexbuf 
    | _ -> failwith "comment"
and eol_comment start lexbuf =
    match eol_comment_tables.Interpret(lexbuf) with
    | 0 ->
        lexbuf.NewLine(); mark_as_comments lexbuf start lexbuf.EndPos; main lexbuf 
    | 1 ->
        mark_as_comments lexbuf start lexbuf.StartPos; EOF 
    | 2 ->
        eol_comment start lexbuf 
    | _ -> failwith "eol_comment"
and char_or_var start_pos lexbuf =
    match char_or_var_tables.Interpret(lexbuf) with
    | 0 ->
        lexbuf.NewLine()
        lexbuf.StartPos <- start_pos;
        CHAR (lexbuf.Lexeme.[0]) 
    | 1 ->
        lexbuf.StartPos <- start_pos;
        CHAR (lexbuf.Lexeme.[0]) 
    | 2 ->
        lexbuf.StartPos <- start_pos;
        CHAR ((char_for_backslash (lexbuf.Lexeme.[1]))) 
    | 3 ->
        lexbuf.StartPos <- start_pos;
        CHAR (char_for_dec3_code lexbuf) 
    | 4 ->
        lexbuf.StartPos <- start_pos;
        CHAR (char_for_hex4_code lexbuf) 
    | 5 ->
        lexbuf.StartPos <- start_pos;
        QUOTED (lexbuf.Lexeme) 
    | 6 ->
        raise (Lexical_error Illegal_character) 
    | _ -> failwith "char_or_var"
and string start_pos buf lexbuf =
    match string_tables.Interpret(lexbuf) with
    | 0 ->
        lexbuf.NewLine()
        (buf : StringBuilder).Add('\n')
        string start_pos buf lexbuf 
    | 1 ->
        lexbuf.StartPos <- start_pos
        STRING ((buf : StringBuilder).ToString()) 
    | 2 ->
        string start_pos buf lexbuf 
    | 3 ->
        (buf : StringBuilder).Add(char_for_backslash(lexbuf.Lexeme.[1]))
        string start_pos buf lexbuf 
    | 4 ->
        (buf : StringBuilder).Add(char_for_dec3_code lexbuf);
        string start_pos buf lexbuf 
    | 5 ->
        (buf : StringBuilder).Add(char_for_hex4_code lexbuf)
        string start_pos buf lexbuf 
    | 6 ->
        lexbuf.StartPos <- start_pos
        raise (Lexical_error Unterminated_string) 
    | 7 ->
        (buf : StringBuilder).Add(lexbuf.Lexeme.[0]);
        string start_pos buf lexbuf 
    | _ -> failwith "string"
and verbatim_string start_pos buf lexbuf =
    match verbatim_string_tables.Interpret(lexbuf) with
    | 0 ->
        lexbuf.NewLine()
        (buf : StringBuilder).Add('\n')
        verbatim_string start_pos buf lexbuf 
    | 1 ->
        (buf : StringBuilder).Add('\034')
        verbatim_string start_pos buf lexbuf 
    | 2 ->
        lexbuf.StartPos <- start_pos
        STRING ((buf : StringBuilder).ToString()) 
    | 3 ->
        lexbuf.StartPos <- start_pos;
        raise (Lexical_error Unterminated_string) 
    | 4 ->
        (buf : StringBuilder).Add(lexbuf.Lexeme.[0])
        verbatim_string start_pos buf lexbuf 
    | _ -> failwith "verbatim_string"

