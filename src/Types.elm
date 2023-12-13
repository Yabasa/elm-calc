module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Url exposing (Url)


type Operation
    = Add
    | Subtract
    | Multiply
    | Divide


type Action
    = Negate
    | Equal
    | Dot
    | Clear
    | ClearEntry


type IncFloat
    = Whole Int
    | WholeWithPoint Int
    | Decimal Int Int


type FrontendMsg
    = NumPressed Int
    | OperPressed Operation
    | ActionPressed Action
    | NoOpFrontendMsg


type FrontendModel
    = Cleared
    | InputNum1 IncFloat
    | InputOper IncFloat Operation
    | InputNum2 IncFloat Operation IncFloat
    | Done IncFloat Operation IncFloat Float


type alias BackendModel =
    { message : String
    }




type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend

