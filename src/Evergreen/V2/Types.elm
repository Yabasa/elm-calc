module Evergreen.V2.Types exposing (..)


type IncFloat
    = Whole Int
    | WholeWithPoint Int
    | Decimal Int Int


type Operation
    = Add
    | Subtract
    | Multiply
    | Divide


type FrontendModel
    = Cleared
    | InputNum1 IncFloat
    | InputOper IncFloat Operation
    | InputNum2 IncFloat Operation IncFloat
    | Done IncFloat Operation IncFloat Float


type alias BackendModel =
    { message : String
    }


type Action
    = Negate
    | Equal
    | Dot
    | Clear
    | ClearEntry


type FrontendMsg
    = NumPressed Int
    | OperPressed Operation
    | ActionPressed Action
    | NoOpFrontendMsg


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
