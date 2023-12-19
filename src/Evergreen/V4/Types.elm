module Evergreen.V4.Types exposing (..)


type FrontendModel
    = Cleared
    | Input String


type alias BackendModel =
    { message : String
    }


type Action
    = Clear
    | Backspace
    | ParenWrap
    | Negate


type FrontendMsg
    = SymbolPressed String
    | ActionPressed Action
    | NoOpFrontendMsg


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
