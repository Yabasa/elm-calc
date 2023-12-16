module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Url exposing (Url)


type Action
    = Clear
    | Backspace
    | Equals


type FrontendMsg
    = SymbolPressed String
    | ActionPressed Action
    | NoOpFrontendMsg


type FrontendModel
    = Cleared
    | Input String
    | Done String


type alias BackendModel =
    { message : String
    }


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend

