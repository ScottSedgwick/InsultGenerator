module InsultApp
  ( Message(..)
  , Model
  , init
  , view
  , update
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Flame (Html, (:>))
import Flame.Html.Element as HE
import Flame.Html.Event as HV

import Insults (randomInsult)

type Model =
  { insult :: String
  }

data Message 
  = GetNewInsult
  | ReturnNewInsult String

init ::  Tuple Model (Array (Aff (Maybe Message)))
init = { insult: "" } :> [ pure (Just GetNewInsult) ]

update :: Model → Message → Tuple Model (Array (Aff (Maybe Message)))
update model GetNewInsult = model :> [ Just <<< ReturnNewInsult <$> liftEffect randomInsult ]
update model (ReturnNewInsult s) = model { insult = s } :> []

view :: Model -> Html Message
view model =
  HE.main "main"
  [ HE.article_ 
    [ HE.h1 [ HV.onClick GetNewInsult] [ HE.text model.insult ] 
    ]
  ]