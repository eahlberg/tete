module Tete.Prelude
  ( module Export,
    tshow,
    tputStrLn,
  )
where

import Control.Applicative as Export
import Control.Monad as Export
import Control.Monad.Extra as Export (whenJust)
import Data.Functor as Export
import Data.List.NonEmpty
import Data.List.NonEmpty as Export (NonEmpty)
import Data.Maybe as Export (mapMaybe)
import Data.Set as Export (Set)
import Data.Text as Export (Text, intercalate, pack, unpack)
import Data.Time as Export (UTCTime, getCurrentTime)
import GHC.Generics as Export (Generic)
import Prelude as Export

tshow :: (Show a) => a -> Text
tshow = pack . show

tputStrLn :: Text -> IO ()
tputStrLn = putStrLn . unpack
