module ConstrainingEffects where

import Prelude

import Control.Monad.Reader (ask, runReaderT) as Reader
import Control.Monad.Reader (class MonadAsk, ReaderT)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (logShow) as Console

effectfulAddOne :: Int -> Effect Int
effectfulAddOne = (+) 1 >>> pure

class Monad m <= ConstrainedEffects m where
  addOne :: Int -> m Int
  printInt :: Int -> m Unit

instance constrainedEffectsReaderT :: ConstrainedEffects (ReaderT ctx Effect) where
  addOne num = liftEffect $ effectfulAddOne num
  -- addOne num = ReaderT \_ -> effectfulAddOne num
  printInt num = liftEffect $ Console.logShow num

type AdditionContext = { numberToAdd :: Int }

addOneThenPrint :: forall m. MonadAsk AdditionContext m => ConstrainedEffects m => m Unit
addOneThenPrint = do
  additionContext <- Reader.ask
  sumNum <- addOne additionContext.numberToAdd
  printInt sumNum

main :: Effect Unit
main = Reader.runReaderT addOneThenPrint { numberToAdd: 5 }
