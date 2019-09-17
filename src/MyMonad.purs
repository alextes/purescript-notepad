module MyMonad where

import Prelude
import Effect (Effect)
import Effect.Console (log) as Console

data Perhaps a
  = Some a
  | None

perhaps :: forall a b m. Monad m => m a -> (a -> m b) -> m b
perhaps thing f = thing >>= f

instance prehapsFunctor :: Functor Perhaps where
  map f None = None
  map f (Some a) = Some (f a)

instance perhapsApply :: Apply Perhaps where
  apply None _ = None
  apply (Some f) None = None
  apply (Some f) (Some a) = Some (f a)

instance perhapsApplicative :: Applicative Perhaps where
  pure = Some

instance perhapsBind :: Bind Perhaps where
  bind None _ = None
  bind (Some a) f = f a

instance perhapsMonad :: Monad Perhaps

-- bind None _ = None
-- pure = Some
-- bind (Some a) f = f a
main :: Effect Unit
main = case Some 1 >>= (const <<< pure $ "Hiya!") of
  None -> Console.log "empty!"
  Some str -> Console.log str
