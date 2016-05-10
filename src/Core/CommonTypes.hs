{-
Module      :  Core.Types

Description :  A module declaring common types necessary for 
               both implementations of FJ and FFJ.

Copyright   :  Daniella & Pedro & Rodrigo

License     :  <license>

Maintainer  :  abreu223@hotmail.com & daniaangelos@gmail.com
Stability   :  experimental 
Portability :  portable

<module description starting at first column>
-}

module Core.CommonTypes where 


import Control.Monad(liftM, ap) 
import Control.Applicative

class Referable a where 
  ref  :: a -> String
  find :: String -> [a] -> Result a
 
  find key list = -- defaul implementation
    case [x | x <- list, key == (ref x)] of
      []    -> raise $ "there is no such a key " ++ key ++ " in the list."
      (x:_) -> Ok x  

data Result a = Ok a
	| Ex Exception
	deriving(Eq, Show, Ord)

raise :: Exception -> Result a
raise x = Ex x 

instance Monad Result where
	return a = Ok a
	{-(>>=) :: MOut a -> (a -> MOut b) -> MOut b-}
	(>>=) m f = case m of 
				(Ok a) -> f a
				(Ex e) -> raise e

-- Note: in more recent versions of GHC, every 
-- monad instance must also be declared as an 
-- instance of both Functor and Applicative 
-- type classes. Sure, this is a little boring. 
 
instance Functor Result where
	 fmap = liftM

instance Applicative Result where 
	 pure = return
	 (<*>) = ap

type Exception = String
