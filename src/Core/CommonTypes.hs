{-
Module      :  Core.Types

Description :  A module declaring common types necessary for 
               both implementations of FJ and FFJ.

Copyright   :  Daniela & Pedro & Rodrigo

License     :  <license>

Maintainer  :  abreu223@hotmail.com & daniaangelos@gmail.com
Stability   :  experimental 
Portability :  portable

<module description starting at first column>
-}

module Core.Types where 

class Referable a where 
  ref  :: a -> String
  find :: String -> [a] -> Maybe a
 
  find key list = -- defaul implementation
    case [x | x <- list, key == (ref x)] of
      []    -> Nothing
      (x:_) -> Just x  