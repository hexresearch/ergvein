Small library that implements "back" button feature for reflex-dom widgets.
The core idea of the library is to have reusable widget that maintains history
of switches and is decoupled from actual implementation of switching widgets.

The library doesn't use platform specific parts like connection to Android
system back button or browser back button, but provides functions to bind them
into your retractable frontend.

Example of usage:

``` haskell
import Control.Monad
import Reflex.Dom
import Reflex.Dom.Retractable

main :: IO ()
main = mainWidget $ runRetract frontend

frontend :: (MonadWidget t m, MonadRetract t m) => m ()
frontend = void $ retractStack $ pageA 42

pageA :: (MonadWidget t m, MonadRetract t m) => Int -> m ()
pageA n = do
   e <- button "Go page B"
   void $ nextWidget $ ffor e $ const Retractable {
       retractableNext = pageB $ n + 1
     , retractablePrev = Just $ pure $ pageA n
     }

pageB :: (MonadWidget t m, MonadRetract t m) => Int -> m ()
pageB n = do
  e <- button "Go page A"
  void $ nextWidget $ ffor e $ const  Retractable {
       retractableNext = pageA $ n + 1
     , retractablePrev = Just $ pure $ pageB n
     }
```
