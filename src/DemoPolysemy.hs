module DemoPolysemy where

import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Output

f :: Member (Error String) r => Sem r String
f = catch @String (throw "Error") \ e -> pure "Caught"

f2 :: Member (Error String) r => Sem r String
f2 = throw "Error"