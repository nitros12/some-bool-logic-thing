-- | All transformations we can do

module Transform where

import           AST
import           Data.Generics.Uniplate.Operations
                                                ( transform )
import           Control.Monad.Writer.Lazy

reduce :: Logic -> Logic
reduce (Or  (Literal True)  _              ) = Literal True
reduce (Or  _               (Literal True) ) = Literal True

reduce (Or  (Literal False) a              ) = a
reduce (Or  a               (Literal False)) = a

reduce (And (Literal True)  a              ) = a
reduce (And a               (Literal True) ) = a

reduce (And (Literal False) _              ) = Literal False
reduce (And _               (Literal False)) = Literal False

reduce (Or a b) | a == b        = a

reduce (And a b) | a == b       = a

reduce (Not (Not a))            = a

reduce (Or a (Not b)) | a == b  = Literal True
reduce (Or (Not b) a) | a == b  = Literal True

reduce (And a (Not b)) | a == b = Literal False
reduce (And (Not b) a) | a == b = Literal False

-- demorgans
reduce (Not (Or a b)   )        = And (Not a) (Not b)
-- reduce (Not (And a b)        )               = Or (Not a) (Not b)

-- associative
reduce (And (And a b) c)        = And a (And b c)
reduce (Or  (Or  a b) c)        = Or a (Or b c)

-- distributive
reduce (And (Or a b) (Or c d)) | a == c = Or a (And b d) -- (a + b)(a + d) -> a + bd
reduce (And (Or a b) (Or c d)) | a == d = Or a (And b c) -- (a + b)(c + a) -> a + bc
reduce (And (Or a b) (Or c d)) | b == c = Or b (And a d) -- (a + b)(b + d) -> b + ad
reduce (And (Or a b) (Or c d)) | b == d = Or a (And b d) -- (a + b)(c + b) -> b + ac

reduce (Or (And a b) (And c d)) | a == c = And a (Or b d) -- ab + ad -> a(b + d)
reduce (Or (And a b) (And c d)) | a == d = And a (Or b c) -- ab + ca -> a(b + c)
reduce (Or (And a b) (And c d)) | b == c = And b (Or a d) -- ab + bd -> b(a + d)
reduce (Or (And a b) (And c d)) | b == d = And a (Or b d) -- ab + cb -> b(a + c)

-- reduce (And a         (Or b c)) = And (Or a b) (Or b c)
-- reduce (And (Or b c)  a       ) = And (Or a b) (Or b c)

reduce a                        = a

apReduce :: Logic -> Writer [Logic] Logic
apReduce x = do
  tell [x]
  let x' = transform reduce x
  if x /= x' then apReduce x' else pure x


logTransforms :: Logic -> [Logic]
logTransforms = execWriter . apReduce

