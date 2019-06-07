-- | All transformations we can do

module Transform where

import           AST
import           Data.Generics.Uniplate.Operations
                                                ( transform )
import           Control.Monad.Writer.Lazy

reduce :: Logic -> Logic
reduce (Literal True  `Or`  _            ) = Literal True
reduce (_             `Or`  Literal True ) = Literal True

reduce (Literal False `Or`  a            ) = a
reduce (a             `Or`  Literal False) = a

reduce (Literal True  `And` a            ) = a
reduce (a             `And` Literal True ) = a

reduce (Literal False `And` _            ) = Literal False
reduce (_             `And` Literal False) = Literal False

reduce (a `Or` b) | a == b             = a

reduce (a `And` b) | a == b            = a

reduce (Not (Not a))                   = a

reduce (a `Or` Not b) | a == b         = Literal True
reduce (Not b `Or` a) | a == b         = Literal True

reduce (a `And` Not b) | a == b        = Literal False
reduce (Not b `And` a) | a == b        = Literal False

-- demorgans
-- reduce (Not a `Or` Not b             ) = Not (a `And` b)
reduce (Not (a `Or` b)               ) = Not a `And` Not b

-- reduce (Not a `And` Not b            ) = Not (a `Or` b)
reduce (Not (a `And` b)              ) = Not a `Or` Not b

-- associative
reduce ((a `And` b) `And` c          ) = a `And` (b `And` c)
reduce ((a `Or`  b) `Or`  c          ) = a `Or` (b `Or` c)

-- distributive
reduce ((a `Or` b) `And` (c `Or` d)) | a == c = a `Or` (b `And` d) -- (a + b)(a + d) -> a + bd
reduce ((a `Or` b) `And` (c `Or` d)) | a == d = a `Or` (b `And` c) -- (a + b)(c + a) -> a + bc
reduce ((a `Or` b) `And` (c `Or` d)) | b == c = b `Or` (a `And` d) -- (a + b)(b + d) -> b + ad
reduce ((a `Or` b) `And` (c `Or` d)) | b == d = a `Or` (b `And` d) -- (a + b)(c + b) -> b + ac

reduce ((a `And` b) `Or` (c `And` d)) | a == c = a `And` (b `Or` d) -- ab + ad -> a(b + d)
reduce ((a `And` b) `Or` (c `And` d)) | a == d = a `And` (b `Or` c) -- ab + ca -> a(b + c)
reduce ((a `And` b) `Or` (c `And` d)) | b == c = b `And` (a `Or` d) -- ab + bd -> b(a + d)
reduce ((a `And` b) `Or` (c `And` d)) | b == d = a `And` (b `Or` d) -- ab + cb -> b(a + c)

reduce a                               = a

apReduce :: Logic -> Writer [Logic] Logic
apReduce x = do
  tell [x]
  let x' = transform reduce x
  -- let x' = transform reduce x
  if x /= x' then apReduce x' else pure x


logTransforms :: Logic -> [Logic]
logTransforms = execWriter . apReduce

