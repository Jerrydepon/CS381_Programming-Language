--
-- Semantics of arithmetic expressions with potential errors
--
module ExprSemM where


-- using monads to avstract from boilerplate error handling
--
import Control.Monad (liftM2,join)


data Expr = N Int
          | Plus Expr Expr
          | Neg Expr
          | Div Expr Expr
          deriving Show

-- Arithmetic expressions are evaluated to Int.
-- To account for errors, the semantic domain is
-- defined as Maybe Int.
--
type D = Maybe Int


-- The semantics is defined as a function that maps
-- expressions of type Expr to D. Normal results (i.e.
-- integers) are wrapped by the Just constructor to
-- be of type D. Just 'injects' integers into the
-- Maybe type. Error values are represented by the
-- constructor Nothing.
--
-- sem :: Expr -> D
sem :: Expr -> Maybe Int
sem (N i)       = Just i
sem (Neg e)     = fmap ((-1)*) (sem e)
sem (Plus e e') = liftM2 (+) (sem e) (sem e')
sem (Div e e')  = join (liftM2 safeDiv (sem e) (sem e'))
                  where safeDiv _ 0 = Nothing
                        safeDiv i j = Just (i `div` j)


-- examples
--
e1 = Plus (N 3) (N 4)

e2 = Div (N 21) e1

e3 = Div e2 (Plus e1 (Neg (N 7)))

results = map sem [e1,e2,e3,Plus e2 e3]
