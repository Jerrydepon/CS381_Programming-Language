-- CS381, HW1
-- Name: 1. Han-Yu Wu
--       2. Po-Ying Chao       
--       3. Jui-Hung Lu
--       4. Chi Wen
--       5. Chih-Hsiang Wang

module HW1 where
--Disregard built in name "Num"
import Prelude hiding (Num)
type Name = String
type Num = Int

-- ==================== Exercise 1 ====================
{- 1.(a) Define the abstract syntax for Mini Logo as a Haskell data type. -}

-- NOTE: Name is defined at line 11
-- NOTE: Num is defined at line 12

data Cmd = Pen Mode
		| Moveto Pos Pos
		| Def Name Pars Cmd
		| Call Name Vals
		| Seq [Cmd]
		deriving Show

data Mode = Up
		| Down
		deriving Show

data Pos = R1 Num
		| R2 Name
		deriving Show

type Pars = [Name]
type Vals = [Num]


{- 1.(b) Write a Mini Logo macro vector that draws a line from a given position (x1,y1) to a given position (x2,y2)
and represent the macro in abstract syntax, that is, as a Haskell data type value. -}

-- Def Name Pars Cmd
vector = Def "vector" ["x1", "y1", "x2", "y2"] (Seq [
														Pen Up,
														Moveto (R2 "x1") (R2 "y1"),
														Pen Down,
														Moveto (R2 "x2") (R2 "y2"),
														Pen Up
													]
												)


{- 1.(c) Define a Haskell function steps :: Int -> Cmd that constructs a Mini Logo program which draws a stair of n
steps. Your solution should not use the macro vector. -}

steps :: Int -> Cmd
steps 0 = Seq [Pen Up, Moveto (R2 "0") (R2 "0")]
steps 1 = Seq [
				steps 0,

					Pen Down,
					Moveto (R2 "0") (R2 "1"),
					Moveto (R2 "1") (R2 "1"),
					Pen Up

			]

steps n = Seq [
				steps (n-1),

					Pen Down,
					Moveto (R2 "n-1") (R2 "n"),
					Moveto (R2 "n") (R2 "n"),
					Pen Up

			]


-- ==================== Exercise 2 ====================
{- 2.(a) Define the abstract syntax for the above language as a Haskell data type. -}

-- NOTE: Num is defined at line 12

data Circuit = Con Gates Links
            deriving Show

data Gates = GatesCon [(Num, GateFN)] | NoGate
            deriving Show

data GateFN = And
            | Or
            | Xor
            | Not
            deriving Show

data Links = LinksCon [((Num, Num), (Num, Num))] | NoLink
            deriving Show


{- 2.(b) Represent the half adder circuit in abstract syntax, that is, as a Haskell data type value. -}

halfAdder :: Circuit
halfAdder = Con (GatesCon   [
                            (1, Xor), (2, And)
                        ]
                )
                (LinksCon   [
                            ((1,1),(2,1)), ((1,2),(2,2))
                        ]
                )


{- 2.(c) Define a Haskell function that implements a pretty printer for the abstract syntax. -}

--  1:xor;
--  2:and;
printGates :: Gates -> String
printGates NoGate = ""
printGates (GatesCon ((n, gate):rest)) = "Gate " ++ show(n) ++ " of type "
                                        ++ show(gate) ++ "\n" ++ show(GatesCon rest)

-- from 1.1 to 2.1;
-- from 1.2 to 2.2;
printLinks :: Links -> String
printLinks NoLink = ""
printLinks (LinksCon (((x1,y1),(x2,y2)):rest)) = "From " ++ show(x1) ++ "." ++ show(y1)++" to "
                                            ++ show(x2) ++ "."++show(y2) ++ "\n" ++ show(LinksCon rest)

printCircuit :: Circuit -> IO()
printCircuit (Con gates links) = putStr ("Gates: \n" ++ show(gates) ++ "\nLinks:\n" ++ show(links))


-- ==================== Exercise 3 ====================
{- 3.(a) Represent the expression -(3+4)*7 in the alternative abstract syntax. -}

data Expr = N Int
            | Plus Expr Expr
            | Times Expr Expr
            | Neg Expr
            deriving Show

data Op = Add | Multiply | Negate
            deriving Show

data Exp = Num Int
            | Apply Op [Exp]
            deriving Show

--data Exp = Apply Multiply [Apply Negate [Apply Add [Num 3, Num 4]], Num 7]


{- 3.(b) What are the advantages or disadvantages of either representation? -}

{-
The first abstract syntax is easy to understand.
It is just like a sentence which decribes the mathematical methods.
It is simple enough for a beginner to understand this abstract syntax.
However, the second abstract syntax is more difficult to understand than the first one.
lthough it is not too difficult, it need more time to think about the meanings.
It can be easily combined with other algorithm. Nevertheless, it is hard to decide which abstract syntax is better.
It depends on what program they are applied to.
-}


{- 3.(c) Define a function translate :: Expr -> Exp that translates expressions given in the first abstract
syntax into equivalent expressions in the second abstract syntax. -}

translate :: Expr -> Exp
translate (N n) = (Num n)
translate (Plus n m) = Apply Add [
									(translate n), (translate m)
								 ]
translate (Times n m) = Apply Multiply [
											(translate n), (translate m)
										]
translate (Neg n) = Apply Negate [
									translate n
								 ]
