-- Kaiyuan Fan  onid:fankai
-- Zijian Huang onid:huangzi
module MiniLogo where
import Prelude hiding (Num)

--Task 1 ; Define the abstract syntax of MiniLogo as a set of Haskell data types.
type Num = Int
type Var = String
type Macro = String
type Prog = [Cmd]

data Mode = Down
            | Up
            deriving (Eq,Show)

-- expr	::=	var	variable reference
--	num	literal number
--	expr + expr	addition expression

data Expr = Ref Var
          | Lit Num
          | Add Expr Expr
          deriving (Eq,Show)
-- cmd	::=	pen mode	change pen mode
-- |	move ( expr , expr )	move pen to a new position
-- |	define macro ( var* ) { prog }  	define a macro
-- |	call macro ( expr* )	invoke a macro

data Cmd = Pen Mode
          | Move (Expr, Expr)
          | Define Macro [Var] Prog
          | Call Macro [Expr]
          deriving (Eq,Show)

-- Task 2 ; Define a MiniLogo macro line (x1,y1,x2,y2) that (starting from anywhere on the canvas) draws a line segment from (x1,y1) to (x2,y2).
--    Concrete Syntax
--    define line ( x1, y1, x2, y2 ){
--      pen up; move (x1,y1);
--      pen down; move (x2,y2);
--    }
line :: Cmd
line = Define "line" ["x1","y1","x2","y2"]
                       [Pen Up,
                       Move (Ref "x1",Ref "y1"),
                       Pen Down,
                       Move (Ref "x2", Ref "y2")
                       ]

--Task 3 ;Use the line macro you just defined to define a new MiniLogo macro nix (x,y,w,h) that draws a big “X” of width w and height h, starting from position (x,y). Your definition should not contain any move commands.
-- Concrete Syntax
-- define nix ( x, y, w, h){
--    Call line (x, y, x+w, y+h);
--    Call line (x, y+h, x+w, y);
-- }
nix :: Cmd
nix = Define "nix" ["x", "y", "w", "h"]
                     [Call "line"[ Ref "x",Ref "y", Add (Ref "x") (Ref "w"), Add (Ref "y") (Ref "h")],
                      Call "line"[ Ref "x",Add (Ref "y") (Ref "h"),Add (Ref "x") (Ref "w"), Ref "y"]
                     ]

-- Task 4 ;Define a Haskell function steps :: Int -> Prog that constructs a MiniLogo program that draws a staircase of n steps starting from (0,0).
-- >>>steps 3
--[Pen Up,Move (Lit 0,Lit 0),Pen Down,Move (Lit 0,Lit 1),Move (Lit 1,Lit 1),Move (Lit 1,Lit 2),Move (Lit 2,Lit 2),Move (Lit 2,Lit 3),Move (Lit 3,Lit 3)]
steps :: Int -> Prog
steps 0 = [Pen Up, Move(Lit 0, Lit 0), Pen Down]
steps n = steps (n - 1) ++ [Move (Lit (n-1),Lit n )] ++ [Move (Lit n, Lit n)]

-- Task 5 ; Define a Haskell function macros :: Prog -> [Macro] that returns a list of the names of all of the macros that are defined anywhere in a given MiniLogo program.
-- >>> macros [Pen Down, Define "line" [] [], Define "nix" [] []]
-- ["line","nix"]
macros :: Prog -> [Macro]
macros [] = []
macros ((Define n _ _): tail) = n : macros tail
macros ( _ : tail) = macros tail

-- Task 6 ; Define a Haskell function pretty :: Prog -> String that pretty-prints a MiniLogo program.
pretty :: Prog -> String
pretty [] = ""
pretty (x:xs) = prettyhelper x ++ pretty xs

prettyhelper :: Cmd -> String
prettyhelper (Pen Down) = "pen down;\n"
prettyhelper (Pen Up) = "pen up;\n"
prettyhelper (Move (x,y)) = "move (" ++ prettyExpr x ++","++prettyExpr y ++ ");\n"
prettyhelper (Define m v p) = "define " ++ m ++ "(" ++ prettyVar v ++ "){" ++ pretty p ++ "}\n"
prettyhelper (Call m e) = "call " ++ m ++ "(" ++prettyListExpr e ++ ");\n"

prettyExpr :: Expr -> String
prettyExpr (Ref n) = n
prettyExpr (Lit n) = show n
prettyExpr (Add m n) = prettyExpr m ++ " + " ++ prettyExpr n

prettyListExpr :: [Expr] -> String
prettyListExpr [] = ""
prettyListExpr (x:xs)= prettyExpr x ++ ", "++ prettyListExpr xs

prettyVar :: [Var] -> String
prettyVar [] = ""
prettyVar [n] = n
prettyVar (x:xs)= x ++ ", "++ prettyVar xs

--Bonus Problem

--Task 7 ; Define a Haskell function optE :: Expr -> Expr that partially evaluates expressions by replacing any additions of literals with the result.
optE :: Expr -> Expr
optE (Ref n)= Ref n
optE (Lit n)= Lit n
optE (Add (Lit m) (Lit n))= Lit (m + n)
optE (Add (Lit m) (Ref n))= Add (Lit m) (Ref n)
optE (Add (Ref m) (Lit n))= Add (Ref m) (Lit n)
optE (Add m n )= Add (optE m) (optE n)

--Task 8 ;Define a Haskell function optP :: Prog -> Prog that optimizes all of the expressions contained in a given program using optE.
optP :: Prog -> Prog
optP [] = []
optP (x:xs) = optPhelper x :optP xs

optPhelper :: Cmd -> Cmd
optPhelper(Move (m,n)) = Move ((optE m), (optE n))
optPhelper(Call m (x:xs)) = Call m (optE x : optListE xs)
opthelper (Define m v p) = Define m v (optP p)
--opthelper Pen Down = Pen Down
--opthelper Pen Up = Pen Up
opthelper n = n

optListE :: [Expr] -> [Expr]
optListE [] = []
optListE [x] = [optE x]
optListE (x:xs) = optE x : optListE xs

-- >>>optP [Move(Add (Lit 3) (Lit 2), Ref "x")]
--[Move (Lit 5,Ref "x")]