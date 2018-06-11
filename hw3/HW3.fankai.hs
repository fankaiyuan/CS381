-- Kaiyuan Fan  onid:fankai
-- Zijian Huang onid:huangzi
module HW3 where

import MiniMiniLogo
import Render


--
-- * Semantics of MiniMiniLogo
--

-- NOTE:
--   * MiniMiniLogo.hs defines the abstract syntax of MiniMiniLogo and some
--     functions for generating MiniMiniLogo programs. It contains the type
--     definitions for Mode, Cmd, and Prog.
--   * Render.hs contains code for rendering the output of a MiniMiniLogo
--     program in HTML5. It contains the types definitions for Point and Line.

-- | A type to represent the current state of the pen.
type State = (Mode,Point)

-- | The initial state of the pen.
start :: State
start = (Up,(0,0))

-- | A function that renders the image to HTML. Only works after you have
--   implemented `prog`. Applying `draw` to a MiniMiniLogo program will
--   produce an HTML file named MiniMiniLogo.html, which you can load in
--   your browswer to view the rendered image.
draw :: Prog -> IO ()
draw p = let (_,ls) = prog p start in toHTML ls


-- Semantic domains:
--   * Cmd:  State -> (State, Maybe Line)
--   * Prog: State -> (State, [Line])


-- | Semantic function for Cmd.
--   
--   >>> cmd (Pen Down) (Up,(2,3))
--   ((Down,(2,3)),Nothing)
--
--   >>> cmd (Pen Up) (Down,(2,3))
--   ((Up,(2,3)),Nothing)
--
--   >>> cmd (Move 4 5) (Up,(2,3))
--   ((Up,(4,5)),Nothing)
--
--   >>> cmd (Move 4 5) (Down,(2,3))
--   ((Down,(4,5)),Just ((2,3),(4,5)))
--
cmd :: Cmd -> State -> (State, Maybe Line)
cmd (Pen m) (_, p) = ((m, p), Nothing)
cmd (Move x2 y2) (mode, (x1, y1)) = case mode of
                                    Up   -> ((Up, (x2, y2)), Nothing)
                                    Down -> ((Down, (x2, y2)), Just ((x1, y1), (x2, y2)))


-- | Semantic function for Prog.
--
--   >>> prog (nix 10 10 5 7) start
--   ((Down,(15,10)),[((10,10),(15,17)),((10,17),(15,10))])
--
--   >>> prog (steps 2 0 0) start
--   ((Down,(2,2)),[((0,0),(0,1)),((0,1),(1,1)),((1,1),(1,2)),((1,2),(2,2))])
prog :: Prog -> State -> (State, [Line])
prog [] state = (state,[])
prog (x:xs) state = case cmd x state of
                      (newstate, Just line) -> (\(state,xs)->(state, line:xs))(prog xs newstate)
                      (newstate, Nothing) -> prog xs newstate


--
-- * Extra credit
--

-- | This should be a MiniMiniLogo program that draws an amazing picture.
--   Add as many helper functions as you want.
amazing :: Prog
amazing = setpen (20,0)++
          [Move 20 1, Move 19 1,Move 19 3, Move 18 3,Move 18 7,Move 19 7 ,Move 19 10 ,Move 20 10 ,Move 20 13 ,Move 19 13 ,Move 19 14 ,Move 17 14 ,Move 17 15 ,Move 16 15 ,Move 16 16,Move 15 16 ,Move 15 17 ,Move 13 17 ,Move 13 19 ,Move 19 19 ,Move 19 18 ,Move 23 18, Move 23 17,Move 27 17, Move 27 16, Move 28 16, Move 28 17, Move 34 17, Move 34 16, Move 35 16, Move 35 17, Move 39 17, Move 39 18, Move 43 18, Move 43 19 , Move 49 19, Move 49 17, Move 47 17, Move 47 16, Move 46 16, Move 46 15, Move 45 15, Move 45 14, Move 44 14, Move 44 13, Move 42 13, Move 42 12, Move 40 12, Move 40 10, Move 41 10, Move 41 7, Move 42 7, Move 42 2, Move 41 2, Move 41 1, Move 40 1, Move 40 0, Move 20 0] ++
          setpen (19,4)++
          eye 19 4 ++
          setpen(36,4)++
          eye 36 4 ++
          setpen(22,8)++
          eye 22 8 ++
          setpen(33,8)++
          eye 33 8 ++
          smallbox 23 9 ++
          smallbox 34 9 ++
          smallbox 25 4 ++
          smallbox 32 4 ++
          rec 26 3 ++
          rec 28 4 ++
          rec 30 3++
          setpen(28,6)++
          [Move 28 7, Move 30 7, Move 30 6, Move 28 6]++
          setpen (25, 5)


setpen::(Int, Int) -> Prog
setpen(x,y) = [Pen Up, Move x y, Pen Down]

smallbox :: Int -> Int -> Prog
smallbox x y = [Pen Up, Move x y, Pen Down,
           Move (x+1) y, Move (x+1) (y+1), Move x (y+1), Move x y]

eye :: Int -> Int -> Prog
eye x y =[Move x (y+2), Move (x+1) (y+2), Move (x+1) (y+3), Move (x+3) (y+3), Move (x+3) (y+2), Move (x+4) (y+2), Move (x+4) y, Move (x+3) y, Move (x+3) (y-1), Move (x+1) (y-1), Move (x+1) y , Move x y]

rec :: Int -> Int -> Prog
rec x y =[Pen Up, Move x y, Pen Down,
          Move (x+2) y, Move (x+2) (y+1), Move x (y+1), Move x y  ]