-- Kaiyuan Fan  onid:fankai
-- Zijian Huang onid:huangzi
module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState


-- | Valuation function for Test.
test :: Test -> World -> Robot -> Bool
test (Not t) w r = not (test t w r)
test (Facing c) _ r = c == getFacing r
test (Clear d) w r = isClear (relativePos d r) w
test (Beeper) w r = hasBeeper (getPos r) w
test (Empty) _ r = isEmpty r





-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown   _ _ r = Done r
stmt PickBeeper _ w r = let p = getPos r
                        in if hasBeeper p w
                              then OK (decBeeper p w) (incBag r)
                              else Error ("No beeper to pick at: " ++ show p)
--
--   >>> stmt Move [] (\p -> Nothing) ((0, 0), North, 0)
--   Error: Blocked at: (0,1)
--
--   >>> stmt Move [] (\p -> Just 0) ((0, 0), North, 0)
--   OK: ((0,1),North,0)
stmt Move _ w r = let p = relativePos Front r
                  in if isClear p w
                        then OK w (setPos p r)
                        else Error ("Blocked at: " ++ show p)
--
--   >>> stmt PutBeeper [] (\p -> Nothing) ((0, 0), North, 0)
--   Error: No beeper to put.
--
--   >>> stmt PutBeeper [] (\p -> Just 0) ((0, 0), North, 1)
--   OK: ((0,0),North,0)
stmt PutBeeper _ w r  = let p = getPos r
                         in if isEmpty r
                               then Error ("No beeper to put.")
                               else OK (incBeeper p w) (decBag r)
--
--   >>> stmt (Turn Left) [] (\p -> Just 0) ((0, 0), North, 0)
--   OK: ((0,0),West,0)
stmt (Turn d) _ w r  = OK w (setFacing (cardTurn d (getFacing r)) r)
stmt (Call m) d w r  = case lookup m d of
                             (Just a) -> stmt a d w r
                             _        -> Error ("Undefined macro: " ++ m)
stmt (Iterate i s) d w r  = if i > 0
                               then case stmt s d w r of
                                         (OK w' r') -> stmt (Iterate (i-1) s ) d w' r'
                                         (Done r')  -> Done r'
                                         (Error e)  -> Error e
                               else OK w r
stmt (If t st1 st2) d w r = if (test t w r)
                              then stmt st1 d w r
                              else stmt st2 d w r
stmt (While t s) d w r = if (test t w r)
                             then case stmt s d w r of
                                  (OK w' r') -> stmt (While t s) d w' r'
                                  (Done r')  -> Done r'
                                  (Error e)  -> Error e
                             else OK w r
stmt (Block []) _ w r     = OK w r
stmt (Block (s:ss)) d w r = case stmt s d w r of
                                 (OK w' r') -> stmt (Block ss) d w' r'
                                 (Done r')  -> Done r'
                                 (Error e)  -> Error e
-- all test passed
-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
