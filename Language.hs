module Language where

data Exp a = EVar a              -- Variables
           | ENum Int            -- Numbers
           | EConstr Int Int     -- Constructor tag arity
           | EAp (Exp a) (Exp a) -- Application
           | ELet {              -- Let(rec) expressions
               letIsRec :: Bool
             , letDefs  :: [(a, Exp a)]
             , letBody  :: (Exp a)
             }
           | ECase {             -- Case expression
               caseExp   :: (Exp a)
             , caseAlts  :: [Alter a]
             }
           | ELam [a] (Exp a)    -- Lambda abstraction
             deriving (Read, Show)

type CoreExp = Exp Name
type Name = String
type Alter a = (Int, [a], Exp a)
type CoreAlt = Alter Name
type Program a = [ScDef a]
type CoreProgram = Program Name
type ScDef a = (Name, [a], Exp a)
type CoreScDef = ScDef Name

bindersOf :: [(a,b)] -> [a]
bindersOf = map fst

rhsOf :: [(a,b)] -> [b]
rhsOf = map snd

isAtomicExp :: Exp a -> Bool
isAtomicExp (EVar v) = True
isAtomicExp (ENum n) = True
isAtomicExp _        = False

preludeDefs :: CoreProgram
preludeDefs
  = [ ("I", ["x"], EVar "x"),
      ("K", ["x","y"], EVar "x"),
      ("K1",["x","y"], EVar "y"),
      ("S", ["f","g","x"], EAp (EAp (EVar "f") (EVar "x"))
                               (EAp (EVar "g") (EVar "x"))),
      ("compose", ["f","g","x"], EAp (EVar "f")
                                     (EAp (EVar "g") (EVar "x"))),
      ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f"))
                           (EVar "f")) ]

extraPreludeDefs :: CoreProgram
extraPreludeDefs = []