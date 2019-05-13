import Helpers
import Data.Maybe

-- Data Definition -------------------
data Identifier = Id String deriving (Eq,Show)

data Expr = EId Identifier | Λ Identifier Expr | App Expr Expr deriving (Eq,Show)

type ValueEnv = [(Identifier,Closure)]

data Closure = Clos ValueEnv Identifier Expr deriving (Eq,Show)

closρ :: Closure -> ValueEnv
closρ (Clos ρ id e) = ρ

closid :: Closure -> Identifier
closid (Clos ρ id e) = id

closexp :: Closure -> Expr
closexp (Clos ρ id e) = e

emptyEnv = []

-- Lookup ----------------------------
lookp :: ValueEnv -> Identifier -> Maybe Closure
lookp ρ x = let venv = (assv x ρ) in
             if isNothing venv
						 then Nothing
						 else Just (snd (fromJust venv))

-- Extend ----------------------------
extend :: ValueEnv -> Identifier -> Closure -> ValueEnv
extend ρ x v = (x,v) : ρ

-- Valof -----------------------
valof :: ValueEnv -> Expr -> Maybe Closure
valof ρ (EId (Id y))     = (lookp ρ (Id y))
valof ρ (Λ x b)          = Just (Clos ρ x b)
valof ρ (App rator rand) = let ratorclos = valof ρ rator in
                           let randclos  = valof ρ rand  in
													 (valof (extend (closρ (fromJust ratorclos))
													                (closid (fromJust ratorclos))
																					(fromJust randclos))
																	(closexp (fromJust ratorclos)))

-- Tests -----------------------------
main =
	print (
		1
	)
