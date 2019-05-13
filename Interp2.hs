import Helpers
import Data.Maybe

-- λ-calculus interpreter using closures

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

-- Valof -----------------------------
valof :: ValueEnv -> Expr -> Maybe Closure
valof ρ (EId y)          = (lookp ρ y)
valof ρ (Λ x b)          = Just (Clos ρ x b)
valof ρ (App rator rand) = let ratorclos = valof ρ rator in
                           let randclos  = valof ρ rand  in
													 (valof (extend (closρ (fromJust ratorclos))
													                (closid (fromJust ratorclos))
																					(fromJust randclos))
																	(closexp (fromJust ratorclos)))

-- Program ---------------------------
data Define = Def Identifier Expr deriving (Eq,Show)
data Exec = ExecD Define | ExecE Expr deriving (Eq,Show)
type Program = [Exec]

valofp :: ValueEnv -> Program -> [Closure]
valofp ρ []                    = []
valofp ρ ((ExecD (Def x e)):d) = (valofp (extend ρ x (fromJust (valof ρ e))) d)
valofp ρ ((ExecE e):d)         = (fromJust (valof ρ e)) : (valofp ρ d)

-- Tests -----------------------------
main =
	print (
		(valof
			emptyEnv
			(Λ (Id "x")
				(Λ (Id "x")
					(Λ (Id "y")
						(App (EId (Id "y")) (EId (Id "x"))))))) ==
		Just (Clos [] 
							 (Id "x")
								 (Λ (Id "x")
									 (Λ (Id "y")
										 (App (EId (Id "y")) (EId (Id "x"))))))
		&&
		(valofp
		  emptyEnv
			[ExecD (Def (Id "id")
			         (Λ (Id "x")
							   (EId (Id "x")))),
			 ExecE (App (EId (Id "id"))
			         (Λ (Id "y")
							   (Λ (Id "z")
								   (App (EId (Id "z"))
									   (EId (Id "y"))))))]) ==
		[Clos [(Id "id",Clos [] (Id "x") (EId (Id "x")))]
		      (Id "y")
					(Λ (Id "z") (App (EId (Id "z")) (EId (Id "y"))))]
	)
