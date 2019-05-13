import Helpers
import Data.Maybe

-- λ-calculus interpreter using closures
-- Consider replacing all occurences of String with String

-- Data Definition -------------------
data Expr = Id String | Λ String Expr | App Expr Expr deriving (Eq,Show)

type ValueEnv = [(String,Closure)]

data Closure = Clos ValueEnv String Expr deriving (Eq,Show)

closρ :: Closure -> ValueEnv
closρ (Clos ρ id e) = ρ

closid :: Closure -> String
closid (Clos ρ id e) = id

closexp :: Closure -> Expr
closexp (Clos ρ id e) = e

emptyEnv = []

-- Lookup ----------------------------
lookp :: ValueEnv -> String -> Maybe Closure
lookp ρ x = let venv = (assv x ρ) in
             if isNothing venv
						 then Nothing
						 else Just (snd (fromJust venv))

-- Extend ----------------------------
extend :: ValueEnv -> String -> Closure -> ValueEnv
extend ρ x v = (x,v) : ρ

-- Valof -----------------------------
valof :: ValueEnv -> Expr -> Maybe Closure
valof ρ (Id y)          = (lookp ρ y)
valof ρ (Λ x b)          = Just (Clos ρ x b)
valof ρ (App rator rand) = let ratorclos = valof ρ rator in
                           let randclos  = valof ρ rand  in
													 (valof (extend (closρ (fromJust ratorclos))
													                (closid (fromJust ratorclos))
																					(fromJust randclos))
																	(closexp (fromJust ratorclos)))

-- Program ---------------------------
data Define = Def String Expr deriving (Eq,Show)
data Exec = ExecD Define | ExecE Expr deriving (Eq,Show)
type Program = [Exec]

valofp :: ValueEnv -> Program -> [Closure]
valofp ρ []                    = []
valofp ρ ((ExecD (Def x e)):d) = (valofp (extend ρ x (fromJust (valof ρ e))) d)
valofp ρ ((ExecE e):d)         = (fromJust (valof ρ e)) : (valofp ρ d)

-- Church ----------------------------
withchurch :: Expr -> Program
withchurch e = [ExecD (Def "church-zero"
                        (Λ "f"
												  (Λ "x"
													  (Id "x")))),
								ExecD (Def "church-add1"
								        (Λ "n-1"
												  (Λ "f"
													  (Λ "x"
														  (App (Id "f")
															  (App (App (Id "n-1") (Id "f"))
																  (Id "x"))))))),
								ExecD (Def "church-plus"
								        (Λ "j"
												  (Λ "k"
													  (Λ "f"
														  (Λ "x"
															  (App (App (Id "j") (Id "f"))
																  (App (App (Id "k") (Id "f"))
																	  (Id "x")))))))),
								ExecE e]

tochurch :: Int -> Expr
tochurch 0 = (Id "church-zero")
tochurch n = App (Id "church-add1")
                 (tochurch (sub1 n))

-- Tests -----------------------------
main =
	print (
		(valof
			emptyEnv
			(Λ "x"
				(Λ "x"
					(Λ "y"
						(App (Id "y") (Id "x")))))) ==
		Just (Clos [] 
							 "x"
							 (Λ "x"
								 (Λ "y"
									 (App (Id "y") (Id "x")))))
		 &&
		(valofp
			emptyEnv
			[ExecD (Def "id"
							 (Λ "x"
								 (Id "x"))),
			 ExecE (App (Id "id")
							 (Λ "y"
								 (Λ "z"
									 (App (Id "z")
										 (Id "y")))))]) ==
		[Clos [("id",Clos [] "x" (Id "x"))]
					"y"
					(Λ "z" (App (Id "z") (Id "y")))]
		 &&
		(valofp
			emptyEnv
			(withchurch (tochurch 0))) ==
		[Clos []
					"f"
					(Λ "x" (Id "x"))]
		 &&
		(valofp
			emptyEnv
			(withchurch (tochurch 1))) ==
		[Clos [("n-1",Clos [] "f" (Λ "x" (Id "x"))),
					 ("church-zero",Clos [] "f" (Λ "x" (Id "x")))]
					"f"
					(Λ "x"
						(App (Id "f")
							(App (App (Id "n-1") (Id "f"))
								(Id "x"))))]
		 &&
		(valofp
			emptyEnv
			(withchurch (tochurch 4))) ==
		[Clos
		  [("n-1",
			  Clos
				  [("n-1",
					  Clos 
						  [("n-1",
							  Clos 
								  [("n-1",
									  Clos 
										  [] 
											"f" 
											(Λ "x" (Id "x"))),
									 ("church-zero",
									  Clos 
											[] 
											"f" 
											(Λ "x" 
											  (Id "x")))]
									"f" 
									(Λ "x" 
									  (App (Id "f") 
										  (App (App (Id "n-1") 
											       (Id "f"))
											  (Id "x"))))),
								("church-zero",
								 Clos 
									 [] 
									 "f" 
									 (Λ "x" 
										 (Id "x")))] 
							"f"
							(Λ "x"
								(App (Id "f")
								  (App (App (Id "n-1") 
									       (Id "f"))
										(Id "x"))))),
					 ("church-zero",
					  Clos 
						  []
							"f" 
							(Λ "x"
								(Id "x")))]
					"f"
					(Λ "x"
					  (App (Id "f")
						  (App (App (Id "n-1")
							       (Id "f"))
							  (Id "x"))))),
			 ("church-zero",
			  Clos
				  []
					"f"
					(Λ "x"
					  (Id "x")))]
			"f"
			(Λ "x"
				(App (Id "f")
					(App (App (Id "n-1")
							   (Id "f"))
						(Id "x"))))]
	)
