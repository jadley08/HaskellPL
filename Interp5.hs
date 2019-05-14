import Helpers
import Data.Maybe

-- λ-calculus interpreter using closures

-- Data Definition -------------------
data Expr = Id String | Zero | Add1 Expr | WhichNat Expr Expr Expr | IterNat Expr Expr Expr | RecNat Expr Expr Expr | Λ String Expr | App Expr Expr deriving (Eq,Show)

data Closure = Clos ValueEnv String Expr deriving (Eq,Show)

data Value = VC Closure | VN Expr deriving (Eq,Show)

type ValueEnv = [(String,Value)]

fromVC :: Value -> Closure
fromVC (VC c) = c

closρ :: Closure -> ValueEnv
closρ (Clos ρ id e) = ρ

closid :: Closure -> String
closid (Clos ρ id e) = id

closexp :: Closure -> Expr
closexp (Clos ρ id e) = e

isZero :: Value -> Bool
isZero (VN Zero) = True
isZero _         = False

vadd1b :: Value -> Value
vadd1b (VN (Add1 b)) = (VN b)

add1v :: Value -> Value
add1v (VN n) = VN (Add1 n)

emptyEnv = []

-- Lookup ----------------------------
lookp :: ValueEnv -> String -> Maybe Value
lookp ρ x = let venv = (assv x ρ) in
            if isNothing venv
            then Nothing
            else Just (snd (fromJust venv))

-- Extend ----------------------------
extend :: ValueEnv -> String -> Value -> ValueEnv
extend ρ x v = (x,v) : ρ

-- Valof -----------------------------
elimFun :: Value -> Value -> Value
elimFun (VC (Clos ρ id b)) rand = valof (extend ρ id rand) b

elimWhichNat :: Value -> Value -> Value -> Value
elimWhichNat tgt base step = if isZero tgt
                             then base
                             else elimFun step (vadd1b tgt)

elimIterNat :: Value -> Value -> Value -> Value
elimIterNat tgt base step = if isZero tgt
                            then base
                            else elimFun step (elimIterNat (vadd1b tgt) base step)

elimRecNat :: Value -> Value -> Value -> Value
elimRecNat tgt base step = if isZero tgt
                           then base
                           else elimFun (elimFun step (vadd1b tgt))
                                        (elimRecNat (vadd1b tgt) base step)

valof :: ValueEnv -> Expr -> Value
valof ρ (Id y)                   = fromJust (lookp ρ y)
valof ρ Zero                     = (VN Zero)
valof ρ (Add1 b)                 = add1v (valof ρ b)
valof ρ (WhichNat tgt base step) = elimWhichNat (valof ρ tgt) (valof ρ base) (valof ρ step)
valof ρ (IterNat tgt base step)  = elimIterNat (valof ρ tgt) (valof ρ base) (valof ρ step)
valof ρ (RecNat tgt base step)   = elimRecNat (valof ρ tgt) (valof ρ base) (valof ρ step)
valof ρ (Λ x b)                  = VC (Clos ρ x b)
valof ρ (App rator rand)         = elimFun (valof ρ rator) (valof ρ rand)

-- Program ---------------------------
data Define = Def String Expr deriving (Eq,Show)
data Exec = ExecD Define | ExecE Expr deriving (Eq,Show)
type Program = [Exec]

valofp :: ValueEnv -> Program -> [Value]
valofp ρ []                    = []
valofp ρ ((ExecD (Def x e)):d) = (valofp (extend ρ x (valof ρ e)) d)
valofp ρ ((ExecE e):d)         = (valof ρ e) : (valofp ρ d)

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
		(VC (Clos [] 
							 "x"
							 (Λ "x"
								 (Λ "y"
									 (App (Id "y") (Id "x"))))))
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
		[VC (Clos [("id",VC (Clos [] "x" (Id "x")))]
					"y"
					(Λ "z" (App (Id "z") (Id "y"))))]
		 &&
		(valofp
			emptyEnv
			(withchurch (tochurch 0))) ==
		[VC (Clos []
					"f"
					(Λ "x" (Id "x")))]
		 &&
		(valofp
			emptyEnv
			(withchurch (tochurch 1))) ==
		[VC (Clos [("n-1",VC (Clos [] "f" (Λ "x" (Id "x")))),
					 ("church-zero",VC (Clos [] "f" (Λ "x" (Id "x"))))]
					"f"
					(Λ "x"
						(App (Id "f")
							(App (App (Id "n-1") (Id "f"))
								(Id "x")))))]
		 &&
		(valofp
			emptyEnv
			(withchurch (tochurch 4))) ==
		[VC (Clos [("n-1",VC (Clos [("n-1",VC (Clos [("n-1",VC (Clos [("n-1",VC (Clos [] "f" (Λ "x" (Id "x")))),("church-zero",VC (Clos [] "f" (Λ "x" (Id "x"))))] "f" (Λ "x" (App (Id "f") (App (App (Id "n-1") (Id "f")) (Id "x")))))),("church-zero",VC (Clos [] "f" (Λ "x" (Id "x"))))] "f" (Λ "x" (App (Id "f") (App (App (Id "n-1") (Id "f")) (Id "x")))))),("church-zero",VC (Clos [] "f" (Λ "x" (Id "x"))))] "f" (Λ "x" (App (Id "f") (App (App (Id "n-1") (Id "f")) (Id "x")))))),("church-zero",VC (Clos [] "f" (Λ "x" (Id "x"))))] "f" (Λ "x" (App (Id "f") (App (App (Id "n-1") (Id "f")) (Id "x")))))]
		 &&
		(valofp
			emptyEnv
			(withchurch (App (App (Id "church-plus")
												 (tochurch 2))
								(tochurch 2)))) ==
		[VC (Clos [("k",VC (Clos [("n-1",VC (Clos [("n-1",VC (Clos [] "f" (Λ "x" (Id "x")))),("church-zero",VC (Clos [] "f" (Λ "x" (Id "x"))))] "f" (Λ "x" (App (Id "f") (App (App (Id "n-1") (Id "f")) (Id "x")))))),("church-zero",VC (Clos [] "f" (Λ "x" (Id "x"))))] "f" (Λ "x" (App (Id "f") (App (App (Id "n-1") (Id "f")) (Id "x")))))),("j",VC (Clos [("n-1",VC (Clos [("n-1",VC (Clos [] "f"
		(Λ "x" (Id "x")))),("church-zero",VC (Clos [] "f" (Λ "x" (Id "x"))))] "f" (Λ "x" (App (Id "f") (App (App (Id "n-1") (Id "f")) (Id "x")))))),("church-zero",VC (Clos [] "f" (Λ "x" (Id "x"))))] "f" (Λ "x" (App (Id "f") (App (App (Id "n-1") (Id "f")) (Id "x")))))),("church-add1",VC (Clos [("church-zero",VC (Clos [] "f" (Λ "x" (Id "x"))))] "n-1" (Λ "f" (Λ "x" (App (Id "f") (App (App (Id "n-1") (Id "f")) (Id "x"))))))),("church-zero",VC (Clos [] "f" (Λ "x" (Id "x"))))] "f" (Λ "x" (App (App (Id "j") (Id "f")) (App (App (Id "k") (Id "f")) (Id "x")))))]
		 &&
		(valofp
			emptyEnv
			[ExecE (WhichNat Zero
											 (Add1 Zero)
											 (Λ "x" (Id "x")))])
		 ==
		[VN (Add1 Zero)]
		 &&
		(valofp
			emptyEnv
			[ExecE (WhichNat (Add1 Zero)
											 (Add1 Zero)
											 (Λ "x" (Id "x")))])
		 ==
		[VN Zero]
		 &&
    (valofp
      []
      [ExecE (IterNat (Add1 (Add1 Zero))
                      (Add1 (Add1 (Add1 Zero)))
                      (Λ "+n-1" (Add1 (Id "+n-1"))))]) ==
		[VN (Add1 (Add1 (Add1 (Add1 (Add1 Zero)))))]
		 &&
    (valofp
      []
      [ExecD (Def "step-+"
                  (Λ "+n-1" (Add1 (Id "+n-1")))),
       ExecE (IterNat (Add1 (Add1 Zero))
                      (Add1 (Add1 (Add1 Zero)))
                      (Id "step-+"))]) ==
		[VN (Add1 (Add1 (Add1 (Add1 (Add1 Zero)))))]
  )
