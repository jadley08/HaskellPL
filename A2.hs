import Helpers
import Data.Maybe
-- Jacob Adley

--- Part 1: Natural Recursion Refresher ---

-- 1: LISTREF -----------------------------
listref :: [a] -> Int -> (Maybe a)
listref [] _    = Nothing
listref (a:d) 0 = Just a
listref (a:d) n = listref d (sub1 n)

-- 2: UNION -----------------------------
union :: (Eq a) => [a] -> [a] -> [a]
union []    s2 = s2
union (a:d) s2 = if (not (member a s2))
                 then a : (union d s2)
								 else (union d s2)

-- 3: EXTEND -----------------------------
extend :: (Eq a) => a -> (a -> Bool) -> (a -> Bool)
extend x p y = (x == y) || (p y)

-- 4: WALKSYMBOL -----------------------------
walksymbol :: (Eq a) => a -> [(a,a)] -> a
walksymbol v [] = v
walksymbol v (a:d) = let p = assv v (a:d)
                     in if isNothing p
										    then v
												else walksymbol (snd (fromJust p)) d

-- Part 2: Free, Bound, Lexical Address ---
data E = EVar String | ELam String E | EApp E E deriving (Eq, Show)

-- 5: WALKSYMBOL -----------------------------
data E' = E'Var String | E'Lum String E' | E'App E' E' deriving (Eq, Show)

lamlum :: E -> E'
lamlum (EVar x)          = E'Var x
lamlum (ELam x b)        = E'Lum x (lamlum b)
lamlum (EApp rator rand) = E'App (lamlum rator) (lamlum rand)

-- 6: VAROCCURS -----------------------------
varoccurs :: String -> E -> Bool
varoccurs v (EVar x)          = v == x
varoccurs v (ELam x b)        = varoccurs v b
varoccurs v (EApp rator rand) = (varoccurs v rator) || (varoccurs v rand)

-- 7: VARS -----------------------------
vars :: E -> [String]
vars (EVar x)          = [x]
vars (ELam x b)        = vars b
vars (EApp rator rand) = (vars rator) ++ (vars rand)

-- 8: UNIQUEVARS -----------------------------
uniquevars :: E -> [String]
uniquevars (EVar x)          = [x]
uniquevars (ELam x b)        = uniquevars b
uniquevars (EApp rator rand) = union (uniquevars rator) (uniquevars rand)

-- 9: VAROCCURSFREE -----------------------------
varoccursfree :: String -> E -> Bool
varoccursfree v (EVar x)          = v == x
varoccursfree v (ELam x b)        = (not (v == x)) && (varoccursfree v b)
varoccursfree v (EApp rator rand) = (varoccursfree v rator) || (varoccursfree v rand)

-- 10: VAROCCURSBOUND -----------------------------
varoccursbound :: String -> E -> Bool
varoccursbound v (EVar x)          = False
varoccursbound v (ELam x b)        = ((v == x) && (varoccurs v b)) || (varoccursbound v b)
varoccursbound v (EApp rator rand) = (varoccursbound v rator) || (varoccursbound v rand)

-- 11: UNIQUEFREEVARS -----------------------------
uniquefreevars :: E -> [String]
uniquefreevars (EVar x)          = [x]
uniquefreevars (ELam x b)        = filter (\ v -> not (x == v)) (uniquefreevars b)
uniquefreevars (EApp rator rand) = union (uniquefreevars rator) (uniquefreevars rand)

-- 12: UNIQUEBOUNDVARS -----------------------------
uniqueboundvars :: E -> [String]
uniqueboundvars (EVar x)          = []
uniqueboundvars (ELam x b)        = if (member x (vars b))
                                    then union [x] (uniqueboundvars b)
																		else (uniqueboundvars b)
uniqueboundvars (EApp rator rand) = union (uniqueboundvars rator) (uniqueboundvars rand)

-- 13: LEX -----------------------------
data L = LFree String | LVar Int | Lλ L | LApp L L deriving (Eq, Show)
mylex :: E -> [(String,Int)] -> L
mylex (EVar x)          acc = if isNothing (assv x acc)
                              then LFree x
														  else LVar (snd (fromJust (assv x acc)))
mylex (ELam x b)        acc = Lλ (mylex b
                                        ((x,0) : (mymap (\ p -> ((fst p),(add1 (snd p)))) acc)))
mylex (EApp rator rand) acc = LApp (mylex rator acc) (mylex rand acc)



-- TESTS -----------------------------
main =
	print (
		listref [5,6,7] 3 == Nothing &&
		listref [5,6,7] 2 == Just 7 &&
		listref [5,6,7] 0 == Just 5 &&
		union [0] [] == [0] &&
		union [0] [0] == [0] &&
		union [0,1] [1,2] == [0,1,2] &&
		(extend 1 myeven) 0 &&
		(extend 1 myeven) 1 &&
		(extend 1 myeven) 2 &&
		not ((extend 1 myeven) 3) &&
		filter (extend 1 myeven) [0,1,2,3,4,5] == [0,1,2,4] &&
		filter (extend 3 (extend 1 myeven)) [0,1,2,3,4,5] == [0,1,2,3,4] &&
		filter (extend 7 (extend 3 (extend 1 myeven))) [0,1,2,3,4,5] == [0,1,2,3,4] &&
    walksymbol 0 [(0,5)] == 5 &&
		lamlum (EVar "x") == E'Var "x" &&
		lamlum (ELam "x" (EVar "x")) == E'Lum "x" (E'Var "x") &&
		lamlum (ELam "z" (EApp (ELam "y" (EApp (EVar "a") (EVar "z"))) (EApp (EVar "h") (ELam "x" (EApp (EVar "h") (EVar "a")))))) == (E'Lum "z" (E'App (E'Lum "y" (E'App (E'Var "a") (E'Var "z"))) (E'App (E'Var "h") (E'Lum "x" (E'App (E'Var "h") (E'Var "a")))))) &&
		lamlum (ELam "lam" (EVar "lam")) == (E'Lum "lam" (E'Var "lam")) &&
		lamlum (EApp (ELam "lam" (EVar "lam")) (ELam "y" (EVar "y"))) == (E'App (E'Lum "lam" (E'Var "lam")) (E'Lum "y" (E'Var "y"))) &&
		lamlum (EApp (ELam "x" (EVar "x")) (ELam "x" (EVar "x"))) == (E'App (E'Lum "x" (E'Var "x")) (E'Lum "x" (E'Var "x"))) &&
		varoccurs "x" (EVar "x") &&
		not (varoccurs "x" (ELam "x" (EVar "y"))) &&
		varoccurs "x" (ELam "y" (EVar "x")) &&
		varoccurs "x" (EApp (EApp (EVar "z") (EVar "y")) (EVar "x")) &&
		vars (EVar "x") == ["x"] &&
		vars (ELam "x" (EVar "x")) == ["x"] &&
		vars (EApp (ELam "y" (EApp (EVar "x") (EVar "x"))) (EApp (EVar "x") (EVar "y"))) == ["x","x","x","y"] &&
		vars (ELam "z" (EApp (ELam "y" (EApp (EVar "a") (EVar "z"))) (EApp (EVar "h") (ELam "x" (EApp (EVar "h") (EVar "a")))))) == ["a","z","h","h","a"] &&
		uniquevars (EApp (ELam "y" (EApp (EVar "x") (EVar "x"))) (EApp (EVar "x") (EVar "y"))) == ["x","y"] &&
		uniquevars (EApp (ELam "z" (ELam "y" (EApp (EVar "z") (EVar "y")))) (EVar "x")) == ["z","y","x"] &&
		uniquevars (EApp (ELam "a" (EApp (EVar "a") (EVar "b"))) (EApp (ELam "c" (EApp (EVar "a") (EVar "c"))) (EApp (EVar "b") (EVar "a")))) == ["c","b","a"] &&
		varoccursfree "x" (EVar "x") &&
		not (varoccursfree "x" (ELam "y" (EVar "y"))) &&
		not (varoccursfree "x" (ELam "x" (EApp (EVar "x") (EVar "y")))) &&
		not (varoccursfree "x" (ELam "x" (ELam "x" (EVar "x")))) &&
		varoccursfree "y" (ELam "x" (EApp (EVar "x") (EVar "y"))) &&
		varoccursfree "y" (EApp (ELam "y" (EApp (EVar "x") (EVar "y"))) (ELam "x" (EApp (EVar "x") (EVar "y")))) &&
		varoccursfree "x" (EApp (ELam "x" (EApp (EVar "x") (EVar "x"))) (EApp (EVar "x") (EVar "x"))) &&
		not (varoccursbound "x" (EVar "x")) &&
		varoccursbound "x" (ELam "x" (EVar "x")) &&
		not (varoccursbound "y" (ELam "x" (EVar "x"))) &&
		varoccursbound "x" (EApp (ELam "x" (EApp (EVar "x") (EVar "x"))) (EApp (EVar "x") (EVar "x"))) &&
		not (varoccursbound "z" (ELam "y" (ELam "x" (EApp (EVar "y") (EVar "z"))))) &&
		varoccursbound "z" (ELam "y" (ELam "z" (EApp (EVar "y") (EVar "z")))) && 
		not (varoccursbound "x" (ELam "x" (EVar "y"))) &&
		varoccursbound "x" (ELam "x" (ELam "x" (EVar "x"))) &&
		uniquefreevars (EVar "x") == ["x"] &&
		uniquefreevars (ELam "x" (EApp (EVar "x") (EVar "y"))) == ["y"] &&
		uniquefreevars (EApp (ELam "x" (EApp (EApp (EVar "x") (EVar "y")) (EVar "e"))) (ELam "c" (EApp (EVar "x") (ELam "x" (EApp (EVar "x") (EApp (EVar "e") (EVar "c"))))))) == ["y","x","e"] &&
		uniqueboundvars (EVar "x") == [] &&
		uniqueboundvars (ELam "x" (EVar "y")) == [] &&
		uniqueboundvars (ELam "x" (EApp (EVar "x") (EVar "y"))) == ["x"] &&
		uniqueboundvars (EApp (ELam "x" (EApp (EApp (EVar "x") (EVar "y")) (EVar "e"))) (ELam "c" (EApp (EVar "x") (ELam "x" (EApp (EVar "x") (EApp (EVar "e") (EVar "c"))))))) == ["c","x"] &&
		uniqueboundvars (ELam "y" (EVar "y")) == ["y"] &&
		uniqueboundvars (ELam "x" (EApp (EVar "y") (EVar "z"))) == [] &&
		uniqueboundvars (ELam "x" (ELam "x" (EVar "x"))) == ["x"] &&
		mylex (ELam "x" (EVar "x")) [] == (Lλ (LVar 0)) &&
		mylex (ELam "y" (ELam "x" (EVar "y"))) [] == (Lλ (Lλ (LVar 1))) &&
		mylex (ELam "y" (ELam "x" (EApp (EVar "x") (EVar "y")))) [] == (Lλ (Lλ (LApp (LVar 0) (LVar 1)))) &&
		mylex (ELam "x" (ELam "x" (EApp (EVar "x") (EVar "x")))) [] == (Lλ (Lλ (LApp (LVar 0) (LVar 0)))) &&
		mylex (ELam "y" (EApp (ELam "x" (EApp (EVar "x") (EVar "y"))) (ELam "c" (ELam "d" (EApp (EVar "y") (EVar "c")))))) [] == (Lλ (LApp (Lλ (LApp (LVar 0) (LVar 1))) (Lλ (Lλ (LApp (LVar 2) (LVar 1)))))) &&
		mylex (ELam 
		        "a" 
						(ELam 
						  "b"
							(ELam 
							  "c" 
								(ELam
								  "a"
									(ELam
									  "b"
										(ELam 
										  "d"
											(ELam
											  "a"
												(ELam
												  "e"
													(EApp
													  (EApp
														  (EApp
															  (EApp
																  (EApp
																	  (EVar "a")
																	(EVar "b"))
																(EVar "c"))
															(EVar "d"))
														(EVar "e"))
													(EVar "a"))))))))))
						[]
			==
			(Lλ
			  (Lλ
				  (Lλ
					  (Lλ
						  (Lλ
							  (Lλ
								  (Lλ
									  (Lλ
										  (LApp
											  (LApp
												  (LApp
													  (LApp
														  (LApp
															  (LVar 1)
																(LVar 3))
															(LVar 5))
														(LVar 2))
													(LVar 0))
												(LVar 1))))))))))
		-- &&
		--lex (ELam
		--      "a"
		--      (ELam
		--        "b"
		--        (ELam
		--          "c"
		--          (ELam
		--            "w"
		--            (ELam
									--"x"
									--(ELam
									--  "y"
									--  (EApp
									--    (ELam 
									--      "a"
									--      (ELam
									--        "b"
									--        (ELam
									--          "c"
									--          (EApp
									--            (EApp
									--              (EApp
									--                (EApp
									--                  (EApp
									--                    (EVar "a")
									--                    (EVar "b"))
									--                  (EVar "c"))
									--                (EVar "w"))
									--              (EVar "x"))
															--(EVar "y")))))
		--                  (ELam
		--                    "w"
		--                    (ELam 
		--                      "x"
		--                      (ELam
		--                        "y"
		--                        (EApp
		--                          (EApp
		--                            (EApp
		--                              (EApp
		--                                (EApp
		--                                  (EVar "a")
		--                                  (EVar "b"))
		--                                (EVar "c"))
		--                              (EVar "w"))
		--                            (EVar "x"))
		--                          (EVar "y"))))))))))))
		--    []
		-- ==
		--(Lλ
		--  (Lλ
		--    (Lλ
		--      (Lλ
		--        (Lλ
		--          (Lλ
		--            (LApp
		--              (Lλ
		--                (Lλ
		--                  (Lλ
		--                    (LApp
		--                      (LApp
		--                        (LApp
		--                          (LApp
		--                            (LApp
		--                              (LVar 2)
		--                              (LVar 1))
		--                            (LVar 0))
		--                          (LVar 5))
		--                        (LVar 4))
		--                      (LVar 3)))))
									--(Lλ
									--  (Lλ
									--    (Lλ
									--      (LApp
									--        (LApp
									--          (LApp
									--            (LApp
									--              (LApp
									--                (LVar 8)
									--                (LVar 7))
									--              (LVar 6))
									--            (LVar 2))
									--          (LVar 1))
									--        (LVar 0))))))))))))
	)
