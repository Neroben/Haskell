data Expr = Const Integer|Var String|Add Expr Expr|Mult Expr Expr|Division Expr Expr  deriving(Show)

diff :: Expr -> String -> Expr
diff (Const _) _ = Const 0
diff (Var x) d = if x == d then Const 1 else Const 0
diff (Add x y) d = Add (diff x d) (diff y d)
diff (Mult x y) d = Add (Mult (diff x d) y) (Mult x (diff y d))

simplify :: Expr -> Expr
simplify (Add (Const 0) a) = a
simplify (Add a (Const 0)) = a
simplify (Add (Const 0) (Const 0)) = Const 0
simplify (Add a b) = Add (simplify a) (simplify b)
simplify (Mult (Const 1) a) = a
simplify (Mult a (Const 1)) = a
simplify (Mult (Const 1) (Const 1)) = Const 1
simplify (Mult (Const 0) _) = Const 0
simplify (Mult _ (Const 0)) = Const 0
simplify (Mult a b) = Mult (simplify a) (simplify b)

toString :: Expr -> String
toString a = toStr a False

toStr :: Expr -> Bool -> String
toStr (Add a b) _ = (toStr a True) ++ "+" ++ (toStr b True)
toStr (Mult a b) False = (toStr a False) ++ "*" ++ (toStr b False)
toStr (Mult a b) True = "(" ++ (toStr a False) ++ "*" ++ (toStr b False) ++ ")"
toStr (Const a) _ = show a
toStr (Var a) _ = a

eval :: Expr -> [(String, Integer)] -> Integer
eval (Add a b) c = (eval a c) + (eval b c)
eval (Mult a b) c = (eval a c) * (eval b c)
eval (Const a) _ = a
eval (Var a) (x:xs) = if a == fst x then snd x else eval (Var a) xs


integration :: Expr -> String -> Expr
integration (Const a) b = Mult (Const a) (Var b)
integration (Var a) b = if (a /= b) then Division (Mult (Var a) (Var b)) (Const 2) else Division (Mult (Var a) (Var b)) (Const 2) 
integration (Add a b) c = Add (integration a c) (integration b c)
integration (Mult (Const a) (Const b)) c = (Mult (integration (Const a) c) (Const 1))
integration (Mult (Const a) (Var b)) c = Mult (Const a) (integration (Var b) c)
integration (Mult (Var b) (Const a)) c = Mult (Const a) (integration (Var b) c)
integration (Mult (Var a) (Var b)) c 
    | (a == b) && (a == c) = Division (Mult (Mult (Var a) (Var b)) (Var c)) (Const 3)
    | (a == c) && (b /= c) = Division (Mult (Mult (Var a) (Var c)) (Var b)) (Const 2)
    | (b == c) && (a /= c) = Division (Mult (Mult (Var b) (Var c)) (Var a)) (Const 2)
    | (a /= c) && (b /= c) = Division (Mult (Mult (Var a) (Var b)) (Var c)) (Const 1)
integration (Division a b) c = Division (Mult a c) (Const (b+1))
