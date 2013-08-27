type Variable = String
type Value = Float
type Program =Stmt
type State = [(Variable,Value)]

data Expr     =  Plus Expr Expr
	   	|Minus Expr Expr
	   	|Mult  Expr Expr
	  	|Div   Expr Expr
	   	|Var   Variable
	   	|Const Value
	 	deriving(Show)

data CondExpr =  Lt  Expr Expr
	     	|Lte Expr Expr
		|Gt  Expr Expr
		|Gte Expr Expr
		|Eq  Expr Expr
		|Neq Expr Expr
		|And CondExpr CondExpr	
		|Or  CondExpr CondExpr
		|Neg CondExpr
		deriving(Show)

data Stmt     =	 Asgn  Variable Expr
		|If    CondExpr Stmt Stmt
		|While CondExpr Stmt
		|Print Expr
		|Read  Variable
		|Seq   Stmt Stmt
		deriving(Show)

present::Variable -> State -> Bool
present s [] = False
present s ((s1,b):xs) = if(s==s1)
                        then True
                        else (present s xs) 

locate:: Variable -> State -> Value
locate s ((s1,b):xs) = if(s==s1)
                       then b
                       else (locate s xs )   

evalExpr::Expr -> State -> Value
evalExpr (Const a) xs = a
evalExpr (Var x) xs = evalExpr (Const(locate x xs)) xs
evalExpr (Plus e1 e2) xs= (evalExpr e1 xs) + (evalExpr e2 xs)
evalExpr (Mult e1 e2) xs= (evalExpr e1 xs) * (evalExpr e2 xs)
evalExpr (Minus e1 e2) xs= (evalExpr e1 xs) - (evalExpr e2 xs)
evalExpr (Div e1 e2) xs = (evalExpr e1 xs) / (evalExpr e2 xs) 


update:: Variable->Value->State->State
update s v [] = [(s,v)]
update s v ((s1,b):xs) = if(s==s1)
			 then ((s1,v):xs)
			 else [(s1,b)]++(update s v xs)

evalCondExpr::CondExpr-> State -> Bool
evalCondExpr (Lt e1 e2) xs =  if ( (evalExpr e1 xs)< (evalExpr e2 xs) )  
			      then True
			      else False 
evalCondExpr (Lte e1 e2) xs =  if ((evalExpr e1 xs)<= (evalExpr e2 xs) )  
			      then True
			      else False 
evalCondExpr (Gt e1 e2) xs =  if ( (evalExpr e1 xs) > (evalExpr e2 xs) )  
			      then True
			      else False 
evalCondExpr (Gte e1 e2) xs = if ( (evalExpr e1 xs) >= (evalExpr e2 xs) )  
			      then True
			      else False 
evalCondExpr (Eq e1 e2) xs =  if ( (evalExpr e1 xs)== (evalExpr e2 xs) )  
			      then True
			      else False 
evalCondExpr (And e1 e2) xs =  if ((evalCondExpr e1 xs)&&(evalCondExpr e2 xs))  
			      then True
			      else False 
evalCondExpr (Or e1 e2) xs =  if ((evalCondExpr e1 xs)||(evalCondExpr e2 xs))  
			      then True
			      else False 
evalCondExpr (Neg e1) xs =  if ((evalCondExpr e1 xs)==False)  
			      then True
			      else False 

evalStmt:: Stmt->State->IO(State)
evalStmt (Asgn x e1) xs   = return(update x (evalExpr e1 xs) xs)
evalStmt (If e1 s1 s2) xs = if((evalCondExpr e1 xs)==True)
			    then (evalStmt s1 xs)
			    else (evalStmt s2 xs)
evalStmt (While e1 s1) xs = if((evalCondExpr e1 xs)==True)
			    then do {ls <- (evalStmt s1 xs);(evalStmt (While e1 s1) ls)}
			    else return(xs)
evalStmt (Read x) xs      = if (xs==[])
			    then do {c <- getLine;return([(x,(read c::Float))])}
			    else do {c <- getLine;
				     if(present x xs) 
				     then return(update x (read c::Float) xs)
				     else return((x,(read c::Float)):xs)}
evalStmt (Seq s1 s2) xs   = do {ls<-(evalStmt s1 xs);(evalStmt s2 ls)}
evalStmt (Print e1) xs   =  do {print(evalExpr e1 xs) ; return(xs)}     


evalProgram:: Program->State->IO(State)
evalProgram s1 xs= evalStmt s1 xs
evalProgram (Seq s1 s2) xs = do {ls<-(evalStmt s1 xs);evalProgram s2 ls}

{------------------------------------The Factorial Program---------------------------------
  Execute by typing evalProgram fact [] 
-}

fact = (Read "n" )`Seq` (Asgn "fact" (Const 1)) `Seq`
      (While (Gt (Var "n") (Const 0)) ( (Asgn "fact" (Mult (Var "fact") (Var "n"))) `Seq` (Asgn "n" (Minus (Var "n") (Const 1))) )
      ) `Seq`
      (Print (Var "fact"))
     

		