data Expr
    = Const Int
    | Var String
    | Binary Op Expr Expr 
    deriving (Show, Eq, Ord)

data Op = Plus 
        | Minus
        | Mul 
        | Div 
        deriving (Show, Eq, Ord)

data Error 
    = Variable_not_found String
    | Div_by_zero
    deriving (Show, Eq, Ord)

type Env = [(String, Int)]



evalExpr :: Env -> Expr -> Either [Error] Int
evalExpr _ (Const n) = Right n
evalExpr env (Var x) = case lookup x env of 
    Nothing -> Left [Variable_not_found x]
    Just n -> Right n 
evalExpr env (Binary Plus expr1 expr2) = 
    case (evalExpr env expr1 , evalExpr env expr2) of 
        (Right n1, Right n2) -> Right $ n1 + n2
        (Left err1, Left err2) -> Left (err1 ++ err2)
        (Left err1, Right n) -> Left err1
        (Right n, Left err2) -> Left err2
evalExpr env (Binary Minus expr1 expr2) = 
    case (evalExpr env expr1 , evalExpr env expr2) of 
        (Right n1, Right n2) -> Right $ n1 - n2
        (Left err1, Left err2) -> Left (err1 ++ err2)
        (Left err1, Right n) -> Left err1
        (Right n, Left err2) -> Left err2
evalExpr env (Binary Mul expr1 expr2) = 
    case (evalExpr env expr1 , evalExpr env expr2) of 
        (Right n1, Right n2) -> Right $ n1 * n2
        (Left err1, Left err2) -> Left (err1 ++ err2)
        (Left err1, Right n) -> Left err1
        (Right n, Left err2) -> Left err2
evalExpr env (Binary Div expr1 expr2) = 
    case (evalExpr env expr1 , evalExpr env expr2) of 
        (Left err, Right 0) -> Left ([Div_by_zero] ++ err)
        (_, Right 0) -> Left [Div_by_zero]
        (Right n1, Right n2) -> Right $ (div n1 n2)
        (Left err1, Left err2) -> Left (err1 ++ err2)
        (Left err1, Right n) -> Left err1
        (Right n, Left err2) -> Left err2