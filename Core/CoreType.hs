
module Core.CoreType where

-- while it may seem tempting to add type signatures to Core
-- it won't work - by this stage all the type signatures are
-- wrong because of desugarring


-- module name, imports, items in the module
data Core = Core String [String] [CoreItem]
            deriving (Show, Read, Eq)


data CoreItem = CoreFunc CoreExpr CoreExpr
              | CoreData String [String] [CoreCtor]
                deriving (Show, Read, Eq)


-- Name, then list of maybe field names
data CoreCtor = CoreCtor String [(String, Maybe String)]
                deriving (Show, Read, Eq)


data CoreExpr = CoreCon String
              | CoreVar String
              | CoreApp CoreExpr [CoreExpr]
              | CoreCase CoreExpr [(CoreExpr,CoreExpr)]
              | CoreLet [CoreItem] CoreExpr
              | CorePos String CoreExpr
              
              | CoreInt Int
              | CoreInteger Integer
              | CoreChr Char
              | CoreStr String
              | CoreFloat Float
              | CoreDouble Double
                deriving (Show, Read, Eq)
