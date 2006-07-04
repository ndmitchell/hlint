
module Core.CoreType where

-- while it may seem tempting to add type signatures to Core
-- it won't work - by this stage all the type signatures are
-- wrong because of desugarring


-- module name, items in the module
data Core = Core String [CoreItem]
            deriving (Show, Read)


data CoreItem = CoreFunc CoreExpr CoreExpr
              | CoreData String [CoreCtor]
                deriving (Show, Read)


-- Name, then list of maybe field names
data CoreCtor = CoreCtor String [Maybe String]
                deriving (Show, Read)


data CoreExpr = CoreCon String
              | CoreVar String
              | CoreApp CoreExpr [CoreExpr]
              | CoreInt Int
              | CoreInteger Integer
              | CoreChr Char
              | CoreStr String
              | CoreCase CoreExpr [(CoreExpr,CoreExpr)]
              | CoreLet [CoreItem] CoreExpr
              | CorePos String CoreExpr
                deriving (Show, Read)
