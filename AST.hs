{-# LANGUAGE DeriveDataTypeable #-}
module AST where 
--module Intel.Cnc.Translator.AST where 

import StringTable.Atom
import Data.Data
import Text.PrettyPrint.HughesPJClass
import Data.Generics.Serialization.SExp
import Data.Generics.Serialization.Streams



data Lit = LitInt Int | LitFloat Float
 deriving (Eq, Ord, Show, Data, Typeable)

-- Expressions are decorated with values of an arbitrary type:
data Exp dec = 
   Lit dec Lit
 | Var dec String
 | App dec (Exp dec) [Exp dec]
 deriving (Eq, Ord, Show, Data, Typeable)


instance Pretty Lit where 
 pPrint (LitInt i)   = pPrint i
 pPrint (LitFloat f) = pPrint f

instance Pretty (Exp dec) where 
 pPrint (Lit _ l) = pPrint l
 pPrint (Var _ s) = text s
 pPrint (App _ rator rands) = 
     parens $  
        pPrint rator <+> sep (map pPrint rands)
--      sep (pPrint rator : map pPrint rands)


-- data SrcLoc = SrcLoc {
-- 		srcFilename :: String,
-- 		srcLine     :: Int,
-- 		srcColumn   :: Int
-- 		}
--  deriving (Eq,Ord,Show,Typeable,Data)


 -- I don't actually see why we would need interned strings for
 -- filenames.  How many unique files are they?  They should be shared
 -- properly even as normal strings.  And how often do they need to be
 -- compared?

data SrcLoc
--  = SrcLoc	Atom	-- A precise location (file name)
  = SrcLoc	String	-- A precise location (file name)
		{-# UNPACK #-} !Int		-- line number, begins at 1
		{-# UNPACK #-} !Int		-- column number, begins at 1
 deriving (Eq,Ord,Show,Data,Typeable)

--data Loc a = Loc SrcLoc a  deriving (Eq,Ord,Show)

unknownLoc = SrcLoc "" 0 0 

-- This is the price of tagging the locs right on the Exprs rather
-- than the even/odd alternating location tags.
getLoc e = 
 case e of 
   Lit s _        -> s
   Var s _        -> s
   App s _ _      -> s

