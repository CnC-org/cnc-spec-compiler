{
{-# LANGUAGE DeriveDataTypeable #-}
module Intel.Cnc.Spec.CncGrammar where

import Intel.Cnc.Spec.CncLexer hiding (main)
import Intel.Cnc.Spec.AST
import Intel.Cnc.Spec.SrcLoc

--import Data.Char
import StringTable.Atom
--import Data.Data
import Text.PrettyPrint.HughesPJClass
import Debug.Trace

}
-- (Based on example from Simon Marlow.)

-- These are similar macros to those used by the GHC parser:
-- define L0 L noSrcSpan
-- define L1 sL (getLoc $1)

-- Both arguments are Lexemes:

-- One or both arguments can be Decorated instead:




-- Here is a praticularly painful special case where we have a possibly
-- empty list on the right end. We take any source info that is there
-- and fall back to the second to last token otherwise.


-- For now we enable BOTH the new syntax and the legacy one:


-- First thing to declare is the name of your parser,
-- and the type of the tokens the parser reads.

%name parse_cnc
%tokentype { Lexeme }

-- The parser will be of type [Token] -> ?, where ? is determined by the
-- production rules. Now we declare all the possible tokens:

%token

 tuple { L _ LVarId "tuple" }

 var { L _ LVarId _ }
 qvar { L _ LQVarId _ }
 int { L _ LInteger _ }

 "->" { L _ LReservedOp "->" }
 "<-" { L _ LReservedOp "<-" }
 "::" { L _ LReservedOp "::" }

-- ':' { L _ LReservedOp ":" }
 ':' { L _ LSpecial ":" }

 '(' { L _ LSpecial "(" }
 ')' { L _ LSpecial ")" }
 '[' { L _ LSpecial "[" }
 ']' { L _ LSpecial "]" }
 ';' { L _ LSpecial ";" }
 ',' { L _ LSpecial "," }
 '<' { L _ LReservedOp "<" }
 '>' { L _ LReservedOp ">" }

 '=' { L _ LVarOp "=" }
 '+' { L _ LVarOp "+" }
 '-' { L _ LVarOp "-" }
-- '*' { L _ LVarOp "*" }
 '*' { L _ LSpecial "*" }
 '/' { L _ LVarOp "/" }
 op { L _ LVarOp _ }

-- step { L _ LReservedId "step" }
 mod { L _ LReservedId "module" }

 tags { L _ LReservedId "tags" }
 items { L _ LReservedId "items" }
 steps { L _ LReservedId "steps" }
 reductions { L _ LReservedId "reductions" }
 dense { L _ LReservedId "dense" }
 prescribes { L _ LReservedId "prescribes" }

 type { L _ LReservedId "type" }

-- FUTURE WORK: Reserving space in the grammar for presently
-- unimplemented features [2011.04.12]:
 constrain { L _ LReservedId "constrain" }

        eof { L _ LEOF _ }

-- comment { L _ LComment _ }


-- The left hand side are the names of the terminals or tokens,
-- and the right hand side is how to pattern match them.

%nonassoc '<' '>' '<=' '>=' '==' '='
%left '+' '-'
%left '*' '/'

-- Like yacc, we include %% here, for no real reason.
%%

-- Now the production rules.
----------------------------------------------------------------------------------------------------


File :: { [PStatement SrcSpan] }
File : Statements { $1 }

Statements : Statement Statements { $1 ++ $2 }
           | Statement { $1 }
           | eof { [] }

-- Statement : Terminated_Relation { [$1] }
-- | Terminated_Decl { $1 }
-- Terminated_Relation : Relation ';' { $1 }
-- Terminated_Decl : Decl ';' { $1 }

Statement : Relation ';' { [$1] }
           | Decl ';' { $1 }

           | type var '=' Type ';' { [TypeDef (lexPointSpan $2) (tAL $2) $4] }
-- | type var Type ';' { [TypeDef (lexPointSpan $2) (tAL $2) $3] }

           -- These are just for better errors:
           | Relation eof { parseErrorSDoc (getDecor $1) $ text "Premature end of file, possible missing semi-colon." }
           | Decl eof { parseErrorSDoc (getDecorLs $1) $ text "Premature end of file, possible missing semi-colon." }

-- reduce/reduce conflict
-- | Relation Instance { parseErrorSDoc (getDecor $2) $ text "Possible missing semi-colon." }

Decl :: { [PStatement SrcSpan] }
Decl
  : steps VarLs { map (\x -> DeclareSteps (lexSpan $1) (tAL x)) $2 }

                                           -- Here we try particularly hard to get good source location info:
  | constrain Instance ':' TagExps { [Constraints (cLLS $1 (lexSpan $3) $4) $2 $4] }
  -- One additional shift/reduce conflict if we do not use a separator:
  | constrain Instance TagExps { [Constraints (cLLS $1 (getDecor $2) $3) $2 $3] }
  --| constrain TagExps { [Constraints (lexSpan $1) (InstName "foo") []] }
  --| constrain Instance { [Constraints (lexSpan $1) (InstName "foo") []] }
  --| constrain var { [Constraints (lexSpan $1) (InstName "foo") []] }

{- #if 0 -}
  | Mods tags var { [DeclareTags (cLL $2 $3) (tAL $3) Nothing] }
  -- [2010.07.20] I am having a strange problem making Mods optional:
-- | Mods tags '<' Type '>' var { [DeclareTags (lexSpan $2) (lexStr $6) (Just $4)] }
  | Mod Mods tags '<' Type '>' var { [DeclareTags (cLL $3 $7) (tAL $7) (Just $5)] }
  | tags '<' Type '>' var { [DeclareTags (combineSrcSpans (lexSpan $1) (lexSpan $>)) (tAL $5) (Just $3)] }

-- TODO: Rearrange this so that the arbitrary numbers of template args
-- are accepted and an error is thrown if the wrong number are present:

  | Mods items var { [DeclareItems (cLL $2 $3) (tAL $3) Nothing] }
  | Mods items '<' Type ',' Type '>' var { [DeclareItems (cLL $2 $8) (tAL $8) (Just ($4, $6))] }

  -- Reductions are similar to item collections except they are parameterized by a reduction op.
  -- | Mods reductions var '(' var ')' { [DeclareReductions (cLL $2 $3) (tAL $3) (tAL $5) Nothing] }
  -- | Mods reductions '<' Type ',' Type '>' var '(' var ')'
  -- { [DeclareReductions (cLL $2 $8) (tAL $8) (tAL $10) (Just ($4, $6))] }

  | Mods reductions var '(' var ',' Exp ')' { [DeclareReductions (cLL $2 $3) (tAL $3) (tAL $5) $7 Nothing] }
  | Mods reductions '<' Type ',' Type '>' var '(' var ',' Exp ')'
                                            { [DeclareReductions (cLL $2 $10) (tAL $8) (tAL $10) $12 (Just ($4, $6))] }



  | '<' Type var '>' { [DeclareTags (combineSrcSpans (lexSpan $1) (lexSpan $>)) (tAL $3) (Just $2)] }
  -- Inexplicable problem with this TagExps version, maybe because of '>' not being special...
  --| '<' Type var ':' TagExps '>' { [DeclareTags (lexSpan $3) (tAL $3) (Just $2)] }
  | '<' Type var ':' VarsOnlyHack '>' { [DeclareTags (combineSrcSpans (lexSpan $1) (lexSpan $>)) (tAL $3) (Just $2)] }

  | '[' Type var '<' Type '>' ':' TagExps ']' { [DeclareItems (combineSrcSpans (lexSpan $1) (lexSpan $>)) (tAL $3) (Just ($5, $2))] }
  | '[' Type var '<' Type '>' ']' { [DeclareItems (combineSrcSpans (lexSpan $1) (lexSpan $>)) (tAL $3) (Just ($5, $2))] }



VarLs : var { [$1] }
      | var ',' VarLs { $1 : $3 }

-- Modifier keywords can precede declarations.
Mods : {- empty -} { [] }
     | Mod Mods { $1 : $2 }
Mod : dense { "dense" }

Relation :: { PStatement SrcSpan }
Relation : Instances Chain { Chain $1 $2 }

Chain :: { [RelLink SrcSpan] }
Chain : { [] }
-- | Link { [$1] }
      | Link Chain { $1 : $2 }
Link : "->" Instances { ProduceLink (lexSpan $1) $2 }
      | "<-" Instances { RevProduceLink (lexSpan $1) $2 }
      | "::" Instances { PrescribeLink (lexSpan $1) $2 }
      | prescribes Instances { PrescribeLink (lexSpan $1) $2 }

Instances :: { [CollectionInstance SrcSpan] }
Instances
  : { [] }
  | Instance { [$1] }
  | Instance ',' Instances { $1 : $3 }

Instance
  : var { InstName (lexSpan $1) (lexStr $1) }
  | var '[' TagExps ']' { InstItemCol (combineSrcSpans (lexSpan $1) (lexSpan $>)) (lexStr $1) $3 }
-- | var '<' TagExps '>' { InstStepOrTags (combineSrcSpans (lexSpan $1) (lexSpan $>)) (lexStr $1) $3 }
  | var '(' TagExps ')' { InstStepOrTags (combineSrcSpans (lexSpan $1) (lexSpan $>)) (lexStr $1) $3 }

  | '<' Var '>' { InstTagCol (combineSrcSpans (lexSpan $1) (lexSpan $>)) (lexStr $2) [] }
  | '(' Var ')' { InstStepCol (combineSrcSpans (lexSpan $1) (lexSpan $>)) (lexStr $2) [] }
  | '[' Var ']' { InstItemCol (combineSrcSpans (lexSpan $1) (lexSpan $>)) (lexStr $2) [] }
  -- TEMP FIXME: Again, problem here with full tag exps:
  | '<' Var ':' VarsOnlyHack '>' { InstTagCol (combineSrcSpans (lexSpan $1) (lexSpan $>)) (lexStr $2) $4 }
  | '(' Var ':' TagExps ')' { InstStepCol (combineSrcSpans (lexSpan $1) (lexSpan $>)) (lexStr $2) $4 }
  | '[' Var ':' TagExps ']' { InstItemCol (combineSrcSpans (lexSpan $1) (lexSpan $>)) (lexStr $2) $4 }


TagExps :: { [Exp SrcSpan] }
TagExps : { [] }
        | Exp { [$1] }
 | Exp ',' TagExps { $1 : $3 }

-- TEMP, HACK:
VarsOnlyHack : { [] }
     | var { [Var (lexSpan $1) (tAL $1)] }
     | var ',' VarsOnlyHack { Var (lexSpan $1) (tAL $1) : $3 }


-- This is just for catching errors:
Var : var { $1 }
   -- This error checking must be introduced CAREFULLY or it can yield errors. Not every position can be checked.

    -- Sadly, this error checking should go EVERYWHERE:
    | tags { parseErrorSDoc (lexSpan $1) $ text "Keyword 'tags' used incorrectly." }
    | items { parseErrorSDoc (lexSpan $1) $ text "Keyword 'items' used incorrectly." }
    | reductions { parseErrorSDoc (lexSpan $1) $ text "Keyword 'reductions' used incorrectly." }
    | steps { parseErrorSDoc (lexSpan $1) $ text "Keyword 'steps' used incorrectly." }
    | dense { parseErrorSDoc (lexSpan $1) $ text "Keyword 'dense' used incorrectly." }
    | prescribes { parseErrorSDoc (lexSpan $1) $ text "Keyword 'prescribes' used incorrectly." }
    | constrain { parseErrorSDoc (lexSpan $1) $ text "Keyword 'constrain' used incorrectly." }
    | mod { parseErrorSDoc (lexSpan $1) $ text "Keyword 'module' used incorrectly." }
-- | step { parseErrorSDoc (lexSpan $1) $ text "Keyword 'step' used incorrectly." }


Exp :: { Exp SrcSpan } -- The haskell type of the result of parsing this syntax class.

Exp : var { Var (lexSpan $1) (tAL $1) }
    | qvar { Var (lexSpan $1) (tAL $1) }
    | int { Lit (lexSpan $1) (LitInt $ read (lexStr $1)) }
    | '(' Exp ')' { $2 }

    -- Function application:
    | var '(' TagExps ')' { App (combineSrcSpans (lexSpan $1) (getDecor (last $3)))
          (Var (lexSpan $1) (tAL $1)) $3 }

    -- Tuples? *** TODO ***.


    -- Including explicit productions for arithmetic just to handle precedence/associativity:
    | Exp '+' Exp { App (combExpSpans $1 $3) (Var (combineSrcSpans (getDecor $1) (getDecor $>)) (toAtom "+")) [$1, $3] }
    | Exp '-' Exp { App (combExpSpans $1 $3) (Var (combineSrcSpans (getDecor $1) (getDecor $>)) (toAtom "-")) [$1, $3] }
    | Exp '*' Exp { App (combExpSpans $1 $3) (Var (combineSrcSpans (getDecor $1) (getDecor $>)) (toAtom "*")) [$1, $3] }
    | Exp '/' Exp { App (combExpSpans $1 $3) (Var (combineSrcSpans (getDecor $1) (getDecor $>)) (toAtom "/")) [$1, $3] }

    -- These need to be handled because they are lexed differently, being reserved characters:
    | Exp '<' Exp { App (combExpSpans $1 $3) (Var (combineSrcSpans (getDecor $1) (getDecor $>)) (toAtom "<")) [$1, $3] }
    | Exp '>' Exp { App (combExpSpans $1 $3) (Var (combineSrcSpans (getDecor $1) (getDecor $>)) (toAtom ">")) [$1, $3] }

    | Exp op Exp { App (combExpSpans $1 $3) (Var (combineSrcSpans (getDecor $1) (getDecor $>)) (tAL $2)) [$1, $3] }

Type
    : var { TSym (toAtom $ lexStr $1) }
    | dense Type { TDense $2 }
    | Type '*' { TPtr $1 }
    | '(' Types ')' { TTuple $2 }
    | tuple '<' Types '>' { TTuple $3 }

Types : { [] }
      | Type { [$1] }
      | Type ',' Types { $1 : $3 }


-- We are simply returning the parsed data structure! Now we need
-- some extra code, to support this parser, and make in complete:

----------------------------------------------------------------------------------------------------
{

-- Combine two lexical tokens for a source span:
cLL a b = (lexSpan a) `combineSrcSpans` (lexSpan b)
-- more obscure, combine two tokens and the first (if it exists) of a list:
cLLS a b c = combineSrcSpans (lexSpan a) $ combineSrcSpans b (getDecorLs c)

getDecorLs [] = srcLocSpan noSrcLoc
getDecorLs (h:t) = getDecor h


-- All parsers must declair this function, which is called when an error
-- is detected. Note that currently we do no error recovery.
happyError :: [Lexeme] -> a

happyError [] = error "Parse error.  Strange - it's not before any token that I know of..."
happyError ls =
 let loc = lexLoc $ head ls in
 error$ "Parse error before token at location : \n   " ++
        show (pPrint loc) ++
 (if srcColumn loc <= 1
  then "\n(An error at the beginning of the line like this could be a missing semi-colon on the previous line.)\n"
  else "")

parseErrorSDoc span doc =
   error $ show $ text "\n\nPARSE ERROR!\n  " <> doc $$
           text "At location: " <> pPrint span

-- Now we declare the datastructure that we are parsing.

runCncParser :: String -> String -> [PStatement SrcSpan]
runCncParser file str =
   let notcomment = filter (not . is_comment) $ scan_to_list str in

   -- FIXME:
   -- Here's a hack that's a bit ineffecient. We POST-FACTO put the right filename in the
   -- sourceloc decorations. It would be better to do it right the first time.
   --
   -- NOTE [2010.07.23] This has another problem. Parse errors will say "unknown file".
   -- I guess I need to thread through a reader monad.
   map (mapDecor (srcSpanSetFileName file)) $
   -- For now filter out comments before parsing:
   if null notcomment
   then error "ERROR: Specification file contains no CnC statements!"
   else parse_cnc $ notcomment

is_comment ( L _ LComment _ ) = True
is_comment _ = False

lexStr :: Lexeme -> String
lexStr (L _ _ str) = str

tAL = toAtom . lexStr

lexLoc :: Lexeme -> SrcLoc
lexLoc (L (AlexPn n l c) _ _) = (SrcLoc "" l c)

lexSpan :: Lexeme -> SrcSpan
-- [2010.07.23] We can do a little better by looking at the length of
-- the string and stretching the src location to include all of it.
-- DANGER, we assume that Lexemes stay on one line!! (not true of multiline comments)
lexSpan (L (AlexPn n l c) _ str) =
   let start = mkSrcLoc "" l c
       end = mkSrcLoc "" l (c + length str)
   in srcLocSpan start `combineSrcSpans` srcLocSpan end

-- Not a span, just a single point:
--lexPointSpan (L (AlexPn n l c) _ _) = srcLocSpan (SrcLoc "unknownfile" l c)
lexPointSpan = srcLocSpan . lexLoc

-- Combine the spans in two expressions.
combExpSpans e1 e2 = combineSrcSpans (getDecor e1) (getDecor e2)

quit = print "runCnc failed\n"

}
