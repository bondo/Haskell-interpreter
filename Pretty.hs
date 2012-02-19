{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Pretty where

import Language
import Text.PrettyPrint

-- | Things that can be pretty-printed.
class Pretty a where
    -- | Pretty-print something in isolation.
    pretty :: a -> Doc
    prettyParens :: a -> Doc
    prettyParens = parens . pretty

instance Pretty a => Pretty (Program a) where
    pretty = hsep . punctuate semi . map pretty

instance Pretty a => Pretty (ScDef a) where
    pretty (name, args, exp) =
        hsep [ pretty name,
               hsep $ map pretty args,
               char '=',
               pretty exp ]

instance Pretty Int where
    pretty = int

instance Pretty Name where
    pretty = text

instance Pretty a => Pretty [(a, Exp a)] where
    pretty = hsep . punctuate semi . map pretty

instance Pretty a => Pretty (a, Exp a) where
    pretty (name, expr) =
        pretty name <+> equals <+> pretty expr

instance Pretty a => Pretty [Alter a] where
    pretty alts = hsep . punctuate semi $ map pretty alts

instance Pretty a => Pretty (Alter a) where
    pretty (num, ids, exp) =
        hsep [ char '<' <> int num <> char '>',
               hsep . map pretty $ ids,
               text "->", pretty exp ]

instance Pretty a => Pretty (Exp a) where
    pretty (EVar var) = pretty var
    pretty (ENum n) = int n
    --pretty (EConstr tag ari
    pretty (EAp fun body) = pretty fun <+> prettyParens body
    pretty (ELet isRec defs exp) =
        hsep [ text keyword,
               nest 4 $ pretty defs,
               text "in", pretty exp ]
        where keyword | isRec = "letrec"
                      | otherwise = "let"
    pretty (ECase exp alts) =
        hsep [ text "case", pretty exp, text "of", pretty alts ]
    pretty (ELam formals exp) =
        hcat [ char '\\',
               hsep . punctuate comma $ map pretty formals,
               text "->", pretty exp ]

    prettyParens e = if isAtomicExp e
                     then pretty e
                     else parens $ pretty e
