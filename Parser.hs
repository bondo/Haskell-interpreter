module Parser (parseCore) where 

-- Use http://hackage.haskell.org/package/haskell-src/ to parse full
-- haskell

import Language

import Text.Parsec
import Text.Parsec.String -- has a parseFromFile
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskell)

import Control.Monad

identifier = P.identifier haskell
reserved   = P.reserved haskell
whiteSpace = P.whiteSpace haskell
parens     = P.parens haskell
integer    = P.integer haskell
reservedOp = P.reservedOp haskell
semi       = P.semi haskell
comma      = P.comma haskell
             
int :: Parser Int
int = liftM fromInteger integer

parseCore :: String -> Either ParseError CoreProgram
parseCore = runParser pProgram () "filename.nice"

pProgram :: Parser CoreProgram
pProgram = do whiteSpace
              scs <- sepBy1 pSc semi
              eof
              return scs
           <?> "Program"

pSc :: Parser CoreScDef
pSc = do name <- identifier
         args <- many identifier
         reservedOp "="
         exp <- pExp
         return (name, args, exp)
      <?> "Supercombinator"

pExp :: Parser CoreExp
pExp =  pApp
--  <|> pBinApp
    <|> pLet
    <|> pCase
    <|> pLam
    <|> pAExp
    <?> "Expression"

pAExp :: Parser CoreExp
pAExp =  pVar
     <|> pNum
     <|> pConstr
     <|> parens pExp
     <?> "(A)Expression"

pApp :: Parser CoreExp
pApp = liftM (foldl1 EAp) (many1 pAExp)
       <?> "Application"

pVar :: Parser CoreExp
pVar = liftM EVar identifier
       <?> "Variabel"

pNum :: Parser CoreExp
pNum = liftM ENum int
       <?> "Number"

pLet :: Parser CoreExp
pLet = do isRec <- (reserved "let"    >> return False)
               <|> (reserved "letrec" >> return True)
          defs <- pDefs
          reserved "in"
          exp <- pExp
          return $ ELet isRec defs exp
       <?> "Let(rec) expression"

pDefs :: Parser [(Name, CoreExp)]
pDefs = sepBy1 pDef semi
        <?> "List of bindings"

pDef :: Parser (Name, CoreExp)
pDef = try (do var <- identifier
               reservedOp "="
               exp <- pExp
               return (var, exp))
       <?> "Let(rec) binding"

pCase :: Parser CoreExp
pCase = do reserved "case"
           exp <- pExp
           reserved "of"
           reservedOp "{"
           alts <- pAlts
           reservedOp "}"
           return $ ECase exp alts
        <?> "Case expression"

pAlts :: Parser [CoreAlt]
pAlts = sepBy1 pAlt semi

pAlt :: Parser CoreAlt
pAlt = do reservedOp "<"
          num <- int
          reservedOp ">"
          ids <- many identifier
          reservedOp "->"
          exp <- pExp
          return (num, ids, exp)
       <?> "Case alternative"

pLam :: Parser CoreExp
pLam = do reserved "\\"
          args <- sepBy1 identifier comma
          reserved "->"
          body <- pExp
          return $ ELam args body
       <?> "Lambda expression"

pConstr :: Parser CoreExp
pConstr = (reserved "@" >> liftM2 EConstr int int)
          <?> "Constructor expression"
