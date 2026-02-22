module Repl.Executor (
  replDefine,
  replRun,
  replExecute,
  replInspect,
  replIsComplete
) where

import qualified Prelude ()
import MHSPrelude

import Control.Applicative ((<|>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Data.Maybe (isJust, listToMaybe, mapMaybe)

import MicroHs.Ident (mkIdent)
import MicroHs.SymTab (stLookup, Entry(..))
import MicroHs.Expr (showExpr)
import MicroHs.TypeCheck (Symbols)
import System.IO (putStrLn)

import Repl.Context
import Repl.Error
import Repl.Utils
import Repl.Analysis
import Repl.Compiler

data MetaCommand
  = TypeOfExpr String
  | KindOfType String

data SplitPlan
  = SplitDefineOnly String
  | SplitDefineThenRun String String

data Completion
  = Complete
  | Incomplete
  | Invalid

replDefine :: ReplCtx -> String -> IO (Either ReplError ReplCtx)
replDefine ctx snippet = runExceptT $ do
  let snippetWithNewline = ensureTrailingNewline snippet
  defsWithNew <- ExceptT (pure (appendDefinition ctx snippetWithNewline))
  (_, cache', syms') <- ExceptT (compileModule ctx (moduleFromDefs defsWithNew))
  pure ctx{ rcDefs = defsWithNew, rcCache = cache', rcSyms = syms' }

replRun :: ReplCtx -> String -> IO (Either ReplError ReplCtx)
replRun ctx stmt = runExceptT $ do
  let block = runBlock stmt
      src = moduleSourceWith ctx block
  (cmdl, cache', syms') <- ExceptT (compileModule ctx src)
  _ <- ExceptT (runAction cache' cmdl runResultIdent)
  pure ctx{ rcCache = cache', rcSyms = syms' }

replIsComplete :: ReplCtx -> String -> IO String
replIsComplete ctx snippet = pure (toText completion)
  where
    completion
      | all isws snippet = Complete
      | hasValidSplit = Complete
      | isIncomplete snippet = Incomplete
      | otherwise = Invalid

    hasValidSplit = isJust (firstValidSplitPlan ctx (lines (ensureTrailingNewline snippet)))

    toText state =
      case state of
        Complete -> "complete"
        Incomplete -> "incomplete"
        Invalid -> "invalid"

replInspect :: ReplCtx -> String -> IO (Either ReplError String)
replInspect ctx name = do
  let ident = mkIdent name
      (typeTable, valueTable) = rcSyms ctx
      valueHit = lookupRendered "value" ident valueTable name
      typeHit = lookupRendered "type" ident typeTable name
      notFound = ReplRuntimeError ("Identifier not found: " ++ name)
  pure (maybe (Left notFound) Right (valueHit <|> typeHit))

replExecute :: ReplCtx -> String -> IO (Either ReplError ReplCtx)
replExecute ctx snippet
  | Just cmd <- parseMetaCommand snippet = runMetaCommand ctx cmd
replExecute ctx snippet =
  case firstValidSplitPlan ctx (lines (ensureTrailingNewline snippet)) of
    Nothing -> pure (Left (ReplParseError "unable to parse snippet"))
    Just (SplitDefineOnly defPart) -> replDefine ctx defPart
    Just (SplitDefineThenRun defPart runPart) -> defineThenRun ctx defPart runPart

runMetaCommand :: ReplCtx -> MetaCommand -> IO (Either ReplError ReplCtx)
runMetaCommand ctx cmd =
  case cmd of
    TypeOfExpr expr -> replTypeOf ctx expr
    KindOfType ty -> replKindOf ctx ty

replTypeOf :: ReplCtx -> String -> IO (Either ReplError ReplCtx)
replTypeOf ctx expr =
  runProbe ctx
    ":type expects an expression"
    "failed to infer expression type"
    trimmedExpr
    src
    inferType
  where
    trimmedExpr = trimWs expr
    probeName = "xhReplTypeProbe"
    probeIdent = mkIdent probeName
    probeDef = unlines
      [ probeName ++ " = ("
      , indent trimmedExpr
      , "  )"
      ]
    src = moduleSourceWith ctx probeDef
    inferType (_, valueTable) =
      case stLookup "value" probeIdent valueTable of
        Right (Entry _ sigma) -> Just (showExpr sigma)
        Left _ -> Nothing

replKindOf :: ReplCtx -> String -> IO (Either ReplError ReplCtx)
replKindOf ctx ty =
  runProbe ctx
    ":kind expects a type"
    "failed to infer type kind"
    trimmedType
    src
    inferKind
  where
    trimmedType = trimWs ty
    probeName = "XhReplKindProbe"
    probeIdent = mkIdent probeName
    probeDef = "type " ++ probeName ++ " = " ++ trimmedType ++ "\n"
    src = moduleSourceWith ctx probeDef
    inferKind (typeTable, _) =
      case stLookup "type" probeIdent typeTable of
        Right (Entry _ kind) -> Just (showExpr kind)
        Left _ -> Nothing

parseMetaCommand :: String -> Maybe MetaCommand
parseMetaCommand raw =
  parseWith ":type" TypeOfExpr snippet <|> parseWith ":kind" KindOfType snippet
  where
    snippet = trimWs raw

    parseWith keyword ctor s =
      if startsWith keyword s && hasBoundary keyword s
        then Just (ctor (drop (length keyword) s))
        else Nothing

    hasBoundary keyword s =
      let rest = drop (length keyword) s
      in null rest || isws (head rest)

startsWith :: String -> String -> Bool
startsWith prefix s = take (length prefix) s == prefix

trimWs :: String -> String
trimWs = dropWhile isws . dropWhileEnd isws

classifySplit :: ReplCtx -> [String] -> Int -> Maybe SplitPlan
classifySplit ctx snippetLines splitIndex
  | not (canParseDefinition candidateDefs) = Nothing
  | all allwsLine runLines = Just (SplitDefineOnly defPart)
  | canParseExpression runPart = Just (SplitDefineThenRun defPart runPart)
  | canParseExpression doRunPart = Just (SplitDefineThenRun defPart doRunPart)
  | otherwise = Nothing
  where
    (defLines, runLines) = splitAt splitIndex snippetLines
    defPart = unlines defLines
    runPart = unlines (dropWhileEnd allwsLine runLines)
    doRunPart = "do\n" ++ indent runPart
    candidateDefs = currentDefsSource ctx ++ defPart

firstValidSplitPlan :: ReplCtx -> [String] -> Maybe SplitPlan
firstValidSplitPlan ctx snippetLines =
  listToMaybe (mapMaybe (classifySplit ctx snippetLines) [length snippetLines, length snippetLines - 1 .. 0])

defineThenRun :: ReplCtx -> String -> String -> IO (Either ReplError ReplCtx)
defineThenRun ctx defPart runPart = runExceptT $ do
  ctx' <- ExceptT (replDefine ctx defPart)
  ExceptT (replRun ctx' runPart)

runProbe
  :: ReplCtx
  -> String
  -> String
  -> String
  -> String
  -> (Symbols -> Maybe String)
  -> IO (Either ReplError ReplCtx)
runProbe ctx emptyInputError inferError shown src infer = runExceptT $ do
  shown' <- ExceptT (pure $
    if null shown
      then Left (ReplRuntimeError emptyInputError)
      else Right shown)
  (_, _, syms) <- ExceptT (compileModule ctx src)
  inferred <- maybe
    (ExceptT (pure (Left (ReplRuntimeError inferError))))
    pure
    (infer syms)
  lift (putStrLn ("> " ++ shown' ++ " :: " ++ inferred))
  pure ctx

lookupRendered scope ident table shown =
  case stLookup scope ident table of
    Right (Entry _ sigOrKind) -> Just (shown ++ " :: " ++ showExpr sigOrKind)
    Left _ -> Nothing
