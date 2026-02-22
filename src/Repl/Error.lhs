This module defines the REPL error model exposed through the FFI layer. It keeps error classification explicit and rendering consistent for callers.

\begin{code}
module Repl.Error (
  ReplError(..),
  prettyReplError
) where
import qualified Prelude ()
import MHSPrelude
\end{code}

\verb|ReplError| classifies failures by pipeline stage. Separating parse, compile, and runtime failures makes error handling and messaging clearer across module boundaries.

\begin{code}
data ReplError
  = ReplParseError String
  | ReplCompileError String
  | ReplRuntimeError String
  deriving (Eq, Show)
\end{code}

\verb|prettyReplError| converts structured errors into plain, user-facing text. It prepends a stable stage prefix while preserving the original detail payload.

\begin{code}
prettyReplError :: ReplError -> String
prettyReplError e =
  case e of
    ReplParseError s   -> "Parse error: "   ++ s
    ReplCompileError s -> "Compile error: " ++ s
    ReplRuntimeError s -> "Runtime error: " ++ s
\end{code}

