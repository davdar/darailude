module FP.Pretty.Console where

import FP.Prelude
import FP.Pretty.Color
import FP.Pretty.Pretty

sgrLeader ∷ 𝕊
sgrLeader = "\ESC["

sgrCloser ∷ 𝕊
sgrCloser = "m"

sgrReset ∷ 𝕊
sgrReset = sgrLeader ⧺ "0" ⧺ sgrCloser

sgrFg ∷ Color → 𝕊
sgrFg = (⧺) "38;5;" ∘ 𝕤 ∘ show ∘ colorCode

sgrBg ∷ Color → 𝕊
sgrBg = (⧺) "48;5;" ∘ 𝕤 ∘ show ∘ colorCode

sgrUl ∷ 𝕊
sgrUl = "4"

sgrBd ∷ 𝕊
sgrBd = "1"

data FormatState = FormatState
  { formatFG ∷ Maybe Color
  , formatBG ∷ Maybe Color
  , formatUL ∷ Bool
  , formatBD ∷ Bool
  }

sgrFormat ∷ FormatState → 𝕊
sgrFormat (FormatState fg bg ul bd) = concat
  [ sgrLeader 
  , concat $ intersperse ";" $ mconcat
      [ elimMaybe [] return $ sgrFg ^$ fg
      , elimMaybe [] return $ sgrBg ^$ bg
      , if ul then [sgrUl] else [] 
      , if bd then [sgrBd] else [] 
      ]
  , sgrCloser
  ]

updateFormat ∷ Format → FormatState → FormatState
updateFormat (FormatFG c) fmt = fmt { formatFG = Just c }
updateFormat (FormatBG c) fmt = fmt { formatBG = Just c }
updateFormat FormatUL fmt = fmt { formatUL = True }
updateFormat FormatBD fmt = fmt { formatBD = True }

formatConsole ∷ [Format] → ReaderT FormatState (Writer 𝕊) () → ReaderT FormatState (Writer 𝕊) ()
formatConsole fmt aM = do
  local (compose $ map updateFormat fmt) $ do
    tell *$ sgrFormat ^$ ask
    aM
  tell $ sgrReset
  tell *$ sgrFormat ^$ ask

renderConsoleM ∷ PrettyOut → ReaderT FormatState (Writer 𝕊) ()
renderConsoleM (ChunkOut c) = tell $ renderChunk c
renderConsoleM (FormatOut f o) = formatConsole f $ renderConsoleM o
renderConsoleM NullOut = tell ""
renderConsoleM (AppendOut o₁ o₂) = renderConsoleM o₁ ≫ renderConsoleM o₂

renderConsole ∷ PrettyOut → 𝕊
renderConsole = execWriter ∘ runReaderTWith (FormatState Nothing Nothing False False) ∘ renderConsoleM

pprintWith ∷ (Pretty a) ⇒ (PrettyM () → PrettyM ()) → a → IO ()
pprintWith f = printLn ∘ renderConsole ∘ renderDoc ∘ ppFinal ∘ Doc ∘ f ∘ runDoc ∘ pretty

pprintWidth ∷ (Pretty a) ⇒ ℕ → a → IO ()
pprintWidth = pprintWith ∘ local ∘ update maxColumnWidthL

pprintRibbon ∷ (Pretty a) ⇒ ℕ → a → IO ()
pprintRibbon = pprintWith ∘ local ∘ update maxRibbonWidthL

pprint ∷ (Pretty a) ⇒ a → IO ()
pprint = pprintWith id

ptrace ∷ (Pretty a) ⇒ a → b → b
ptrace a b = unsafePerformIO $ do
  pprint a
  return b

ptraceM ∷ (Monad m,Pretty a) ⇒ a → m ()
ptraceM x = ptrace x $ return ()
