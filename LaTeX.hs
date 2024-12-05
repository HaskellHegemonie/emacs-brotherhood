module Main where
import Text.Printf
import Symbols
import System.IO
import System.FilePath
import System.Directory

dir = "templates"
names = map (dir </>) ["hshegMathLT.org", "hshegTextLT.org"]
main = do
  createDirectoryIfMissing True dir
  handles <- mapM (`openFile` WriteMode) names
  mapM_ (uncurry hPutStr) $ zip handles $ map unlines [mathStr, textStr] 
  mapM_ hClose handles 
  pure ()

mathStr :: [String]
mathStr =  map (uncurry $ printf "#+latex_header_extra: \\DeclareUnicodeCharacter{%X}{\\%s}" ) symbolAlist

textStr :: [String]
textStr =  map (uncurry $ printf "#+latex_header_extra: \\DeclareUnicodeCharacter{%X}{$\\%s$}" ) symbolAlist
