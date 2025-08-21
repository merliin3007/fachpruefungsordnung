{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.ToLaTeX.Testing
    ( testThis
    , readText
    , runTestToPDF
    , runTestToLaTeX
    , tmp
    )
where

import Control.Monad.State (runState)
import qualified Data.ByteString.Lazy as BS
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as LTIO
import Language.Ltml.Parser.Common.Lexeme (nSc)
import Language.Ltml.Parser.Footnote (unwrapFootnoteParser)
import Language.Ltml.Parser.Section (sectionP)
import Language.Ltml.ToLaTeX (generatePDFFromSection)
import Language.Ltml.ToLaTeX.Renderer (renderLaTeX)
import Language.Ltml.ToLaTeX.ToLaTeXM (ToLaTeXM (toLaTeXM))
import Language.Ltml.ToLaTeX.Type
import System.IO.Unsafe (unsafePerformIO)
import Text.Megaparsec (MonadParsec (eof), errorBundlePretty, runParser)

import Language.Lsd.Example.Fpo (footnoteT, sectionT)
import Language.Ltml.ToLaTeX.Format (staticDocumentFormat)
import Language.Ltml.ToLaTeX.GlobalState
    ( GlobalState (_labelToFootNote, _labelToRef)
    , initialGlobalState
    )
import System.IO

readText :: String -> Text
readText filename = unsafePerformIO $ TIO.readFile filename

testThis :: (ToLaTeXM a) => a -> (LaTeX, GlobalState)
testThis a =
    runState
        (toLaTeXM a)
        initialGlobalState

runTestToPDF :: IO ()
runTestToPDF = do
    let txt = readText "./src/Language/Ltml/ToLaTeX/Auxiliary/test.txt"
    eAction <- generatePDFFromSection txt
    case eAction of
        Left err -> error err
        Right pdf -> BS.writeFile "./src/Language/Ltml/ToLaTeX/Auxiliary/test.pdf" pdf

runTestToLaTeX :: IO String
runTestToLaTeX = do
    let input = readText "./src/Language/Ltml/ToLaTeX/Auxiliary/test.txt"
    case runParser
        (nSc *> unwrapFootnoteParser [footnoteT] (sectionP sectionT eof))
        ""
        (input <> "\n") of
        Left err -> return (errorBundlePretty err)
        Right parsedInput -> do
            let texFile = "./src/Language/Ltml/ToLaTeX/Auxiliary/test.tex"
            -- Write LaTeX source
            LTIO.writeFile texFile (sectionToText parsedInput)
            return "everything went well!"
  where
    sectionToText (sec, labelmap) =
        let (latexSection, gs) = runState (toLaTeXM sec) $ initialGlobalState {_labelToFootNote = labelmap}
         in renderLaTeX (_labelToRef gs) (staticDocumentFormat <> document latexSection)

tmp :: IO ()
tmp = do
    h <- openFile "./src/Language/Ltml/ToLaTeX/Auxiliary/test.tex" ReadMode
    hSetEncoding h utf8
    contents <- hGetContents h
    putStrLn contents
