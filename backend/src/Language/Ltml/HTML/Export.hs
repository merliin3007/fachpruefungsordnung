{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.HTML.Export (exportDocument) where

import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (runState)
import Language.Ltml.AST.Document
import Language.Ltml.HTML
import Language.Ltml.HTML.CSS.Util
import Language.Ltml.HTML.Common
import Language.Ltml.HTML.Util
import Lucid
import System.Directory
import System.FilePath

-- ReaderState felder mit wrapperFunktionen für:
--  - <a> für Section Headings (Sprung zur Einzelansicht)

-- | Path to main CSS file relative to the index.html
relativeCssFilePath :: FilePath
relativeCssFilePath = "css" </> "style.css"

-- | Directory which holds all subppages relative to the index.html
relativeSectionsDir :: FilePath
relativeSectionsDir = "sections"

-------------------------------------------------------------------------------

-- | Exports document structure as HTML pages to given directory path
exportDocument :: Document -> FilePath -> IO ()
exportDocument doc@(Document format header (DocumentBody nodeSections)) path =
    let absCssFilePath = path </> relativeCssFilePath
        absSectionsDir = path </> relativeSectionsDir
     in do
            createDirectoryIfMissing True path
            createDirectoryIfMissing True (takeDirectory absCssFilePath)
            createDirectoryIfMissing True absSectionsDir
            -- TODO: this has to build the final css based on the rendering
            -- writeCss absCssFilePath
            -- \| TODO: Add actual Document title
            renderToFile
                (path </> "index.html")
                (addHtmlHeader "Tolles Dokument" relativeCssFilePath $ aToHtml doc)
            mapState (exportSingleSection absSectionsDir) initGlobalState nodeSections

-- | Render section with given initial state and creates .html file
-- in given directory; returns the final state
exportSingleSection
    :: (ToHtmlM a) => FilePath -> GlobalState -> a -> IO GlobalState
exportSingleSection path globalState a =
    let (delayedHtml, finalState) = runState (runReaderT (toHtmlM a) initReaderState) globalState
        body = evalDelayed delayedHtml finalState
        sectionID = show (currentSectionID globalState)
     in do
            renderToFile (path </> ("section_" ++ sectionID ++ ".html")) $
                addHtmlHeader
                    ("Einzelansicht § " ++ sectionID)
                    (".." </> relativeCssFilePath)
                    body
            return finalState
