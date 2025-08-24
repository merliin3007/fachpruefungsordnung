{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.ToLaTeX.ToLaTeXM (ToLaTeXM (..))
where

import Control.Lens (use, (%=), (.=), (.~))
import Control.Monad.State (State, modify)
import qualified Data.DList as DList
import qualified Data.Map as Map
import qualified Data.Text.Lazy as LT
import Data.Void (Void, absurd)
import Language.Lsd.AST.Format
    ( EnumItemKeyFormat (EnumItemKeyFormat)
    , HeadingFormat (HeadingFormat)
    , ParagraphKeyFormat (ParagraphKeyFormat)
    , TocKeyFormat (TocKeyFormat)
    )
import Language.Lsd.AST.Type.AppendixSection
    ( AppendixElementFormat (AppendixElementFormat)
    , AppendixSectionFormat (AppendixSectionFormat)
    , AppendixSectionTitle (AppendixSectionTitle)
    )
import Language.Lsd.AST.Type.Document (DocumentFormat (..))
import Language.Lsd.AST.Type.DocumentContainer
    ( DocumentContainerFormat (DocumentContainerFormat)
    )
import Language.Lsd.AST.Type.Enum
    ( EnumFormat (..)
    , EnumItemFormat (EnumItemFormat)
    )
import Language.Lsd.AST.Type.Paragraph (ParagraphFormat (ParagraphFormat))
import Language.Lsd.AST.Type.Section (SectionFormat (SectionFormat))
import Language.Lsd.AST.Type.SimpleParagraph
    ( SimpleParagraphFormat (SimpleParagraphFormat)
    )
import Language.Lsd.AST.Type.SimpleSection
    ( SimpleSectionFormat (SimpleSectionFormat)
    )
import Language.Ltml.AST.AppendixSection (AppendixSection (AppendixSection))
import Language.Ltml.AST.Document
    ( Document (..)
    , DocumentBody (..)
    , DocumentHeading (DocumentHeading)
    )
import Language.Ltml.AST.DocumentContainer
    ( DocumentContainer (DocumentContainer)
    , DocumentContainerHeader (DocumentContainerHeader)
    )
import Language.Ltml.AST.Footnote (Footnote (Footnote))
import Language.Ltml.AST.Label (Label (..))
import Language.Ltml.AST.Node (Node (..))
import Language.Ltml.AST.Paragraph (Paragraph (..))
import Language.Ltml.AST.Section
    ( Heading (..)
    , Section (..)
    , SectionBody (InnerSectionBody, LeafSectionBody, SimpleLeafSectionBody)
    )
import Language.Ltml.AST.SimpleBlock
    ( SimpleBlock (SimpleParagraphBlock, TableBlock)
    )
import Language.Ltml.AST.SimpleParagraph (SimpleParagraph (SimpleParagraph))
import Language.Ltml.AST.SimpleSection (SimpleSection (SimpleSection))
import Language.Ltml.AST.Table (Table (Table))
import Language.Ltml.AST.Text
    ( EnumItem (..)
    , Enumeration (..)
    , FootnoteReference (..)
    , HardLineBreak (..)
    , SentenceStart (..)
    , TextTree (..)
    )
import Language.Ltml.ToLaTeX.Format
    ( Stylable (..)
    , formatHeading
    , getEnumStyle
    , getIdentifier
    )
import qualified Language.Ltml.ToLaTeX.GlobalState as GS
import Language.Ltml.ToLaTeX.Type
    ( LaTeX (MissingRef, Sequence, Text)
    , center
    , enumerate
    , footnote
    , footref
    , hrule
    , hyperlink
    , hypertarget
    , label
    , linebreak
    , newpage
    , resetfootnote
    , setpdftitle
    )

class ToLaTeXM a where
    toLaTeXM :: a -> State GS.GlobalState LaTeX

class Labelable a where
    attachLabel :: Maybe Label -> a -> State GS.GlobalState LaTeX

-------------------------------- Void -----------------------------------

instance ToLaTeXM Void where
    toLaTeXM = absurd

-------------------------------- Node -----------------------------------

instance (Labelable a) => ToLaTeXM (Node a) where
    toLaTeXM (Node mLabel content) = attachLabel mLabel content

-------------------------------- Text -----------------------------------

instance
    ( Stylable style
    , ToLaTeXM lbrk
    , ToLaTeXM fnref
    , ToLaTeXM enum
    , ToLaTeXM special
    )
    => ToLaTeXM (TextTree lbrk fnref style enum special)
    where
    toLaTeXM (Word t) = pure $ Text $ LT.fromStrict t
    toLaTeXM Space = pure $ Text $ LT.pack " "
    toLaTeXM NonBreakingSpace = pure $ Text $ LT.pack "\xA0"
    toLaTeXM (LineBreak lbrk) = toLaTeXM lbrk
    toLaTeXM (Special s) = toLaTeXM s
    toLaTeXM (Reference l) = pure $ MissingRef l
    toLaTeXM (Styled style tt) = do
        tt' <- mapM toLaTeXM tt
        pure $ applyTextStyle style (Sequence tt')
    toLaTeXM (Enum enum) = toLaTeXM enum
    toLaTeXM (FootnoteRef fnref) = toLaTeXM fnref

instance ToLaTeXM HardLineBreak where
    toLaTeXM HardLineBreak = pure linebreak

instance ToLaTeXM FootnoteReference where
    toLaTeXM (FootnoteReference l@(Label lt)) = do
        labelToRef <- use GS.labelToRef
        case Map.lookup l labelToRef of
            Nothing -> do
                n <- GS.nextFootnote
                GS.insertRefLabel (Just l) (LT.pack $ show n)
                labelToFootNote <- use GS.labelToFootNote
                case Map.lookup l labelToFootNote of
                    Nothing -> pure mempty -- TODO: maybe throw error here?
                    Just (Footnote _ tt) -> do
                        tt' <- mapM toLaTeXM tt
                        pure $
                            footnote $
                                hypertarget l mempty
                                    <> label (LT.fromStrict lt)
                                    <> Sequence tt'
            Just _ -> pure $ footref $ LT.fromStrict lt

instance ToLaTeXM Enumeration where
    toLaTeXM
        ( Enumeration
                (EnumFormat (EnumItemFormat ident (EnumItemKeyFormat key)))
                enumItems
            ) = do
            currentIdent <- use (GS.formatState . GS.enumIdentifierFormat)
            (GS.formatState . GS.enumIdentifierFormat) .= ident
            enumItems' <- mapM toLaTeXM enumItems
            (GS.formatState . GS.enumIdentifierFormat) .= currentIdent
            pure $ enumerate [getEnumStyle ident key] enumItems'

instance ToLaTeXM EnumItem where
    toLaTeXM = attachLabel Nothing

instance Labelable EnumItem where
    attachLabel mLabel (EnumItem tt) = do
        path <- GS.nextEnumPosition
        ident <- use (GS.formatState . GS.enumIdentifierFormat)
        GS.insertRefLabel mLabel (getIdentifier ident (last path))
        tt' <- GS.descendEnumTree $ mapM toLaTeXM tt
        let anchor = maybe mempty (`hypertarget` mempty) mLabel
        pure $ anchor <> Sequence tt'

instance ToLaTeXM SentenceStart where
    toLaTeXM (SentenceStart mLabel) = do
        n <- GS.nextSentence
        GS.insertRefLabel mLabel (LT.pack (show n))
        maybe (pure mempty) toLaTeXM mLabel

-------------------------------- Label -----------------------------------

instance ToLaTeXM Label where
    toLaTeXM l = pure $ hyperlink l mempty

-------------------------------- Paragraph -----------------------------------

instance ToLaTeXM SimpleParagraph where
    toLaTeXM (SimpleParagraph (SimpleParagraphFormat t) content) = do
        content' <- mapM toLaTeXM content
        pure $
            applyTextStyle t $
                Sequence content'

instance ToLaTeXM Paragraph where
    toLaTeXM = attachLabel Nothing

instance Labelable Paragraph where
    attachLabel mLabel (Paragraph (ParagraphFormat ident (ParagraphKeyFormat key)) content) = do
        n <- GS.nextParagraph
        let identifier = getIdentifier ident n
        GS.insertRefLabel mLabel identifier
        GS.enumPosition .= [0]
        content' <- mapM toLaTeXM content
        let anchor = maybe mempty (`hypertarget` mempty) mLabel
        b <- use (GS.flagState . GS.onlyOneParagraph)
        pure $
            anchor
                <> if b
                    then Sequence content'
                    else
                        enumerate
                            [ LT.pack $ "start=" <> show n
                            , getEnumStyle ident key
                            ]
                            [Sequence content']

--------------------------------- Table ------------------------------------

instance ToLaTeXM Table where
    toLaTeXM Table = undefined -- TODO

-------------------------------- Section -----------------------------------

instance ToLaTeXM SimpleSection where
    toLaTeXM (SimpleSection (SimpleSectionFormat hasHLine) content) = do
        content' <- mapM toLaTeXM content
        pure $ (if hasHLine then hrule else mempty) <> Sequence content'

createHeading :: HeadingFormat b -> LaTeX -> LaTeX -> State GS.GlobalState LaTeX
createHeading (HeadingFormat t hfmt) tt ident = do
    pure $
        applyTextStyle t $
            formatHeading hfmt ident tt

instance ToLaTeXM Section where
    toLaTeXM = attachLabel Nothing

instance Labelable Section where
    attachLabel
        mLabel
        (Section (SectionFormat ident (TocKeyFormat keyident)) (Heading fmt tt) nodes) =
            do
                tt' <- mapM toLaTeXM tt
                let headingText = Sequence tt'
                    buildHeading n = do
                        createHeading fmt headingText (Text $ getIdentifier ident n)
                    setLabel n = GS.insertRefLabel mLabel (LT.pack (show n))
                case nodes of
                    LeafSectionBody paragraphs -> do
                        n <- GS.nextSection
                        setLabel n
                        GS.flagState . GS.onlyOneParagraph .= (length paragraphs == 1)
                        GS.addTOCEntry n keyident ident headingText
                        headingDoc <- buildHeading n
                        content' <- mapM toLaTeXM paragraphs
                        let anchor = maybe (center [headingDoc]) (`hypertarget` center [headingDoc]) mLabel
                        pure $ anchor <> Sequence content'
                    InnerSectionBody subsections -> do
                        n <- GS.nextSupersection
                        setLabel n
                        modify $
                            {-  -} (GS.flagState . GS.isSupersection .~ True)
                                . (GS.counterState . GS.supersectionCTR .~ 0)
                        GS.addTOCEntry n keyident ident headingText
                        headingDoc <- buildHeading n
                        content' <- mapM toLaTeXM subsections
                        modify $
                            {-  -} (GS.flagState . GS.isSupersection .~ False)
                                . (GS.counterState . GS.supersectionCTR .~ n)
                        let anchor =
                                maybe (headingDoc <> linebreak) (`hypertarget` (headingDoc <> linebreak)) mLabel
                        pure $ anchor <> Sequence content'
                    SimpleLeafSectionBody simpleblocks -> do
                        content' <- mapM toLaTeXM simpleblocks
                        pure $ Sequence content'

-------------------------------- Block ----------------------------------

instance ToLaTeXM SimpleBlock where
    toLaTeXM (SimpleParagraphBlock b) = toLaTeXM b
    toLaTeXM (TableBlock b) = toLaTeXM b

-------------------------------- Document -----------------------------------

instance ToLaTeXM Document where
    toLaTeXM = attachLabel Nothing

instance Labelable Document where
    attachLabel
        mLabel
        ( Document
                (DocumentFormat hasTOC)
                (DocumentHeading tt)
                (DocumentBody intro content outro)
                footnotemap
            ) = do
            {- build the heading text from the given HeadingFormat
               passed by the state and depending on the position we are in -}
            tt' <- mapM toLaTeXM tt
            headingText <- buildHeading tt'

            {- prepare the state for this document -}
            GS.labelToFootNote .= footnotemap
            GS.resetCountersSoft
            GS.toc .= mempty

            {- recursively receive the needed parts of the document -}
            intro' <- mapM toLaTeXM intro
            content' <- case content of
                LeafSectionBody paragraphs -> do
                    mapM toLaTeXM paragraphs
                SimpleLeafSectionBody simpleblocks -> do
                    mapM toLaTeXM simpleblocks
                InnerSectionBody sections -> do
                    mapM toLaTeXM sections
            outro' <- mapM toLaTeXM outro

            {- if we need a toc then we assemble it. -}
            toc' <- buildTOC

            {- assemble the final document -}
            pure $
                headingText
                    <> Sequence toc'
                    <> Sequence intro'
                    <> Sequence content'
                    <> Sequence outro'
          where
            buildHeading :: [LaTeX] -> State GS.GlobalState LaTeX
            buildHeading tt' = do
                b <- use (GS.flagState . GS.docType)
                case b of
                    GS.Appendix -> do
                        n <- GS.nextAppendix
                        AppendixElementFormat ident (TocKeyFormat key) fmt <-
                            use (GS.formatState . GS.appendixFormat)
                        let iText = getIdentifier ident n
                        GS.insertRefLabel mLabel iText
                        GS.addTOCEntry n key ident (Sequence tt')
                        GS.addAppendixHeaderEntry n key ident (Sequence tt')
                        createHeading fmt (Sequence tt') (Text iText)
                    GS.Main -> do
                        fmt <- use (GS.formatState . GS.docHeadingFormat)
                        createHeading fmt (Sequence tt') (Text " ")

            buildTOC :: State GS.GlobalState [LaTeX]
            buildTOC
                | not hasTOC = pure mempty
                | otherwise = do
                    t <- use (GS.flagState . GS.docType)
                    case t of
                        GS.Main -> do
                            toc' <- use GS.toc
                            appendixHeaders' <- use GS.appendixHeaders
                            pure $ DList.toList (toc' <> appendixHeaders')
                        GS.Appendix -> do
                            toc' <- use GS.toc
                            pure $ DList.toList toc'

-------------------------------- AppendixSection -----------------------------------

instance ToLaTeXM AppendixSection where
    toLaTeXM
        ( AppendixSection
                ( AppendixSectionFormat
                        (AppendixSectionTitle t)
                        elementFmt
                    )
                nodes
            ) = do
            GS.counterState . GS.appendixCTR .= 0
            GS.flagState . GS.docType .= GS.Appendix
            GS.formatState . GS.appendixFormat .= elementFmt
            GS.appendixHeaders %= (<> DList.fromList [Text (LT.fromStrict t), linebreak])
            nodes' <- mapM toLaTeXM nodes
            pure $ Sequence $ map ((newpage <> resetfootnote) <>) nodes'

instance ToLaTeXM DocumentContainer where
    toLaTeXM
        ( DocumentContainer
                (DocumentContainerFormat headerFmt footerFmt headingFmt)
                (DocumentContainerHeader pdfTitle superTitle title date)
                doc
                appendices
            ) = do
            {- prepare the state -}
            GS.preDocument %= (<> setpdftitle (LT.fromStrict pdfTitle))
            GS.addHeaderFooter headerFmt footerFmt superTitle title date
            GS.formatState . GS.docHeadingFormat .= headingFmt
            GS.resetCountersHard

            appendices' <- mapM toLaTeXM appendices

            GS.flagState . GS.docType .= GS.Main
            doc' <- toLaTeXM doc

            {- assemble the final document container -}
            pure $ doc' <> Sequence appendices'
