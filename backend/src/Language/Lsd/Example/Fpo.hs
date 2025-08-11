{-# LANGUAGE OverloadedStrings #-}

module Language.Lsd.Example.Fpo
    ( fpoT
    , superSectionT
    , sectionT
    , paragraphT
    )
where

import Data.Void (Void)
import Language.Lsd.AST.Common
import Language.Lsd.AST.Format
import Language.Lsd.AST.SimpleRegex
import Language.Lsd.AST.Type.AppendixSection
import Language.Lsd.AST.Type.Document
import Language.Lsd.AST.Type.DocumentContainer
import Language.Lsd.AST.Type.Enum
import Language.Lsd.AST.Type.Paragraph
import Language.Lsd.AST.Type.Section
import Language.Lsd.AST.Type.Text

fpoT :: DocumentContainerType
fpoT =
    DocumentContainerType
        DocumentContainerFormat
        mainDocT
        [appendixT, attachmentT]

appendixT :: AppendixSectionType
appendixT =
    AppendixSectionType
        ( AppendixSectionFormat
            (AppendixSectionTitle "Anlagen")
            ( AppendixElementFormat
                (FormatString [PlaceholderAtom Arabic])
                ( TocKeyFormat $
                    FormatString
                        [ StringAtom "Anlage "
                        , PlaceholderAtom KeyIdentifierPlaceholder
                        ]
                )
                ( FormatString
                    [ StringAtom "Anlage "
                    , PlaceholderAtom IdentifierPlaceholder
                    , StringAtom "\n"
                    , PlaceholderAtom HeadingTextPlaceholder
                    ]
                )
            )
        )
        [] -- TODO

attachmentT :: AppendixSectionType
attachmentT =
    AppendixSectionType
        ( AppendixSectionFormat
            (AppendixSectionTitle "Anhänge")
            ( AppendixElementFormat
                (FormatString [PlaceholderAtom Arabic])
                ( TocKeyFormat $
                    FormatString
                        [ StringAtom "Anhang "
                        , PlaceholderAtom KeyIdentifierPlaceholder
                        ]
                )
                ( FormatString
                    [ StringAtom "Anhang "
                    , PlaceholderAtom IdentifierPlaceholder
                    , StringAtom "\n"
                    , StringAtom "(nicht Bestandteil der Satzung)"
                    , StringAtom "\n"
                    , PlaceholderAtom HeadingTextPlaceholder
                    ]
                )
            )
        )
        [] -- TODO

mainDocT :: DocumentType
mainDocT =
    DocumentType
        DocumentFormat
        ( SimpleRegex
            (Sequence [])
            ( Disjunction
                [ Star $ Disjunction [sectionT]
                , Star $ Disjunction [superSectionT]
                ]
            )
            (Sequence [])
        )

superSectionT :: SectionType
superSectionT =
    SectionType
        (Keyword "=")
        ( HeadingType
            ( FormatString
                [ StringAtom "Abschnitt "
                , PlaceholderAtom IdentifierPlaceholder
                , StringAtom " "
                , PlaceholderAtom HeadingTextPlaceholder
                ]
            )
            plainTextT
        )
        ( SectionFormat
            (FormatString [PlaceholderAtom Arabic])
            ( TocKeyFormat $
                FormatString
                    [ StringAtom "Abschnitt "
                    , PlaceholderAtom KeyIdentifierPlaceholder
                    ]
            )
        )
        ( Right $
            SimpleRegex
                (Sequence [])
                ( Disjunction
                    [ Star $ Disjunction [sectionT]
                    ]
                )
                (Sequence [])
        )

sectionT :: SectionType
sectionT =
    SectionType
        (Keyword "§")
        ( HeadingType
            ( FormatString
                [ StringAtom "§ "
                , PlaceholderAtom IdentifierPlaceholder
                , StringAtom "\n"
                , PlaceholderAtom HeadingTextPlaceholder
                ]
            )
            plainTextT
        )
        ( SectionFormat
            (FormatString [PlaceholderAtom Arabic])
            ( TocKeyFormat $
                FormatString
                    [ StringAtom "§ "
                    , PlaceholderAtom KeyIdentifierPlaceholder
                    ]
            )
        )
        (Left paragraphT)

paragraphT :: ParagraphType
paragraphT =
    ParagraphType
        ( ParagraphFormat $
            FormatString [StringAtom "(", PlaceholderAtom Arabic, StringAtom ")"]
        )
        richTextT

plainTextT :: TextType Void
plainTextT = TextType [] [footnoteT]

richTextT :: TextType EnumType
richTextT = richTextTF [enumT]

richTextTF :: [EnumType] -> TextType EnumType
richTextTF enumTs = TextType enumTs [footnoteT]

footnoteTextT :: TextType Void
footnoteTextT = plainTextT

enumT :: EnumType
enumT =
    EnumType
        (Keyword "#")
        ( EnumFormat $
            EnumItemFormat
                (FormatString [PlaceholderAtom Arabic])
                ( EnumItemKeyFormat $
                    FormatString
                        [ PlaceholderAtom KeyIdentifierPlaceholder
                        , StringAtom "."
                        ]
                )
        )
        (richTextTF [enumTF 1 3])
  where
    enumTF :: Int -> Int -> EnumType
    enumTF depth maxdepth =
        EnumType
            (Keyword "#")
            ( EnumFormat $
                EnumItemFormat
                    ( FormatString $
                        replicate depth (PlaceholderAtom AlphabeticLower)
                    )
                    ( EnumItemKeyFormat $
                        FormatString
                            [ StringAtom "("
                            , PlaceholderAtom KeyIdentifierPlaceholder
                            , StringAtom ")"
                            ]
                    )
            )
            (richTextTF nextEnumTs)
      where
        nextEnumTs = case compare depth maxdepth of
            LT -> [enumTF (depth + 1) maxdepth]
            EQ -> []
            GT -> error "enumTF: depth > maxdepth"

footnoteT :: FootnoteType
footnoteT = FootnoteType (Keyword "^") footnoteTextT
