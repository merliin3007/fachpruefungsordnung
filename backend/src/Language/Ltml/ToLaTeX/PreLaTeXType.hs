{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.ToLaTeX.PreLaTeXType
    ( PreLaTeX (..)
    {- styling -}
    , text
    , bold
    , italic
    , underline
    , large
    , small
    {- references -}
    , footnote
    , hypertarget
    , hyperlink
    , label
    , footref
    {- commands to structure the text -}
    , medskip
    , hrule
    , linebreak
    , newpage
    {- setup and metadata -}
    , setpdftitle
    , usepackage
    , documentclass
    , fancyhead
    , fancyfoot
    , resetfootnote
    {- environments -}
    , enumerate
    , itemize
    , center
    , flushleft
    , flushright
    , minipage
    , document
    {- other -}
    , setindent
    , setfontArabic
    , enumStyle
    ) where

import qualified Data.Text.Lazy as LT
import Language.Ltml.AST.Label (Label (Label))

data PreLaTeX
    = IText LT.Text
    | IRaw LT.Text -- raw unescaped PreLaTeX
    | ICommandS LT.Text -- \command
    | ICommand LT.Text [LT.Text] [PreLaTeX] -- \command[opts]{args}
    | IEnvironment LT.Text [LT.Text] [PreLaTeX] -- \begin{env}[opts] ... \end{env}
    | IBraced PreLaTeX -- used for wrapping in braces
    | ISequence [PreLaTeX] -- concatenation
    {- the reason why we introduced this intermediate data type: -}
    | MissingRef Label
    deriving (Show)

-- | We want to be able to connect PreLaTeX structures and avoid deeply rooted sequences.
--   Here we are using a monoid to be able to concat PreLaTeX structures while flattening sequences.
instance Semigroup PreLaTeX where
    a <> b = sequence' [a, b]
      where
        sequence' :: [PreLaTeX] -> PreLaTeX
        sequence' xs = case flatten xs of
            [x] -> x
            ys -> ISequence ys

        -- Flatten nested Sequences as we build them
        flatten :: [PreLaTeX] -> [PreLaTeX]
        flatten = concatMap go
          where
            go (ISequence ys) = flatten ys
            go x = [x]

instance Monoid PreLaTeX where
    mempty = ISequence []

-------------------------------------------------------------------------------
{-                                styling                                   -}

text :: LT.Text -> PreLaTeX
text = IText

bold :: PreLaTeX -> PreLaTeX
bold = ICommand "textbf" [] . (: [])

italic :: PreLaTeX -> PreLaTeX
italic = ICommand "emph" [] . (: [])

underline :: PreLaTeX -> PreLaTeX
underline = ICommand "underline" [] . (: [])

large :: PreLaTeX -> PreLaTeX
large content = IBraced $ ICommand "large" [] [content]

small :: PreLaTeX -> PreLaTeX
small content = IBraced $ ICommand "small" [] [content]

-------------------------------------------------------------------------------
{-                             referencing                                -}

footnote :: PreLaTeX -> PreLaTeX
footnote = ICommand "footnote" [] . (: [])

hypertarget :: Label -> PreLaTeX -> PreLaTeX
hypertarget (Label l) latex = ICommand "hypertarget" [] [IText (LT.fromStrict l), latex]

hyperlink :: Label -> PreLaTeX -> PreLaTeX
hyperlink (Label l) latex = ICommand "hyperlink" [] [IText (LT.fromStrict l), latex]

label :: LT.Text -> PreLaTeX
label l = ICommand "label" [] [IText l]

footref :: LT.Text -> PreLaTeX
footref r = ICommand "footref" [] [IText r]

-------------------------------------------------------------------------------
{-                             setup and metadata                             -}

usepackage :: [LT.Text] -> LT.Text -> PreLaTeX
usepackage opts package = ICommand "usepackage" opts [IText package]

documentclass :: [LT.Text] -> LT.Text -> PreLaTeX
documentclass opts name = ICommand "documentclass" opts [IText name]

fancyhead :: [LT.Text] -> PreLaTeX -> PreLaTeX
fancyhead opts content = ICommand "fancyhead" opts [content]

fancyfoot :: [LT.Text] -> PreLaTeX -> PreLaTeX
fancyfoot opts content = ICommand "fancyfoot" opts [content]

resetfootnote :: PreLaTeX
resetfootnote = ICommand "setcounter" [] [IText "footnote", IText "0"]

setpdftitle :: LT.Text -> PreLaTeX
setpdftitle title = ICommand "hypersetup" [] [IText $ "pdftitle={" <> title <> "}"]

-------------------------------------------------------------------------------
{-                              text structure                              -}

linebreak :: PreLaTeX
linebreak = IRaw "\\\\"

newpage :: PreLaTeX
newpage = ICommandS "newpage"

medskip :: PreLaTeX
medskip = IText "\n" <> ICommandS "medskip" <> IRaw "\n"

hrule :: PreLaTeX
hrule = ICommandS "hrule"

-------------------------------------------------------------------------------
{-                              environments                                 -}

enumerate :: [LT.Text] -> [PreLaTeX] -> PreLaTeX
enumerate opts items = IEnvironment "enumerate" opts (map (\i -> ICommand "item" [] [i]) items)

itemize :: [PreLaTeX] -> PreLaTeX
itemize items = IEnvironment "itemize" [] (map (\i -> ICommand "item" [] [i]) items)

center :: [PreLaTeX] -> PreLaTeX
center = IEnvironment "center" []

flushleft :: [PreLaTeX] -> PreLaTeX
flushleft = IEnvironment "flushleft" []

flushright :: [PreLaTeX] -> PreLaTeX
flushright = IEnvironment "flushright" []

minipage :: [LT.Text] -> [PreLaTeX] -> PreLaTeX
minipage = IEnvironment "minipage"

document :: PreLaTeX -> PreLaTeX
document content = IEnvironment "document" [] [content]

-------------------------------------------------------------------------------
{-                              other                                        -}

setindent :: PreLaTeX
setindent = IRaw "\\setlength{\\parindent}{0pt}"

setfontArabic :: PreLaTeX
setfontArabic =
    ISequence
        [ usepackage [] "helvet"
        , IRaw "\\renewcommand{\\familydefault}{\\sfdefault}"
        ]

enumStyle :: PreLaTeX
enumStyle =
    IRaw "\\setlist[enumerate,1]{label=\\arabic*., left=0pt}"
        <> IRaw "\\setlist[enumerate,2]{label=\\alph*., left=0.5em}"
        <> IRaw "\\setlist[enumerate,3]{label=\\alph*\\alph*., left=1em}"
        <> IRaw "\\setlist[enumerate,4]{label=-, left=1.5em}"
        <> IRaw "\\setlist{nosep}"
