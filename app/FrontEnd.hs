{-# Language DataKinds #-}
{-# Language FlexibleContexts #-}
{-# Language ImportQualifiedPost #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language TemplateHaskell #-}

module Main
  ( main
  ) where

import Brick hiding (Direction, Max)



import qualified Graphics.Vty as V

import Lens.Micro (Traversal, (^.))
import qualified Brick.Main as M
import Brick.Types ( Widget, BrickEvent(..))
import Brick.Widgets.Core ( padAll, str)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L
import qualified Brick.AttrMap as A
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Util (on, bg)
import qualified Brick.Types as T

import Data.Foldable (fold)
import Data.Functor (($>))
import Data.Vector qualified as V
import Graphics.Vty.Image
import Graphics.Vty.Attributes (bold, withStyle)

import Lens.Micro.TH


type CoverpageOptions = L.List Pannel CoverpageChoices

data CoverpageChoices
    = ChooseEntryViewer
    | ChooseEntryCreate
    | ChoosePreferences
            deriving (Bounded, Enum, Show)


data Pannel
    = JournalCoverpage
    | JournalPreferences
    | JournalPageViewer Word
    | JournalEntryViewer
    deriving (Show, Eq, Ord)


--    | EntryCreator
--    | EntryViewer
--    | EntryEditor


data AppState
    = Coverpage CoverpageOptions
    | Settings ()
    | Browser ()
    | Editor ()


_Coverpage :: Traversal AppState AppState CoverpageOptions CoverpageOptions
_Coverpage f (Coverpage xs) = Coverpage <$> f xs
_Coverpage _ (Settings b) = pure (Settings b)
_Coverpage _ (Browser b) = pure (Browser b)
_Coverpage _ (Editor b) = pure (Editor b)


_Editor :: Traversal AppState AppState () ()
_Editor _ (Coverpage xs) = pure (Coverpage xs)
_Editor _ (Settings b) = pure (Settings b)
_Editor _ (Browser b) = pure (Browser b)
_Editor f (Editor b) = Editor <$> f b


initialState :: AppState
initialState = Coverpage coverPageOptions


main :: IO ()
main = do
    M.defaultMain theApp initialState
    putStrLn $ "You chose: "


theApp :: M.App AppState e Pannel
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return ()
          , M.appAttrMap = const theMap
          }


drawUI :: AppState -> [Widget Pannel]
drawUI = \case
    Coverpage opts -> drawCoverpage opts
    Editor () -> [str "E D I T O R \x2303 + c\x20E3"]
    _ -> []


appEvent :: T.BrickEvent Pannel e -> T.EventM Pannel AppState ()
appEvent = \case
    -- Immediately handle terminal control events
    VtyEvent (V.EvResize  {}) -> pure ()
    VtyEvent (V.EvKey V.KEsc []) -> M.halt

    -- Next check which mode we are in
    VtyEvent vtyEvent -> get >>= \case
        Coverpage opts -> case vtyEvent of
            V.EvKey V.KEnter [] -> case L.listSelectedElement opts of
                Nothing -> pure ()
                Just (_, ChooseEntryViewer) -> put $ Browser ()
                Just (_, ChooseEntryCreate) -> put $ Editor ()
                Just (_, ChoosePreferences) -> put $ Settings ()
            _ -> zoom _Coverpage $ L.handleListEvent vtyEvent

        _ -> pure ()


theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (D.dialogAttr, V.white `on` V.blue)
    , (D.buttonAttr, V.black `on` V.white)
    , (D.buttonSelectedAttr, bg V.yellow)
    , (coverpageSelection, fg V.yellow `withStyle` bold)
    ]


coverpageSelection = A.attrName "coverpageSelection"


{-
-- -- -- -- -- --
-}


coverPageOptions = flip (L.list JournalCoverpage) 3 $ V.fromList
    [ minBound :: CoverpageChoices .. maxBound :: CoverpageChoices ]



drawCoverpage :: CoverpageOptions -> [Widget Pannel]
drawCoverpage opts = pure . C.vCenter . vBox $ C.hCenter <$>
            [ padBottom (Pad 2) imageBanner
            , drawCoverPageOptions opts
            ]


drawCoverPageOptions :: CoverpageOptions -> Widget Pannel
drawCoverPageOptions opts =
    let
        drawCoverPageOption :: Bool -> CoverpageChoices -> Widget Pannel
        drawCoverPageOption =
            let
                renderStyle f isSelected =
                    let styling
                            | isSelected = labelBoarder -- border -- . withAttr coverPageSelected
                            | otherwise = (<+> str " ")
                          
                    in   styling . f isSelected
        
                renderGlyph = \case
                    ChooseEntryViewer -> "🔎"
                    ChooseEntryCreate -> "➕"
                    ChoosePreferences -> "⚙️ "
                
                renderLabel isSelected = labelFormatter isSelected . labelText

                labelText = \case
                    ChooseEntryViewer -> "Browse"
                    ChooseEntryCreate -> "Record"
                    ChoosePreferences -> "Settings"

                labelFormatter :: Bool -> String -> Widget Pannel
                labelFormatter isSelected =
                    let offset
                            | isSelected = 1
                            | otherwise  = 0
                        (padH, padV) = (labelPaddingHor - offset, labelPaddingVer - offset)
                    in  padLeftRight padH .
                        padTopBottom padV .
                        fixedWidthStr (toEnum labelFixedWidth)

                labelBoarder val = withBorderStyle unicodeBold .
                    updateAttrMap (A.applyAttrMappings selectedOptionAttrs) $ vBox
                    [ joinableBorder (Edges False False False False) <+> hBorder
                    , vLimit labelPaddingVer . (vBorder <+>) . withAttr coverpageSelection $ val <+> str "▶"
                    , joinableBorder (Edges False False False False) <+> hBorder
                    ]

            in  renderStyle renderLabel

        labelFixedWidth = 8
        labelPaddingHor = 2
        labelPaddingVer = 1

        optListWidth = labelFixedWidth + 2 * labelPaddingHor -- - 1 -- Minus one for the missing right border
        optListHeight = (1 + 2 * labelPaddingVer) * optListLength
        optListLength = V.length $ L.listElements opts

        helpBoxHeight = optListHeight - 2 -- Minus 2 for the horizontal boarder lines
        helpBoxWidth = 2 * optListWidth


        label = str "Item " <+> cur <+> str " of " <+> total
        cur = case opts^.(L.listSelectedL) of
                Nothing -> str "-"
                Just i -> str (show (i + 1))

        getSelectedOptText = case L.listSelectedElement opts of
            Nothing -> ""
            Just (_,v) -> case v of
                    ChooseEntryViewer -> "View and/or edit the existing dive log enrties in the journal"
                    ChooseEntryCreate -> "Create a new dive log entry in the journal"
                    ChoosePreferences -> "View and/or edit the journal preferences"
        
        total = str $ show $ V.length $ opts ^. L.listElementsL

        boxOptList =
              hLimit optListWidth $
              vLimit optListHeight $
              L.renderList drawCoverPageOption True opts
              
        boxDescribe = updateAttrMap (A.applyAttrMappings selectedOptionAttrs) $
              withBorderStyle unicodeBold . border $ --withBorderStyle unicodeRounded $ B.border $
              hLimit helpBoxWidth $
              vLimit helpBoxHeight $
              (<=> fill ' ') $ padLeftRight 1 $ strWrap getSelectedOptText
        
    in  vBox [ C.hCenter $ joinBorders $ (boxOptList <+> boxDescribe)
                              , str " "
                              , C.hCenter $ vBox
                                 [ str "Select: \x23CE (Enter)"
                                 , str "Exit:   \x238B (Esc)"  -- \x2303 + c\x20E3"
                                 ]
                              ]


selectedOptionAttrs :: [(A.AttrName, V.Attr)]
selectedOptionAttrs =
    [ (B.borderAttr,  fg V.yellow `withStyle` bold)
    , (B.vBorderAttr, fg V.yellow `withStyle` bold)
    , (B.hBorderAttr, fg V.yellow `withStyle` bold)
    ]


coverPageRenderOptions :: Widget Pannel
coverPageRenderOptions =
    let renderer isSelected = \case
            ChooseEntryViewer -> labelFormatter "View Entries"
            ChooseEntryCreate -> labelFormatter "New Entry"
            ChoosePreferences -> labelFormatter "Preferences"
        labelFormatter = padLeftRight 1 . fixedWidthStr 12
    in  L.renderList renderer True coverPageOptions


fixedWidthStr :: Word -> String -> Widget n
fixedWidthStr width val =
    let len = length val
    in  str $ val <> replicate (fromEnum width - len) ' '


imageBanner :: Widget Pannel
imageBanner = raw $ imageDiver <|> imageTitle


imageDiver :: Image
imageDiver = fold
    [ iso10646String V.defAttr "      _ @"
    , iso10646String V.defAttr "     (_) 0"
    , iso10646String V.defAttr "        o  _"
    , iso10646String V.defAttr "       °   \\\\"
    , iso10646String V.defAttr "   )))))))) \\\\"
    , iso10646String V.defAttr " //.-----.  //"
    , iso10646String V.defAttr "//( o   o )//"
    , iso10646String V.defAttr " C `--^--'//"
    , iso10646String V.defAttr "  \\  ( ~~`/"
    , iso10646String V.defAttr "   `--`\"\"`"
    ]

imageTitle :: Image
imageTitle = fold
    [ iso10646String V.defAttr "  ____   ____ _   _ ____    _"
    , iso10646String V.defAttr " / ___| / ___| | | | __ )  / \\"
    , iso10646String V.defAttr " \\___ \\| |   | | | |  _ \\ / _ \\"
    , iso10646String V.defAttr "  ___) | |___| |_| | |_) / ___ \\"
    , iso10646String V.defAttr " |____/ \\____|\\___/|____/_/   \\_\\"
    , iso10646String V.defAttr "  ____  _                 _                              _"
    , iso10646String V.defAttr " |  _ \\(_)_   _____      | | ___  _   _ _ __ _ __   __ _| |"
    , iso10646String V.defAttr " | | | | \\ \\ / / _ \\  _  | |/ _ \\| | | | '__| '_ \\ / _` | |"
    , iso10646String V.defAttr " | |_| | |\\ V /  __/ | |_| | (_) | |_| | |  | | | | (_| | |"
    , iso10646String V.defAttr " |____/|_| \\_/ \\___|  \\___/ \\___/ \\__,_|_|  |_| |_|\\__,_|_|"
    ]
