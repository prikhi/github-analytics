{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE UndecidableInstances #-}
module Main where

import Control.Arrow ((&&&))
import Control.Lens ((%~))
import Data.Aeson (encode)
import Data.Function (on)
import Data.List (sortBy)
import Data.Monoid ((<>))
import Data.Text.Encoding (decodeUtf8)
import Reflex.Dom

import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Language.Javascript.JSaddle.Warp as W
import qualified Reflex.Dom.Core as RC

import CSS
import Common

main :: IO ()
main = mainWidgetWithCss renderedCss bodyWidget

mainServer :: IO ()
mainServer =
    W.run 3911 $ RC.mainWidgetWithCss renderedCss bodyWidget


data Page
    = Login
    | PageData
    | PageError
    deriving (Eq)

bodyWidget :: MonadWidget t m => m ()
bodyWidget = elAttr "div" ("id" =: "body-wrapper") $ do
    rec evApi@(evApiOk, evApiErr) <- request authSubmitEv
        dynPage <- dynamicPage evApi
        authSubmitEv <- authForm dynPage
        errorMessage dynPage evApiErr
        dataView dynPage evApiOk
    return ()
    where
        dynamicPage (evApiOk, evApiErr) =
            foldDyn ($) Login $ leftmost
                [ const PageData <$ evApiOk
                , const PageError <$ evApiErr
                ]
        errorMessage dynPage evError = do
            let dynAttr = visible (== PageError) <$> dynPage
            elDynAttr "div" dynAttr $ text "Error"

visible :: (a -> Bool) -> a -> M.Map T.Text T.Text
visible p v = "class" =: choose (p v) "" "hidden"
    where
        choose b t f =
            if b then t else f



authForm :: MonadWidget t m => Dynamic t Page -> m (Event t AuthData)
authForm dynPage = do
    let dynAttr = visible (`elem` [Login, PageError]) <$> dynPage
    fmap snd . elDynAttr' "div" dynAttr $ do
        userEv <- textInput $ def
            & attributes .~ constDyn ("placeholder" =: "Username")
        passEv <- textInput $ def
            & textInputConfig_inputType .~ "password"
            & attributes .~ constDyn ("placeholder" =: "Password")
        clickEv <- button "Fetch Data"
        let enterEv = leftmost $ map (keypress Enter) [userEv, passEv]
        return
            $ tagPromptlyDyn (AuthData <$> value userEv <*> value passEv)
            $ leftmost [clickEv, enterEv]


dataView :: MonadWidget t m => Dynamic t Page -> Event t [RepositoryTraffic] -> m ()
dataView dynPage evRepos = do
    let dynAttr = visible (== PageData) <$> dynPage
    elDynAttr "div" dynAttr $
        el "table" $ do
            el "thead" $
                el "tr" $ do
                    el "th" $ text "Name"
                    el "th" $ text "Views"
            dynRepos <- holdDyn [] evRepos
            el "tbody" $ simpleList dynRepos renderRepo
    return ()
    where
        renderRepo :: MonadWidget t m => Dynamic t RepositoryTraffic -> m ()
        renderRepo dynRepo = el "tr" $ do
            el "td" $ dynText $ rtName <$> dynRepo
            el "td" $ display $ rtViews <$> dynRepo
            return ()


-- Get users repos w/ statistics
request :: MonadWidget t m => Event t AuthData -> m (Event t [RepositoryTraffic], Event t XhrResponse)
request evAuthData =
    fmap checkXhrRsp . performRequestAsync $ makeRequest <$> evAuthData
    where
        makeRequest aData =
            let
                url =
                    "http://localhost:9021"
                config =
                    def & xhrRequestConfig_sendData
                            .~ decodeUtf8 (LB.toStrict $ encode aData)
                        & xhrRequestConfig_headers
                            %~ (<> "Content-Type" =: "application/json;charset=utf-8")
            in
                XhrRequest "POST" url config

checkXhrRsp :: Reflex t => Event t XhrResponse -> (Event t [RepositoryTraffic], Event t XhrResponse)
checkXhrRsp = evOk &&& evErr
    where
        evOk = fmap (sortBy (flip compare `on` rtViews)) . fmapMaybe decodeXhrResponse . ffilter (\rsp -> _xhrResponse_status rsp == 200)
        evErr = ffilter (\rsp -> _xhrResponse_status rsp /= 200)
