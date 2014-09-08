{-# LANGUAGE OverloadedStrings #-}
import Graphics.Vty.Widgets.All
import qualified Data.Text as T
import Network.HTTP
import Text.Regex
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Debug.Trace
import Control.Monad

data Video = Video { url :: String
                   , title :: String
                   , duration :: String
                   , uploaded :: String
                   , uploader :: String
                   , description :: String
                   , plays :: String
                   , likes :: String
                   , numcomments :: String
                   , thumbnail :: String
                   } deriving (Show)

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP ((getRequest x) {rqHeaders = [mkHeader HdrUserAgent "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/36.0.1985.125 Safari/537.36"]})

resultsscreen = do
        label <- plainText ""

        rg <- newRadioGroup
        cb1 <- newCheckbox "Relevant"
        cb2 <- newCheckbox "Date"
        cb3 <- newCheckbox "Alphabetical"
        cb4 <- newCheckbox "Plays"
        cb5 <- newCheckbox "Likes"
        cb6 <- newCheckbox "Comments"
        cb7 <- newCheckbox "Duration"
        
        radios <- hBox cb1 =<< hBox cb2 =<< hBox cb3 =<< hBox cb4 =<< hBox cb5 =<< hBox cb6 cb7

        lst <- newList (getNormalAttr defaultContext) 1
        blst <- bordered lst
        ui <- centered =<< vBox label =<< vBox radios blst
        fg <- newFocusGroup

        addToRadioGroup rg cb1
        addToRadioGroup rg cb2
        addToRadioGroup rg cb3
        addToRadioGroup rg cb4
        addToRadioGroup rg cb5
        addToRadioGroup rg cb6
        addToRadioGroup rg cb7

        setCheckboxChecked cb1

        addToFocusGroup fg lst
        addToFocusGroup fg cb1
        addToFocusGroup fg cb2
        addToFocusGroup fg cb3
        addToFocusGroup fg cb4
        addToFocusGroup fg cb5
        addToFocusGroup fg cb6
        addToFocusGroup fg cb7

        return (ui, fg, lst, label)

searchscreen :: IO (Widget (VCentered (HCentered (HFixed (Box FormattedText Edit)))), Widget FocusGroup, Widget Edit)
searchscreen = do
        e <- editWidget
        b <- (plainText "Enter a search: ") <++> (return e) >>= hFixed 40
        ui <- centered b
        fg <- newFocusGroup
        addToFocusGroup fg b
        return (ui, fg, e)

loadingscreen :: IO (Widget (VCentered (HCentered FormattedText)), Widget FocusGroup)
loadingscreen = do
        b <- (plainText "Loading...")
        ui <- centered b
        fg <- newFocusGroup
        addToFocusGroup fg b
        return (ui, fg)

attrs :: (TagTree String) -> Maybe [Attribute String]
attrs (TagBranch _ ats _) = Just ats
attrs (TagLeaf _) = Nothing

ch :: (TagTree String) -> [TagTree String]
ch (TagBranch _ _ cs) = cs

chForN :: [(TagTree String)] -> Int -> [TagTree String]
chForN tb n = ch $ tb !! n 

getTitle :: [TagTree String] -> String
getTitle [] = ""
getTitle ((TagLeaf (TagText txt)):tags) = txt ++ (getTitle tags)
getTitle ((TagBranch _ _ [(TagLeaf (TagText txt))]):tags) = txt ++ (getTitle tags)

dosearch :: String -> IO [Video]
dosearch url = do
        resultSrc <- openURL url
        let uniTree = universeTree $ tagTree $ parseTags resultSrc
        let tree = (filter (\x -> ((flattenTree [x]) !! 0) ~== ("<ol>" :: String)) uniTree) !! 0
        let results = case tree of
                          TagLeaf _ -> []
                          TagBranch _ _ liLst -> map(\x -> case x of
                                    (TagBranch _ _ li) -> do
                                                         vidpath <- lookup "href" =<< (attrs $ li !! 1)
                                                         title <- return $ getTitle (chForN (chForN (chForN li 3) 1) 1)
                                                         duration <- return $ getTitle (chForN (chForN li 3) 3)
                                                         uploaded <- lookup "title" =<< (attrs $ (chForN (chForN li 3) 5) !! 3)
                                                         uploader <- return $ getTitle (chForN (chForN (chForN li 3) 5) 1)
                                                         description <- return $ getTitle (chForN (chForN li 3) 9)
                                                         plays <- return $ getTitle (chForN (chForN (chForN li 3) 7) 1)
                                                         likes <- return $ getTitle (chForN (chForN (chForN li 3) 7) 3)
                                                         numcomments <- return $ getTitle (chForN (chForN (chForN li 3) 7) 5)
                                                         return (Video { url = ("http://vimeo.com" ++ vidpath)
                                                                       , title = title
                                                                       , duration = duration
                                                                       , uploaded = uploaded
                                                                       , uploader = uploader
                                                                       , description = ((T.unpack . T.strip . T.pack) description)
                                                                       , plays = plays
                                                                       , likes = likes
                                                                       , numcomments = numcomments
                                                                       , thumbnail = ""
                                                                       })
                                    (TagLeaf _) -> Nothing
                                                    ) liLst
        let shorterresults = map (\(Just x) -> x) $ filter (\x -> case x of
                                            Just _ -> True
                                            Nothing -> False) results
        return shorterresults


startsearch edit lst label nextScreen showResults = do
        text <- getEditText edit
        setText label $ T.pack ("Search: " ++ (T.unpack text))
        nextScreen
        let searchquery = subRegex (mkRegex "[^a-zA-Z0-9]+") (T.unpack text) "+"
        results <- dosearch $ "http://vimeo.com/search/sort:relevant/format:detail?q=" ++ subRegex (mkRegex "^[^a-zA-Z0-9]+|[^a-zA-Z0-9]+$") searchquery ""
        forM_ results (\vid -> addToList lst (T.pack $ show $ title vid) =<< plainText (T.pack $ show $ title vid))
        showResults

main :: IO ()
main = do
        (searchui, searchfg, e) <- searchscreen
        (loadui, loadfg) <- loadingscreen
        (resultsui, resultsfg, lst, label) <- resultsscreen

        c <- newCollection
        changeToSearch <- addToCollection c searchui searchfg
        changeToLoading <- addToCollection c loadui loadfg
        changeToResults <- addToCollection c resultsui resultsfg

        e `onActivate` \this -> startsearch this lst label changeToLoading changeToResults

        runUi c defaultContext
