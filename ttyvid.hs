{-# LANGUAGE OverloadedStrings #-}
import Graphics.Vty.Widgets.All
import qualified Data.Text as T
import Network.HTTP
import Text.Regex
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Debug.Trace
import Control.Monad

-- Holds metadata for a video
data Video = Video { url         :: String -- The URL of the page of the video, not of the video file itself
                   , title       :: String
                   , duration    :: String
                   , uploaded    :: String
                   , uploader    :: String
                   , description :: String
                   , plays       :: String
                   , likes       :: String
                   , numcomments :: String
                   , thumbnail   :: String
                   } deriving (Show)

-- Holds the UI elements of the right pane on the search results screen
data RightPane = RightPane { titleWidget       :: Widget FormattedText
                           , durationWidget    :: Widget FormattedText
                           , uploadedWidget    :: Widget FormattedText
                           , uploaderWidget    :: Widget FormattedText
                           , descriptionWidget :: Widget FormattedText
                           , playsWidget       :: Widget FormattedText
                           , likesWidget       :: Widget FormattedText
                           , numcommentsWidget :: Widget FormattedText
                           }

-- Returns a string of the content hosted at url x
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
        
        radios1 <- hBox cb1 =<< hBox cb2 cb3
        radios2 <- hBox cb4 =<< hBox cb5 =<< hBox cb6 cb7

        lst <- newList (getNormalAttr defaultContext) 3
        blst <- bordered lst
        lftui <- vBox label =<< vBox radios1 =<< vBox radios2 blst

        title       <- textWidget wrap "Placeholder Text"
        duration    <- textWidget wrap "Placeholder Text"
        uploaded    <- textWidget wrap "Placeholder Text"
        uploader    <- textWidget wrap "Placeholder Text"
        description <- textWidget wrap "Placeholder Text"
        plays       <- textWidget wrap "Placeholder Text"
        likes       <- textWidget wrap "Placeholder Text"
        numcomments <- textWidget wrap "Placeholder Text"

        rtui <- bordered =<< vBox title =<< vBox duration =<< vBox uploaded =<< vBox uploader =<< vBox description =<< vBox plays =<< vBox likes numcomments

        ui <- hBox lftui =<< hFixed 80 rtui
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

        return (ui, fg, lst, label, (RightPane title duration uploaded uploader description plays likes numcomments))

-- Builds and returns the initial screen for searching
searchscreen :: IO (Widget (VCentered (HCentered (HFixed (Box FormattedText Edit)))), Widget FocusGroup, Widget Edit)
searchscreen = do
        e <- editWidget
        b <- (plainText "Enter a search: ") <++> (return e) >>= hFixed 40
        ui <- centered b
        fg <- newFocusGroup
        addToFocusGroup fg b
        return (ui, fg, e)

-- Builds and returns a simple screen that says "Loading..."
loadingscreen :: IO (Widget (VCentered (HCentered FormattedText)), Widget FocusGroup)
loadingscreen = do
        b <- (plainText "Loading...")
        ui <- centered b
        fg <- newFocusGroup
        addToFocusGroup fg b
        return (ui, fg)

-- Gets the attributes from a TagTree
attrs :: (TagTree String) -> Maybe [Attribute String]
attrs (TagBranch _ ats _) = Just ats
attrs (TagLeaf _) = Nothing

-- Gets the list of children for a given TagBranch
ch :: (TagTree String) -> [TagTree String]
ch (TagBranch _ _ cs) = cs

-- Gets the list of children for the Nth element in the passed in list of TagTrees
chForN :: [(TagTree String)] -> Int -> [TagTree String]
chForN tb n = ch $ tb !! n 

-- Concatenates and returns all the text from within a list of TagTrees.
-- Assumes TagLeafs only have a TagText node.
getTitle :: [TagTree String] -> String
getTitle [] = ""
getTitle ((TagLeaf (TagText txt)):tags) = txt ++ (getTitle tags)
getTitle ((TagBranch _ _ [(TagLeaf (TagText txt))]):tags) = txt ++ (getTitle tags)

-- Given a URL for a search on vimeo, return all the Video elements representing
-- the videos on the page
fetchsearchresults :: String -> IO [Video]
fetchsearchresults url = do
        resultSrc <- openURL url
        let uniTree = universeTree $ tagTree $ parseTags resultSrc
        let tree = (filter (\x -> ((flattenTree [x]) !! 0) ~== ("<ol>" :: String)) uniTree) !! 0
        let results = case tree of
                          TagLeaf _ -> []
                          TagBranch _ _ liLst -> map(\x -> case x of
                                    (TagBranch _ _ li) -> do
                                                         vidpath     <- lookup "href" =<< (attrs $ li !! 1)
                                                         title       <- return $ getTitle (chForN (chForN (chForN li 3) 1) 1)
                                                         duration    <- return $ getTitle (chForN (chForN li 3) 3)
                                                         uploaded    <- lookup "title" =<< (attrs $ (chForN (chForN li 3) 5) !! 3)
                                                         uploader    <- return $ getTitle (chForN (chForN (chForN li 3) 5) 1)
                                                         description <- return $ getTitle (chForN (chForN li 3) 9)
                                                         plays       <- return $ getTitle (chForN (chForN (chForN li 3) 7) 1)
                                                         likes       <- return $ getTitle (chForN (chForN (chForN li 3) 7) 3)
                                                         numcomments <- return $ getTitle (chForN (chForN (chForN li 3) 7) 5)
                                                         return (Video { url         = ("http://vimeo.com" ++ vidpath)
                                                                       , title       = title
                                                                       , duration    = duration
                                                                       , uploaded    = uploaded
                                                                       , uploader    = uploader
                                                                       , description = ((T.unpack . T.strip . T.pack) description)
                                                                       , plays       = plays
                                                                       , likes       = likes
                                                                       , numcomments = numcomments
                                                                       , thumbnail   = ""
                                                                       })
                                    (TagLeaf _) -> Nothing
                                                    ) liLst
        let shorterresults = map (\(Just x) -> x) $ filter (\x -> case x of
                                            Just _ -> True
                                            Nothing -> False) results
        return shorterresults

-- Build an item for the list on the results screen
mkListItem :: Video -> IO (Widget (Box (Box (Box FormattedText HFill) FormattedText) (Box (Box FormattedText HFill) FormattedText)))
mkListItem vid = (plainText (T.pack $ title vid) <++> hFill ' ' 1 <++> plainText (T.pack $ duration vid)) <--> (plainText (T.pack $ "By: " ++ uploader vid) <++> hFill ' ' 1 <++> plainText (T.pack $ plays vid))

runsearch edit lst label rightpane nextScreen showResults = do
        text <- getEditText edit
        setText label $ T.pack ("Search: " ++ (T.unpack text))
        nextScreen
        let no_nonalphanum = subRegex (mkRegex "[^a-zA-Z0-9]+") (T.unpack text) "+"
        let stripped = subRegex (mkRegex "^[^a-zA-Z0-9]+|[^a-zA-Z0-9]+$") no_nonalphanum ""
        results <- fetchsearchresults $ "http://vimeo.com/search/sort:relevant/format:detail?type=videos&q=" ++ stripped
        lst `onSelectionChange` (\(SelectionOn a _ _) -> do let vid = results !! a
                                                            (titleWidget rightpane)       `setText` (T.pack $ title vid)
                                                            (durationWidget rightpane)    `setText` (T.pack $ duration vid)
                                                            (uploadedWidget rightpane)    `setText` (T.pack $ uploaded vid)
                                                            (uploaderWidget rightpane)    `setText` (T.pack $ uploader vid)
                                                            (descriptionWidget rightpane) `setText` (T.pack $ description vid)
                                                            (playsWidget rightpane)       `setText` (T.pack $ plays vid)
                                                            (likesWidget rightpane)       `setText` (T.pack $ likes vid)
                                                            (numcommentsWidget rightpane) `setText` (T.pack $ numcomments vid))
        forM_ results (\vid -> do widget <- mkListItem vid
                                  addToList lst vid widget)

        showResults

main :: IO ()
main = do
        (searchui, searchfg, e) <- searchscreen
        (loadui, loadfg) <- loadingscreen
        (resultsui, resultsfg, lst, label, rightpane) <- resultsscreen

        c <- newCollection
        changeToSearch <- addToCollection c searchui searchfg
        changeToLoading <- addToCollection c loadui loadfg
        changeToResults <- addToCollection c resultsui resultsfg

        e `onActivate` \this -> runsearch this lst label rightpane changeToLoading changeToResults

        runUi c defaultContext
