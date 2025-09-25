{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad (unless)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isLower, isUpper)
import Data.Either (partitionEithers)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Read (decimal)
import Data.List (sortOn)
import Data.Ord (Down(..))
import Network.HTTP.Simple (Request, getResponseBody, httpLBS)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath ((</>))

main :: IO ()
main = do
  let tempDir = "./temp"
  makeSureTempDir tempDir

  let rawFilePath = tempDir </> "0--shanren00.dict.yaml"
  let rawFileUrl = "https://raw.githubusercontent.com/arpcn/rime-shanren3/refs/heads/master/0--shanren00.dict.yaml"
  downloadFile rawFileUrl rawFilePath

  rawContent <- extractContentFromRawFile rawFilePath
  rawDefinition <- parseRawTable rawContent

  freqTable <- readFreqTable "./single_char.tsv"

  codeMap <- generateCodeMap freqTable rawDefinition
  writeCodeMapToTSV codeMap "./dict.tsv"

  phrasesFreqTable <- readFreqTable "./multi_char.tsv"

  phrasesCodeMap <- generatePhrasesCodeMap phrasesFreqTable rawDefinition
  writeCodeMapToTSV phrasesCodeMap "./words.dict.tsv"
  TIO.putStrLn "Done"

makeSureTempDir :: FilePath -> IO ()
makeSureTempDir dirName = do
  dirExists <- doesDirectoryExist dirName
  if not dirExists
    then do
      createDirectoryIfMissing True dirName
      putStrLn $ "创建目录: " ++ dirName
    else putStrLn $ "目录已存在: " ++ dirName

downloadFile :: Request -> FilePath -> IO ()
downloadFile url destFilePath = do
  response <- httpLBS url
  let body = getResponseBody response
  LBS.writeFile destFilePath body
  print $ "文件下载成功: " ++ destFilePath

extractContentFromRawFile :: FilePath -> IO [Text]
extractContentFromRawFile rawFilePath = do
  raw <- TIO.readFile rawFilePath
  let allLines = T.lines raw
      linesAfterMarker = findMarker "###-----" allLines
  return $ filter keepLine linesAfterMarker

findMarker :: Text -> [Text] -> [Text]
findMarker marker allLines = case dropWhile (not . T.isPrefixOf marker) allLines of
  [] -> []
  (_ : rest) -> rest

keepLine :: Text -> Bool
keepLine line = not (T.null line) && not ("#" `T.isPrefixOf` line)

parseRawTable :: [Text] -> IO [(Text, ([Text], [Text]), Text)]
parseRawTable table = do
  let processed = map (processLine . T.stripEnd) table
      (warnings, validEntries) = partitionEithers processed
  unless (null warnings) $ do
    TIO.putStrLn "以下条目被跳过："
    mapM_ (TIO.putStrLn . formatWarning) warnings
  return validEntries
  where
    formatWarning (char, reason) = T.concat ["字: ", char, " 原因: ", reason]

processLine :: Text -> Either (Text, Text) (Text, ([Text], [Text]), Text)
processLine line = case parseLine line of
  Just (char, holocode) -> case generateStemAndCodeParts holocode of
    Right (stem, code) ->
      Right (char, code, stem)
    Left reason ->
      Left (char, reason)
  Nothing -> Left (line, "Unknown")

generateStemAndCodeParts :: Text -> Either Text (Text, ([Text], [Text]))
generateStemAndCodeParts holocode =
  let chars = T.unpack holocode
      uppers = take 2 [c | c <- chars, isUpper c]
      (remaining, warn1) = case length uppers of
        2 -> (uppers, Nothing)
        n -> (uppers, Just $ "大写字母不足，找到" <> T.pack (show n))

      lowers = take (2 - length remaining) [c | c <- chars, isLower c]
      (finalChars, warn2) = case length (remaining ++ lowers) of
        2 -> (remaining ++ lowers, Nothing)
        m -> (remaining ++ lowers, Just $ "总字符不足，仅找到" <> T.pack (show m))

      errorMsg = case catMaybes [warn1, warn2] of
        [] -> Nothing
        ws -> Just $ T.intercalate "；" ws

      stem = T.toLower $ T.pack $ take 2 finalChars
      codeParts = splitByUppers holocode
   in if T.length stem == 2
        then Right (stem, codeParts)
        else Left $ fromMaybe "未知错误" errorMsg

parseLine :: Text -> Maybe (Text, Text)
parseLine line =
  case T.words line of
    (charPart : _)
      | not (T.null charPart) ->
          let char = T.take 1 charPart
              codePart = case T.splitOn "〖" line of
                (_ : body : _) -> T.takeWhile (/= '〗') body
                _ -> T.empty
           in if T.null codePart
                then Nothing
                else Just (char, codePart)
    _ -> Nothing

splitByUppers :: Text -> ([Text], [Text])
splitByUppers = go [] [] T.empty
  where
    go uppers lowers current txt
      | T.null txt =
          let finalLowers =
                if T.null current
                  then lowers
                  else current : lowers
           in (reverse uppers, reverse finalLowers)
      | isUpper (T.head txt) =
          let newChar = T.take 1 txt
              newUppers = newChar : uppers
              newLowers =
                if T.null current
                  then lowers
                  else current : lowers
           in go newUppers newLowers T.empty (T.tail txt)
      | isLower (T.head txt) =
          let newChar = T.take 1 txt
           in go uppers lowers (T.append current newChar) (T.tail txt)
      | otherwise =
          go uppers lowers current (T.tail txt)

type FrequencyPair = (Text, Text)

type FrequencyTable = [FrequencyPair]

readFreqTable :: FilePath -> IO FrequencyTable
readFreqTable path = do
  content <- TIO.readFile path
  let lns = T.lines content
  let parsed = mapMaybe parse lns
  return $ sortByNumericFrequency parsed
  where
    parse line = case T.splitOn "\t" line of
      [char, freq] -> Just (char, freq)
      _ -> Nothing
    sortByNumericFrequency :: FrequencyTable -> FrequencyTable
    sortByNumericFrequency table = 
      map snd $ sortOn (Down . fst) $ map toNumeric table
      where
        toNumeric (char, freq) = 
          case decimal freq of
            Right (n :: Int, "") -> (n, (char, freq))
            _ -> (0, (char, freq))

type CodeMap = HM.HashMap Text FrequencyPair

generateCodeMap :: FrequencyTable -> [(Text, ([Text], [Text]), Text)] -> IO CodeMap
generateCodeMap freqTable rawDef = do
  let charToCodesMap = HM.fromList [(char, (codes, stem)) | (char, codes, stem) <- rawDef]
      processChars [] codeMap codeSet = (codeMap, codeSet)
      processChars ((char, freq) : rest) codeMap codeSet =
        case HM.lookup char charToCodesMap of
          Nothing -> processChars rest codeMap codeSet
          Just (codes, _) ->
            let candidateCode = deriveCandidateCode codes
             in if T.null candidateCode
                  then processChars rest codeMap codeSet
                  else
                    let shortestCode = findShortestUniquePrefix candidateCode codeSet
                        finalCode = T.toLower shortestCode
                     in processChars rest (HM.insert char (finalCode, freq) codeMap) (HS.insert shortestCode codeSet)
      (finalCodeMap, finalCodeSet) = processChars freqTable HM.empty HS.empty
      codesCount = HS.size finalCodeSet
      totalCount = HM.size finalCodeMap
  putStrLn $ "总条目数：" ++ show totalCount
  putStrLn $ "唯一编码数：" ++ show codesCount
  return finalCodeMap
  where
    isCodeUsed :: Text -> HS.HashSet Text -> Bool
    isCodeUsed = HS.member

    deriveCandidateCode :: ([Text], [Text]) -> Text
    deriveCandidateCode (uppers, lowers) =
      let upperPart = T.concat uppers
          lowerFirstChar = case lowers of
            [] -> T.empty
            (x : _) -> T.take 1 x
          lowerLastChar = case reverse lowers of
            [] -> T.empty
            (x : _) -> T.take 1 x
          lowerPart = T.append lowerLastChar lowerFirstChar
          result = T.append upperPart lowerPart
       in result

    findShortestUniquePrefix :: Text -> HS.HashSet Text -> Text
    findShortestUniquePrefix candidate currentSet = go 1
      where
        go n
          | n > T.length candidate = candidate
          | otherwise =
              let prefix = T.take n candidate
               in if isCodeUsed prefix currentSet
                    then go (n + 1)
                    else prefix

generatePhrasesCodeMap :: FrequencyTable -> [(Text, ([Text], [Text]), Text)] -> IO CodeMap
generatePhrasesCodeMap freqTable rawDef = do
  let charToCodesMap = HM.fromList [(char, stem) | (char, _, stem) <- rawDef]
  return $ process freqTable charToCodesMap HM.empty
  where
    process [] _ codeMap = codeMap
    process ((phrase, freq) : rest) charToCodesMap codeMap =
      case traverse (`HM.lookup` charToCodesMap) (T.chunksOf 1 phrase) of
        Nothing -> process rest charToCodesMap codeMap
        Just stems -> process rest charToCodesMap (HM.insert phrase (T.concat stems, freq) codeMap)

writeCodeMapToTSV :: CodeMap -> FilePath -> IO ()
writeCodeMapToTSV codeMap outPath = do
  let lns = map toTSVLine (HM.toList codeMap)
  TIO.writeFile outPath (T.unlines lns)
  where
    toTSVLine (char, (code, freq)) = char `T.append` "\t" `T.append` code `T.append` "\t" `T.append` freq
