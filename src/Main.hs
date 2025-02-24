{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Simple (httpLBS, parseRequest, getResponseBody)
import System.Directory (doesDirectoryExist, createDirectoryIfMissing)
import System.FilePath ((</>))
import qualified Data.ByteString.Lazy as LBS

-- 下载文件
downloadFile :: String -> FilePath -> IO ()
downloadFile url destFilePath = do
    request <- parseRequest url
    response <- httpLBS request
    let body = getResponseBody response
    LBS.writeFile destFilePath body
    putStrLn $ "文件下载成功: " ++ destFilePath

-- 主程序
main :: IO ()
main = do
    let dirPath = "./temp"
        filePath = dirPath </> "0--shanren00.dict.yaml"
        url = "https://raw.githubusercontent.com/arpcn/rime-shanren3/refs/heads/master/0--shanren00.dict.yaml"

    -- 确保目录存在
    dirExists <- doesDirectoryExist dirPath
    if not dirExists
        then do
            createDirectoryIfMissing True dirPath
            putStrLn $ "创建目录: " ++ dirPath
        else
            putStrLn $ "目录已存在: " ++ dirPath

    downloadFile url filePath

