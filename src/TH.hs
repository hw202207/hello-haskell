{-# LANGUAGE TemplateHaskellQuotes #-}

module TH where

import Language.Haskell.TH
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath

data EmailTemplate = EmailTemplate {name :: String, plainText :: String}
data Foo = Foo

compileTemplate :: EmailTemplate -> Q Exp
compileTemplate _et = do
    runIO $ do
        absolutePath <- getCurrentDirectory

        let
            rootDir :: String
            rootDir = absolutePath </> "templates"

            templateFilePath :: FilePath
            templateFilePath = rootDir </> "index.tsx"

        createDirectoryIfMissing True rootDir

        -- Write file with exactly one newline, for compliance with precommit checks.
        writeFile templateFilePath $ emailContent <> "\n"

    [|Foo|]
  where
    emailContent = "test"

run :: Foo -> IO ()
run _foo = pure ()
