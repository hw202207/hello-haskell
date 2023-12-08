{-# LANGUAGE TemplateHaskellQuotes #-}

module THUtil where

import Language.Haskell.TH
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath

data EmailTemplate = EmailTemplate {name :: String, plainText :: String}
data Foo = Foo

compileTemplate :: Name -> EmailTemplate ->  Q Exp
compileTemplate sth _et = do
    runIO $ do
        absolutePath <- getCurrentDirectory

        let
            rootDir :: String
            rootDir = absolutePath </> "templates"

            templateFilePath :: FilePath
            templateFilePath = rootDir </> "index.tsx"

        createDirectoryIfMissing True rootDir

        -- Write file with exactly one newline, for compliance with precommit checks.
        let result =
              nameBase sth ++ "\n"
              ++ emailContent
        writeFile templateFilePath $ result <> "\n"

    [|Foo|]
  where
    emailContent = "test"

run :: Foo -> IO ()
run _foo = pure ()
