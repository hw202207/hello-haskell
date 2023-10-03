{-# LANGUAGE ImportQualifiedPost #-}

module Sort where

import Data.ByteString.Char8 qualified as BS
import Data.List
import Data.Text qualified as T
import Data.Text.ICU qualified as ICU
-- import Data.Text.ICU.Collate qualified as ICU hiding (collate, sortKey)
-- improt Text.Collate qualified as TC

main :: IO ()
main = do
    let xs =
            [ "id"
            , "organization_id"
            , "locked_by"
            , "lock_reasons"
            , "lock_note"
            , "lock_source"
            , "sent_notification_emails"
            , "created_at"
            ] ::
                [String]
    let ys = fmap T.pack xs
    let zs = fmap BS.pack xs
    print (sort xs)
    print (sort ys)
    print (sortBy myCompare ys)
    -- print (sortBy ( TC.collate "en_US" ) ys)
    print (sort zs)

myCompare :: T.Text -> T.Text -> Ordering
myCompare = ICU.collate enUSCollator

enUSCollator :: ICU.Collator
enUSCollator = ICU.collator (ICU.Locale "en_US")

-- [ ICU.CaseFirst (Just ICU.LowerFirst)
-- , ICU.CaseLevel True
-- , ICU.NormalizationMode True
-- , ICU.HiraganaQuaternaryMode True
-- ]
