{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Play with safe DOM eDSL
module HelloDom where

import Prelude hiding (div)

-------------------------------------------------------------------------------
--                                 data types                                --
-------------------------------------------------------------------------------

newtype Comment = Comment String
    deriving (Eq, Show)

newtype Body = Body [BodyChild]
    deriving (Eq, Show)

data BodyChild
    = BCSection Section
    | BCDiv Div
    | BCComment Comment
    deriving (Eq, Show)

newtype Section = Section [SectionChild]
    deriving (Eq, Show)

data SectionChild
    = SCButton Button
    | SCComment Comment
    deriving (Eq, Show)

newtype Div = Div [DivChild]
    deriving (Eq, Show)

data DivChild
    = DCButton Button
    | DCComment Comment
    deriving (Eq, Show)

data Button = Button [String] String
    deriving (Eq, Show)

-------------------------------------------------------------------------------
--                          type classes and helper                          --
-------------------------------------------------------------------------------

class IsComment a b where
    comment :: a -> b
instance IsComment String Comment where
    comment :: [Char] -> Comment
    comment = Comment

instance IsComment String BodyChild where
    comment :: String -> BodyChild
    comment = BCComment . Comment

instance IsComment String SectionChild where
    comment :: String -> SectionChild
    comment = SCComment . Comment

class IsButton b where
    button :: [String] -> String -> b

instance IsButton Button where
    button :: [String] -> String -> Button
    button = Button
instance IsButton SectionChild where
    button :: [String] -> String -> SectionChild
    button attrs label = SCButton (button attrs label)
instance IsButton DivChild where
    button :: [String] -> String -> DivChild
    button attrs label = DCButton (button attrs label)

class IsSection a where
    section :: [SectionChild] -> a
instance IsSection Section where
    section :: [SectionChild] -> Section
    section = Section
instance IsSection BodyChild where
    section :: [SectionChild] -> BodyChild
    section = BCSection . section

class IsDiv a where
    div :: [DivChild] -> a
instance IsDiv Div where
    div :: [DivChild] -> Div
    div = Div
instance IsDiv BodyChild where
    div :: [DivChild] -> BodyChild
    div = BCDiv . div

class IsBody a where
    body :: [BodyChild] -> a
instance IsBody Body where
    body = Body

sample1 :: Body
sample1 =
    Body
        [ BCComment (Comment "<!-- foo1 -->")
        , BCSection $
            Section
                [ SCButton (Button [] "Button A")
                , SCComment (Comment "<!-- Button A -->")
                ]
        ]

sample2 :: Body
sample2 =
    body
        [ comment "<!-- foo1 -->"
        , section
            [ button [] "Button A"
            , comment "<!-- Button A -->"
            ]
        ]

render :: Body -> String
render = error "TBD"

main :: IO ()
main = do
    print (sample1 == sample2)
