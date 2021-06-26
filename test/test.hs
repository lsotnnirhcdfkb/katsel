module Main where

import Test

main :: IO ()
main = run_test_suite $ TestSuite
    [ Describe "thing"
        [ When "given nothing"
            [ ItCan "do something" True
            ]
        ]
    ]
