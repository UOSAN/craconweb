module Tests exposing (..)

import CycleTests
import Expect
import Fuzz exposing (int, list, string, tuple)
import ResultTests
import String
import Test exposing (..)
import UpdateTests


all : Test
all =
    describe "Test Suite"
        [ CycleTests.all
        , ResultTests.all
        , UpdateTests.all
        ]
