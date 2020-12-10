module ResultTests exposing (all)

import Result.DotProbeTests
import Result.GoNoGoTests
import Result.StopSignalTests
import Result.VisualSearchTests
import Test exposing (..)


all : Test
all =
    describe "Cycle Test Suite"
        [ Result.GoNoGoTests.all
        , Result.DotProbeTests.all
        , Result.StopSignalTests.all
        , Result.VisualSearchTests.all
        ]
