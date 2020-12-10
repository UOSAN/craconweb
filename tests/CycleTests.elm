module CycleTests exposing (all)

import Cycle.DotProbeTests
import Cycle.FmriStopSignalTests
import Cycle.GoNoGoTests
import Cycle.StopSignalTests
import Cycle.VisualSearchTests
import Test exposing (..)


all : Test
all =
    describe "Cycle Test Suite"
        [ Cycle.GoNoGoTests.all
        , Cycle.DotProbeTests.all
        , Cycle.StopSignalTests.all
        , Cycle.VisualSearchTests.all
        , Cycle.FmriStopSignalTests.all
        ]
