module UpdateTests exposing (all)

import Test exposing (..)
import Update.GoNoGoTests



-- import Update.DotProbeTests
-- import Update.StopSignalTests
-- import Update.VisualSearchTests


all : Test
all =
    describe "Update Test Suite"
        [ Update.GoNoGoTests.all

        -- , Update.DotProbeTests.all
        -- , Update.StopSignalTests.all
        -- , Update.VisualSearchTests.all
        ]
