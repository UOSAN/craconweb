module Result.VisualSearchTests exposing (all)

import Expect
import Game exposing (BorderType(..), Direction(..), Layout(..), LogEntry(..))
import Game.Result
import Test exposing (..)


all : Test
all =
    describe "Visual Search Tests"
        [ test "Correct Selection" <|
            \() ->
                Expect.true "Correct Selection"
                    (Game.Result.isCorrect { gameSlug = "visualsearch" }
                        { id = Nothing
                        , sessionId = "sessionId"
                        , sort = 0
                        , fixation = Just 1495327801376
                        , selection = Just 1495327804391
                        , pictures = Just 1495327801876
                        , redcross = Nothing
                        , probe = Nothing
                        , border = Nothing
                        , timeout = Nothing
                        , rest = Just 1495327805392
                        , interval = Nothing
                        , width = Just 4
                        , height = Just 4
                        , blue = False
                        , gray = False
                        , dash = False
                        , targetIndex = 11
                        , selectedIndex = 11
                        , startIndex = 0
                        , images =
                            [ "2988761543824519516"
                            , "4260027113823189510"
                            , "8845805633824319507"
                            , "8754370733822399511"
                            , "3504457123823159508"
                            , "2494187463824499516"
                            , "4364296433824149510"
                            , "8170185243823339508"
                            , "643837883823469513"
                            , "8500237403822389507"
                            , "3252014013824529514"
                            , "5352283793819429514"
                            , "3257302483823149513"
                            , "4565365463823199516"
                            , "5321754783823229511"
                            , "144480343823449507"
                            ]
                        }
                    )
        , test "Incorrect Selection" <|
            \() ->
                Expect.false "Incorrect Selection"
                    (Game.Result.isCorrect { gameSlug = "visualsearch" }
                        { id = Nothing
                        , sessionId = "sessionId"
                        , sort = 0
                        , fixation = Just 1495328323504
                        , selection = Just 1495328324620
                        , pictures = Just 1495328324019
                        , redcross = Nothing
                        , probe = Nothing
                        , border = Nothing
                        , timeout = Nothing
                        , rest = Just 1495328325636
                        , interval = Nothing
                        , width = Just 4
                        , height = Just 4
                        , blue = False
                        , gray = False
                        , dash = False
                        , targetIndex = 12
                        , selectedIndex = 0
                        , startIndex = 0
                        , images =
                            [ "5699024483824199510"
                            , "6197713983824219512"
                            , "5321754783823229511"
                            , "2008476443823519514"
                            , "3828527133824549508"
                            , "274072046967653701"
                            , "3504457123823159508"
                            , "392012383823459512"
                            , "3582034143824539508"
                            , "536409513824429514"
                            , "1630240873824469514"
                            , "8424400623823349507"
                            , "8345941453819549512"
                            , "4861755103824169509"
                            , "2494187463824499516"
                            , "130748266969563708"
                            ]
                        }
                    )
        , test "Timeout" <|
            \() ->
                Expect.false "Timeout is not correct"
                    (Game.Result.isCorrect { gameSlug = "visualsearch" }
                        { id = Nothing
                        , sessionId = "sessionId"
                        , sort = 0
                        , fixation = Just 1495328603235
                        , selection = Nothing
                        , pictures = Just 1495328603751
                        , redcross = Nothing
                        , probe = Nothing
                        , border = Nothing
                        , timeout = Just 1495328606735
                        , rest = Just 1495328607751
                        , interval = Nothing
                        , width = Just 4
                        , height = Just 4
                        , blue = False
                        , gray = False
                        , dash = False
                        , targetIndex = 6
                        , selectedIndex = 0
                        , startIndex = 0
                        , images =
                            [ "1882365243824479512"
                            , "2253137213823529512"
                            , "3504457123823159508"
                            , "300015136968613708"
                            , "6197713983824219512"
                            , "4083635623824559508"
                            , "2051267963819309509"
                            , "6486496543824229509"
                            , "2227436633824489512"
                            , "6913464483823289507"
                            , "8424400623823349507"
                            , "26808586967643704"
                            , "6658947263823279514"
                            , "2499595343823539516"
                            , "8170185243823339508"
                            , "536409513824429514"
                            ]
                        }
                    )
        ]
