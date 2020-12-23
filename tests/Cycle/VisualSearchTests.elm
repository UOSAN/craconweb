module Cycle.VisualSearchTests exposing (all)

import Expect
import Game exposing (BorderType(..), Direction(..), Layout(..), LogEntry(..))
import Game.Cycle
import Test exposing (..)


all : Test
all =
    describe "Visual Search Tests"
        [ test "Correct Selection" <|
            \() ->
                Expect.equal
                    [ { id = Nothing
                      , sessionId = "sessionId"
                      , sort = 0
                      , fixation = Just 1495327801376
                      , selection = Just 1495327804391
                      , pictures = Just 1495327801876
                      , redcross = Nothing
                      , probe = Nothing
                      , border = Nothing
                      , timeout = Nothing
                      , rest = Nothing
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
                    ]
                    (Game.Cycle.generate "sessionId"
                        [ EndTrial 1495327805392
                        , AcceptSelection { desired = 11, actual = 11 } 1495327804391
                        , BeginInput 1495327801876
                        , BeginDisplay
                            (Just
                                (SelectGrid None
                                    { columns = 4
                                    , images =
                                        [ { url = "http://localhost:8654/repo/09351084b44158b897696649c8d26f11.png", id = "2988761543824519516" }
                                        , { url = "http://localhost:8654/repo/8c32e32fb6a060810758faa5af457351.png", id = "4260027113823189510" }
                                        , { url = "http://localhost:8654/repo/d4a0750500c9ad5cfe94e53b6ed3a52d.png", id = "8845805633824319507" }
                                        , { url = "http://localhost:8654/repo/df01a1fc573101cf57ce3a9bdc4d15ac.png", id = "8754370733822399511" }
                                        , { url = "http://localhost:8654/repo/5406f8fb796a6b2d63135d462055c699.png", id = "3504457123823159508" }
                                        , { url = "http://localhost:8654/repo/0ca809a966a10f2ef634a5bf5318eb6c.png", id = "2494187463824499516" }
                                        , { url = "http://localhost:8654/repo/fffc2c1c3a056f3591f93df378204495.png", id = "4364296433824149510" }
                                        , { url = "http://localhost:8654/repo/3ae125b920cf79a28ef5b02a12dd7b5e.png", id = "8170185243823339508" }
                                        , { url = "http://localhost:8654/repo/120d1ceab457741622e1e50151251e73.png", id = "643837883823469513" }
                                        , { url = "http://localhost:8654/repo/caf14d81381f0762523f0788a4abeb8a.png", id = "8500237403822389507" }
                                        , { url = "http://localhost:8654/repo/64ef36e2dc1d3057898bb0a1c4277598.png", id = "3252014013824529514" }
                                        , { url = "http://localhost:8654/repo/0bc3d2091c6c2944f53ce38e58038825.png", id = "5352283793819429514" }
                                        , { url = "http://localhost:8654/repo/c93cc4417e1d25f587c9083c170fa9d5.png", id = "3257302483823149513" }
                                        , { url = "http://localhost:8654/repo/07bafd94cf2c2018d5298a5725b5ad48.png", id = "4565365463823199516" }
                                        , { url = "http://localhost:8654/repo/7a0186c62f81cd2e5396bba36dfc6b10.png", id = "5321754783823229511" }
                                        , { url = "http://localhost:8654/repo/d82d14c6303b3a8280d45cec25c58bdb.png", id = "144480343823449507" }
                                        ]
                                    , goIndex = 11
                                    }
                                )
                            )
                            1495327801876
                        , BeginDisplay (Just (Fixation None)) 1495327801376
                        , BeginTrial 1495327801376
                        ]
                    )
        , test "Incorrect Selection" <|
            \() ->
                Expect.equal
                    [ { id = Nothing
                      , sessionId = "sessionId"
                      , sort = 0
                      , fixation = Just 1495328323504
                      , selection = Just 1495328324620
                      , pictures = Just 1495328324019
                      , redcross = Nothing
                      , probe = Nothing
                      , border = Nothing
                      , timeout = Nothing
                      , rest = Nothing
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
                    ]
                    (Game.Cycle.generate "sessionId"
                        [ EndTrial 1495328325636
                        , AcceptSelection { desired = 12, actual = 0 } 1495328324620
                        , BeginInput 1495328324019
                        , BeginDisplay
                            (Just
                                (SelectGrid None
                                    { columns = 4
                                    , images =
                                        [ { url = "http://localhost:8654/repo/85a587a73e80051b4d8095bd69091f4d.png", id = "5699024483824199510" }
                                        , { url = "http://localhost:8654/repo/d2771af0553929d5b7087cc9be924697.png", id = "6197713983824219512" }
                                        , { url = "http://localhost:8654/repo/7a0186c62f81cd2e5396bba36dfc6b10.png", id = "5321754783823229511" }
                                        , { url = "http://localhost:8654/repo/9fd0424197618516b3151329259f1d5b.png", id = "2008476443823519514" }
                                        , { url = "http://localhost:8654/repo/6bedf16e96322c615bc887a3de53584b.png", id = "3828527133824549508" }
                                        , { url = "http://localhost:8654/repo/c83348a3e3060c1488147d002ab55f65.png", id = "274072046967653701" }
                                        , { url = "http://localhost:8654/repo/5406f8fb796a6b2d63135d462055c699.png", id = "3504457123823159508" }
                                        , { url = "http://localhost:8654/repo/1aa521a4a2dc0789164054ebc931f0e1.png", id = "392012383823459512" }
                                        , { url = "http://localhost:8654/repo/3eda4fc66cec6ca7b941d7da86c75199.png", id = "3582034143824539508" }
                                        , { url = "http://localhost:8654/repo/8c171c15d8bf8e0ed58424022647050a.png", id = "536409513824429514" }
                                        , { url = "http://localhost:8654/repo/1f5ce3a60432bc27e6984aaf624ea985.png", id = "1630240873824469514" }
                                        , { url = "http://localhost:8654/repo/c149a28fb64b3f33e05d9049331182d2.png", id = "8424400623823349507" }
                                        , { url = "http://localhost:8654/repo/7c78f75fce1a8d4db2474d82f1a6f904.png", id = "8345941453819549512" }
                                        , { url = "http://localhost:8654/repo/cd18c12f7e6d9bd4dc05be052a7f4269.png", id = "4861755103824169509" }
                                        , { url = "http://localhost:8654/repo/0ca809a966a10f2ef634a5bf5318eb6c.png", id = "2494187463824499516" }
                                        , { url = "http://localhost:8654/repo/cdab147173ee66c0d9e3570c52040557.png", id = "130748266969563708" }
                                        ]
                                    , goIndex = 12
                                    }
                                )
                            )
                            1495328324019
                        , BeginDisplay (Just (Fixation None)) 1495328323504
                        , BeginTrial 1495328323504
                        ]
                    )
        , test "Timeout" <|
            \() ->
                Expect.equal
                    [ { id = Nothing
                      , sessionId = "sessionId"
                      , sort = 0
                      , fixation = Just 1495328603235
                      , selection = Nothing
                      , pictures = Just 1495328603751
                      , redcross = Nothing
                      , probe = Nothing
                      , border = Nothing
                      , timeout = Just 1495328606735
                      , rest = Nothing
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
                    ]
                    (Game.Cycle.generate "sessionId"
                        [ EndTrial 1495328607751
                        , Timeout { desired = False } 1495328606735
                        , BeginInput 1495328603751
                        , BeginDisplay
                            (Just
                                (SelectGrid None
                                    { columns = 4
                                    , images =
                                        [ { url = "http://localhost:8654/repo/169ca933746ed144932b9f4f789ae44f.png", id = "1882365243824479512" }
                                        , { url = "http://localhost:8654/repo/849c00e313180b9454340561213173cf.png", id = "2253137213823529512" }
                                        , { url = "http://localhost:8654/repo/5406f8fb796a6b2d63135d462055c699.png", id = "3504457123823159508" }
                                        , { url = "http://localhost:8654/repo/54be1aaa459a4971248883db4d134186.png", id = "300015136968613708" }
                                        , { url = "http://localhost:8654/repo/d2771af0553929d5b7087cc9be924697.png", id = "6197713983824219512" }
                                        , { url = "http://localhost:8654/repo/acb149b15c2940ec5eb28d4a15eadb42.png", id = "4083635623824559508" }
                                        , { url = "http://localhost:8654/repo/fac3e9a8dc40b04d8bcbda61d53c176d.png", id = "2051267963819309509" }
                                        , { url = "http://localhost:8654/repo/cf7358c2f71318399d17034f0a3dba66.png", id = "6486496543824229509" }
                                        , { url = "http://localhost:8654/repo/61963d307865e6288c5a6bcafe13fcc5.png", id = "2227436633824489512" }
                                        , { url = "http://localhost:8654/repo/b533fbfb92dbbf6f96b6a552c615020d.png", id = "6913464483823289507" }
                                        , { url = "http://localhost:8654/repo/c149a28fb64b3f33e05d9049331182d2.png", id = "8424400623823349507" }
                                        , { url = "http://localhost:8654/repo/13b528d3976a8068777d792e21e1d599.png", id = "26808586967643704" }
                                        , { url = "http://localhost:8654/repo/9cb5e4e5f5fa58f21d951437b49337b4.png", id = "6658947263823279514" }
                                        , { url = "http://localhost:8654/repo/f4540e69253222f0f8ade01df0ab40fe.png", id = "2499595343823539516" }
                                        , { url = "http://localhost:8654/repo/3ae125b920cf79a28ef5b02a12dd7b5e.png", id = "8170185243823339508" }
                                        , { url = "http://localhost:8654/repo/8c171c15d8bf8e0ed58424022647050a.png", id = "536409513824429514" }
                                        ]
                                    , goIndex = 6
                                    }
                                )
                            )
                            1495328603751
                        , BeginDisplay (Just (Fixation None)) 1495328603235
                        , BeginTrial 1495328603235
                        ]
                    )
        ]
