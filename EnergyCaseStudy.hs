import GCM
import CP
import Examples

-- Wind power production in MWh by hour
windPowerProduction25_08_2016 :: [Float]
windPowerProduction25_08_2016 =
    [
        2201,
        2237,
        2222,
        2189,
        2020,
        1892,
        1663,
        1558,
        1316,
        1259,
        1330,
        1340,
        1387,
        1433,
        1517,
        1507,
        1394,
        1254,
        1114,
        1081,
        1243,
        1436,
        1440,
        1451
    ]

-- Total power production in MWh by hou
totalPowerProduction25_08_2016 :: [Float]
totalPowerProduction25_08_2016 =
    [
        11197,
        10970,
        10904,
        10889,
        10950,
        11606,
        13346,
        15605,
        16628,
        16622,
        16666,
        16670,
        16691,
        16371,
        16153,
        16018,
        15722,
        16324,
        16487,
        16777,
        16449,
        14773,
        13038,
        11444
    ]

-- Swedish energy price in öre/KWh, averaged across SE1-SE4
averageEnergyPrice25_08_2016 :: [Float]
averageEnergyPrice25_08_2016 =
    [
        22.63,
        22.40,
        20.97,
        21.16,
        21.98,
        24.95,
        30.52,
        35.04,
        36.58,
        35.96,
        36.06,
        35.65,
        35.54,
        34.74,
        34.52,
        34.08,
        32.94,
        34.84,
        36.65,
        38.78,
        37.96,
        34.16,
        28.88,
        26.50
    ]
