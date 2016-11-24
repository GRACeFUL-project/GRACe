{-# LANGUAGE FlexibleInstances #-}
import Port
import GCM
import CP
import Examples
import Control.Monad

-- Wind power production in MWh by hour
windPowerProduction25_08_2016 :: [Int]
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

-- Estimated power production in MWh by hour from nuclear
nuclearPowerProduction25_08_2016 :: [Int]
nuclearPowerProduction25_08_2016 =
    replicate 24 6335 -- Assume they are always producing
                      -- at full capacity

-- Total power production in MWh by hour
totalPowerProduction25_08_2016 :: [Int]
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

-- Total power consumption in MWh by hour
totalPowerConsumption25_08_2016 :: [Int]
totalPowerConsumption25_08_2016 =
    [
        10892,
        10566,
        10371,
        10359,
        10551,
        10842,
        12551,
        14058,
        14724,
        14918,
        15262,
        15281,
        15116,
        15176,
        14894,
        14872,
        14760,
        14838,
        14528,
        14450,
        14321,
        13491,
        12611,
        11554
    ]

-- Swedish energy price in EUR/MWh, averaged across SE1-SE4
averageEnergyPrice25_08_2016 :: [Int]
averageEnergyPrice25_08_2016 = map round
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

instance Num [Int] where
    (+) = zipWith (+)
    (*) = zipWith (*)
    (-) = zipWith (-)
    fromInteger = repeat . fromInteger
    abs = map abs
    signum = map signum

fillList :: (Num a, Ord a, CPType a) => [Port a] -> [([Maybe (Port a)], [Port a])] -> GCM ()
fillList p lst = sequence_ [fill (p !! i) [(m !! i, prt !! i) | (m, prt) <- lst] | i <- [0..(length p - 1)]]

constraintModel :: GCM ()
constraintModel =
    do
        -- Total power consumption
        powerConsumption       <- mapM source totalPowerConsumption25_08_2016

        -- Capacity for hydropower
        hydropowerCapacity     <- replicateM 24 $ source 16200
        -- Total hydropower production
        hydropowerProduction   <- replicateM 24 createPort

        -- The factor by which wind has to increase to meet demand
        windFactor             <- createPort
        component $ do
                        wf <- value windFactor
                        assert $ wf `inRange` (0, 10)
        --  Wind production
        windCapacity         <- replicateM 24 createPort
        -- Wind production increase
        component $ mapM_ (\(wp, n) -> do
                                        v <- value wp
                                        f <- value windFactor
                                        assert $ v === f * (lit n)
                          ) $ zip windCapacity windPowerProduction25_08_2016
        windProduction <- replicateM 24 createPort 

        residual <- replicateM 24 createPort
        mapM_ ((flip set) 0) residual

        fillList powerConsumption [(map Just windCapacity, windProduction), (map Just hydropowerCapacity, hydropowerProduction), (replicate 24 Nothing, residual)]

        g <- createGoal
        component $
            do  
                vg <- value g
                vw <- value windFactor
                assert $ vg === 0 - vw

        mapM_ (\(p, i) -> output p ("water " ++ (show i))) $ zip hydropowerProduction [0..]
        output windFactor "wind factor"
