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

outputList :: (CPType a) => [Port a] -> String -> GCM ()
outputList xs s = mapM_ (\(p, i) -> output p (s ++ "[" ++ (show i)++ "]")) $ zip xs [0..]

constraintModel :: (Int, Int) -> [Int] -> [Int] -> GCM ()
constraintModel (start, len_) wind_ total_ =
    do
        let wind = take len_ $ drop start wind_ 
            total = take len_ $ drop start total_ 
            len = length wind
        -- Total power consumption
        powerConsumption       <- mapM source total

        -- Capacity for hydropower
        hydropowerCapacity     <- replicateM len $ source 162000 -- swedish hydropower capacity
        -- Total hydropower production
        hydropowerProduction   <- replicateM len createPort

        -- The factor by which wind has to increase to meet demand
        windFactor             <- createPort
        component $ do
                        wf <- value windFactor
                        assert $ wf `inRange` (0, 100)
        
        --  Wind production
        windCapacity         <- replicateM len createPort
        -- Wind production increase
        component $ mapM_ (\(wp, n) -> do
                                        v <- value wp
                                        f <- value windFactor
                                        assert $ v === f * (lit n)
                          ) $ zip windCapacity wind
        windProduction <- replicateM len createPort 

        windSavedEnergy <- replicateM len createPort
        component $ mapM_ (\(wse, i) -> do
                                         v <- value wse
                                         wp <- value (windProduction !! i)
                                         wc <- value (windCapacity !! i)

                                         assert $ (2*v) `inRange` ((wc - wp)-1, (wc - wp)) -- 2*v from losses in storage
                          ) $ zip windSavedEnergy [0..]

        (inputs, sumWindStored) <- sumGCM len
        zipWithM_ link inputs windSavedEnergy

        transferCapacity <- replicateM len $ source 10000
        transferTotal <- replicateM len createPort

        (inputs, transferTotalSum) <- sumGCM len
        zipWithM_ link inputs transferTotal

        energyStoreCapacity <- replicateM len createPort
        energyStoreUsed <- replicateM len createPort
        set (energyStoreCapacity !! 0) 0
        component $ mapM_ (\(es, i) -> do
                                        e <- value es
                                        esc <- value $ energyStoreCapacity !! (i - 1)
                                        wse <- value $ windSavedEnergy !! (i - 1)
                                        ese <- value $ energyStoreUsed !! (i - 1)
                                        assert $ e ===  esc + wse - ese
                          ) $ zip (tail energyStoreCapacity) [1..]

        (inputs, loadFactor) <- sumGCM len 
        zipWithM_ link inputs hydropowerProduction

        component $
            do
                lf <- value loadFactor
                assert $ lf `inRange` (0, lit (16200*(len `div` 2))) -- load factor on hydropower needs to not really exceed 0.5, as this would consume more energy than we have in total
        
        residual <- replicateM len createPort
        mapM_ ((flip set) 0) residual

        fillList powerConsumption [(map Just windCapacity, windProduction),
                                   (map Just energyStoreCapacity, energyStoreUsed),
                                   (map Just hydropowerCapacity, hydropowerProduction),
                                   (map Just transferCapacity, transferTotal),
                                   (replicate len Nothing, residual)]

        g <- createGoal
        component $
            do  
                vg <- value g
                vw <- value windFactor
                assert $ vg === 0 - vw

        (inputs, nonZeroSaved) <- sumGCM len
        component $ mapM_ (\(inpt, i) -> do
                                    saved <- value $ windSavedEnergy !! i
                                    inp   <- value $ inpt
                                    assert $ inp === (min' saved (min' saved 1))
                          ) $ zip inputs [0..]

        --outputList hydropowerProduction "water"
        --outputList windProduction "wind"

        --outputList energyStoreUsed "used"
        --outputList energyStoreCapacity "store"
        --outputList windSavedEnergy "wind saved"
        --outputList powerConsumption "power consumption"
        --outputList windCapacity "wind capacity"
        --outputList windProduction "wind production"

        output windFactor "wind factor"
        output transferTotalSum "transfer"
        --output nonZeroSaved "non zero saved"
        --output (energyStoreCapacity !! (len - 1)) "wind saved energy"
