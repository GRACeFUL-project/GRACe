module CLDlib (library) where

import qualified Interfaces.MZASTBase as HZ
import Library

-- Missing urls to appropriate images
library :: Library
library = Library "cld"
  [ Item "factor" "Factor" "pathToFactorImage" $
      cldNode ::: "obsSign" # (tMaybe tSign) .-> "numIn" # tInt .->
      tGCM (tPair ("influences" # tList (tPort tSign))
                  ("outSign" # tPort tSign))

  , Item "relation" "Causal relation" "pathToArrowImage" $
      relate ::: "sign" # tSign .-> tGCM (tPair ("fromNode" # tPort tSign)
                                                ("toNode"  #  tPort tSign)
                                         )

  , Item "plus arrow" "Positive relation" "pathToParrowImage" $
      pArrow ::: tGCM (tPair ("fromNode" # tPort tSign)
                             ("toNode"  #  tPort tSign)
                      )

  , Item "minus arrow" "Negative relation" "pathToMarrowImage" $
      mArrow ::: tGCM (tPair ("fromNode" # tPort tSign)
                             ("toNode"  #  tPort tSign)
                      )
  ]

add :: Port Sign -> CPExp Sign -> CPExp Sign -> CP ()
add p x y = do
  vp <- value p
  assert $ (x === y) ==> (vp === x)
  assert $ ((x /== y) .&& (x /== Lit Z) .&& (y /== Lit Z)) ==> (vp === Lit Q)
  assert $ ((x === Lit Z) .|| (y === Lit Z)) ==> (vp === (x + y))

constructSum :: [Port Sign] -> GCM (Port Sign)
constructSum [] = do
  p <- createPort
  set p 0
  return p
constructSum (x:xs) = do
  hereResult <- createPort
  restResult <- constructSum xs
  vx <- value x
  vr <- value restResult
  component $ add hereResult vx vr
  return hereResult

pArrow :: GCM (Port Sign, Port Sign)
pArrow = do
  p <-createPort
  q <- createPort
  link p q
  return (p, q)

mArrow :: GCM (Port Sign, Port Sign)
mArrow = do
  p <- createPort
  q <- createPort
  component $ do
    pv <- value p
    qv <- value q
    assert $ qv === (-1) * pv
  return (p, q)

relate :: Sign -> GCM (Port Sign, Port Sign)
relate P = pArrow
relate M = mArrow
relate _ = do
  p <- createPort
  q <- createPort
  return (p,q)


(-+>) :: Port Sign -> Port Sign -> GCM ()
p -+> q = do
 (p0, q0) <- pArrow
 link p p0
 link q q0

(-->) :: Port Sign -> Port Sign -> GCM ()
p --> q = do
 (p0, q0) <- mArrow
 link p p0
 link q q0

cldNode :: Maybe Sign -> Int -> GCM ([Port Sign], Port Sign)
cldNode obsSign numIn = do
  outSign <- createPort
  influences <- mapM (\_ -> createPort) [1..numIn]
  influence <- constructSum influences
  component $ do
    i <- value influence
    o <- value outSign
    case obsSign of
      Just s -> do
        assert $ (o === lit s)
        if numIn > 0
          then (assert $ (i === lit s))
          else return ()
      Nothing -> if numIn > 0
        then assert $ (i === o)
        else return ()
  return (influences, outSign)

