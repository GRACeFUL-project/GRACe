module CLD where
import GCM
import CP

type S = Int

pArrow :: GCM (Port S, Port S)
pArrow = do
  p <- createPort
  q <- createPort
  link p q
  return (p, q)

mArrow :: GCM (Port S, Port S)
mArrow = do
  p <- createPort
  q <- createPort
  component $ do
    p <- value p
    q <- value q
    assert $ q === (-1) * p
  return (p, q)

(-+>) :: Port S -> Port S -> GCM ()
p -+> q = do
 (p0, q0) <- pArrow
 link p p0
 link q q0

(-->) :: Port S -> Port S -> GCM ()
p --> q = do
 (p0, q0) <- mArrow
 link p p0
 link q q0

combine :: GCM (Port S, Port S, Port S)
combine = do
  i0 <- createPort
  i1 <- createPort
  o  <- createPort
  component $ do
    o  <- value o
    i0 <- value i0
    i1 <- value i1
    assert $ (i0 === i1) ==> (o === i1)
    assert $ (i0 /== i1) ==> (o === 0 )
  return (i0, i1, o)
