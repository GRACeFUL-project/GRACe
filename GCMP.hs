module GCMP where
import GL
import QuickCheck.GenT

type GCMP a = GenT (GCM a)
