GHC=ghc
SRC=src
OUT=out
FLAGS=--make -O -i$(SRC) -hidir $(OUT) -odir $(OUT) 


rest: $(SRC)/Rest.hs
	$(GHC) $(FLAGS) -main-is Rest -o $@ $<
