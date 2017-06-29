GHC=ghc
SRC=src
OUT=out
FLAGS=--make -O -i$(SRC) -hidir $(OUT) -odir $(OUT)

default:
	stack build

rest: RestAPI/Main.hs
	$(GHC) $(FLAGS) -o $@ $<
