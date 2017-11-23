default: multiSoln0a.run multiSoln0b.run

%.run: %.fzn
	fzn-gecode $< | tee $@

%.fzn: %.mzn
	MZN_STDLIB_DIR=/home/patrikj/Downloads/MiniZincIDE-2.1.0-bundle-linux-x86_64/share/minizinc/ mzn2fzn $<
