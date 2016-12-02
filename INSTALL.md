



2016-12-02: Patrik's install script

```shell
cd Downloads
wget https://github.com/MiniZinc/MiniZincIDE/releases/download/2.1.0/MiniZincIDE-2.1.0-bundle-linux-x86_64.tgz
cd ~/Downloads/MiniZincIDE-2.1.0-bundle-linux-x86_64
cp solns2out mzn2fzn fzn-gecode mzn-gecode minizinc \
   ~/bin
export MZN_STDLIB_DIR=~/Downloads/MiniZincIDE-2.1.0-bundle-linux-x86_64/share/minizinc
```
