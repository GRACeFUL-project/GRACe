# MiniZinc and Gecode installation guide

## Install from binary bundle

```shell
cd Downloads
wget https://github.com/MiniZinc/MiniZincIDE/releases/download/2.1.0/MiniZincIDE-2.1.0-bundle-linux-x86_64.tgz
tar zxf MiniZincIDE-2.1.0-bundle-linux-x86_64.tgz
cd ~/Downloads/MiniZincIDE-2.1.0-bundle-linux-x86_64
cp solns2out mzn2fzn fzn-gecode mzn-gecode minizinc \
   ~/.local/bin
export MZN_STDLIB_DIR=~/Downloads/MiniZincIDE-2.1.0-bundle-linux-x86_64/share/minizinc
```

## Install from source

Assumes git and subversion are installed and set up.

Choose install path (needs to be absolute)

```shell
export PREFIX=$HOME/.local/
```

The binaries will be placed in a `bin` subdirectory on this path.

### MiniZinc

#### Install prerequisites

```shell
sudo apt-get install clang cmake bison flex
```

#### Install MiniZinc

```shell
git clone git@github.com:MiniZinc/libminizinc.git
cd libminizinc
mkdir build
cd build
cmake -DCMAKE_INSTALL_PREFIX:PATH=$PREFIX ..
cmake --build .
cmake --build . --target install
ln -s $PREFIX/bin/mzn-fzn $PREFIX/bin/minizinc
```

Notice: The `minizinc` driver from MiniZinc v1.6 has been replaced by `mzn-fzn`
in MiniZinc v2.0 but `mzn-gecode` in Gecode v5.0.0 seem to still be looking for
the old driver name. Creating a symbolic link works as a temporary solution.

### Gecode

#### Install prerequisites

```shell
sudo apt-get install autoconf
```

#### Install Gecode

```shell
svn --username anonymous checkout https://svn.gecode.org/svn/gecode/tags/release-5.0.0
```

When asked for password enter an email adress (`a@a.a` for example).

```shell
cd release-5.0.0
./configure --prefix=$PREFIX --enable-examples=no --enable-qt=no --enable-gist=no
make
make install
```

Notice: The Gecode Interactive Search Tool might have unmet QT dependencies
and is disabled here. If you want to try installing it anyway just set
`--enable-qt=yes --enable-gist=yes`.

### References

#### MiniZinc installation

 * [MiniZinc README](https://github.com/MiniZinc/libminizinc/blob/master/README.txt)

 * [MiniZinc INSTALL](https://github.com/MiniZinc/libminizinc/blob/master/INSTALL.txt)

#### Gecode installation

 * Run `./config --help` in base directory to list build options.

 * [Installation Instructions for Gecode/J](http://www.bioinf.uni-freiburg.de/Lehre/Courses/2006_WS/V_ConstraintProgramming/Software/gecodej-installation.pdf)
