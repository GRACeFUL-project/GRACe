#!/bin/sh

mkdir bins

# Pick up executables, copy to bins
find "../$(stack path --dist-dir)/build" -type f -perm -u=x,g=x,o=x | \
	xargs -I % cp % bins

# Get dependencies
git clone https://github.com/MiniZinc/libminizinc.git
svn --username anonymous checkout https://svn.gecode.org/svn/gecode/tags/release-5.0.0

# Build image
docker build -t eugraceful/grace-examples .

# Clean up
echo Removing
rm -rf bins libminizinc release-5.0.0
