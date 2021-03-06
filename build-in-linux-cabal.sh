#!/bin/sh

# This script is used to build binaries inside the docker
# See mega-repo-tool -h

set -ex

# This matters
unset POSIXLY_CORRECT

# Root directory. In docker it's /app/src
if [ "x$DOCKER" = "xYES" ]; then
    ROOTDIR=/app/src
else
    ROOTDIR=$(pwd)
fi

cd $ROOTDIR

# Check that we have somewhat clean working dir
if [ ! -z "$(git status --porcelain)" ]; then
    echo "DIRTY WORKINGDIR"
    exit 1
fi

# GHC version
GHCVER=${GHCVER-8.0.2}
export PATH=/opt/ghc/$GHCVER/bin:$PATH
HC=ghc-$GHCVER

# Use different BUILDDIR
BUILDDIR=dist-newstyle-prod

# Concurrency
CONCURRENCY=${CONCURRENCY-1}

# Update cabal
cabal update

# Perform build
cabal new-build -j$CONCURRENCY -w $HC --builddir=$BUILDDIR all:exes

# write current git hash, so we know where we are
GITHASH=$(git log --pretty=format:'%h' -n 1)

# Copy binaries to ./build/exe/exe
# We put binaries in separate directories to speed-up docker image creation
mkdir -p $ROOTDIR/build
for fullexe in $(cabal-plan --builddir=$BUILDDIR list-bin|grep ':exe:'|awk '{print $2}'); do
    if [ $(echo $fullexe | sed "s:^$ROOTDIR/.*$:matches:") = "matches" ]; then
        exe=$(basename $fullexe)
        mkdir -p  $ROOTDIR/build/$exe
        cp $fullexe $ROOTDIR/build/$exe/$exe
        strip $ROOTDIR/build/$exe/$exe
        echo $GITHASH > $ROOTDIR/build/$exe/git-hash.txt
    else
        echo "Skipping $fullexe"
    fi
done

echo $GITHASH > $ROOTDIR/build/git-hash.txt
