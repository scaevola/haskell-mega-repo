set -ex

STEP=$1
BUILD=$2

STACK="stack +RTS -N2 -RTS --no-terminal --system-ghc --skip-ghc-check"

timed () {
    JOB_CURR_TIME=$(date +%s)
    JOB_DURR=$((JOB_START_TIME + 4800 - JOB_CURR_TIME))
    echo time to run $JOB_DURR
    if [ $JOB_DURR -lt 300 ]; then
        echo "Less than 5 minutes to go, aborting"
        exit 1
    else
        timeout $JOB_DURR $*
    fi
}

case $STEP in
prepare)
    case $BUILD in
    stack)
        # Download and unpack the stack executable
        mkdir -p ~/.local/bin
        
        # if [ ! -e ~/.local/bin/stack ]; then
        #    travis_retry curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack';
        #  fi
        if [ ! -e ~/.local/bin/stack ]; then
            curl -L http://oleg.fi/stack.xz | xz --decompress --stdout - > ~/.local/bin/stack; chmod a+x ~/.local/bin/stack
        fi

        stack +RTS -N1 -RTS --version
        ghc --version
        ;;

    esac
    ;;

install)

    # common data
    cp futurice-constants/constants.sample.json futurice-constants/constants.json
    cp futurice-tribes/tribes.sample.json futurice-tribes/tribes.json

    case $BUILD in
    stack)
        timed $STACK build --test --only-snapshot -j2 --ghc-options=-j2 futurice-prelude
        timed $STACK build --test --only-snapshot -j2 --ghc-options=-j2 servant-Chart
        timed $STACK build --test --only-snapshot -j2 --ghc-options=-j2
        ;;

    esac
    ;;

build)

    case $BUILD in
    stack)
        if [ "$PEDANTIC" = "YES" ]; then
            STACKOPTS=--pedantic
        fi

        timed $STACK build --test $STACKOPTS -j1 --ghc-options=-j2
        ;;

    esac
    ;;
esac
