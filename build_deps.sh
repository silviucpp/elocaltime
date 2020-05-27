#!/usr/bin/env bash

ROOT=$(pwd)
DEPS_LOCATION=deps
OS=$(uname -s)
KERNEL=$(echo $(lsb_release -ds 2>/dev/null || cat /etc/*release 2>/dev/null | head -n1 | awk '{print $1;}') | awk '{print $1;}')
CPUS=`getconf _NPROCESSORS_ONLN 2>/dev/null || sysctl -n hw.ncpu`

# https://github.com/google/cctz.git

CCTZ_DESTINATION=cctz
CCTZ_REPO=https://github.com/google/cctz.git
CCTZ_BRANCH=master
CCTZ_REV=24e9dcfb299ed08c5a97a36b35b8b81b363b8a4a
CCTZ_SUCCESS=libcctz.a

fail_check()
{
    "$@"
    local status=$?
    if [ $status -ne 0 ]; then
        echo "error with $1" >&2
        exit 1
    fi
}

CheckoutLib()
{
    if [ -f "$DEPS_LOCATION/$4/$5" ]; then
        echo "$4 fork already exist. delete $DEPS_LOCATION/$4 for a fresh checkout ..."
    else
        #repo rev branch destination

        echo "repo=$1 rev=$2 branch=$3"

        mkdir -p $DEPS_LOCATION
        pushd $DEPS_LOCATION

        if [ ! -d "$4" ]; then
            fail_check git clone -b $3 $1 $4
        fi

        pushd $4
        fail_check git checkout $2
        BuildLibrary $4
        popd
        popd
    fi
}

BuildLibrary()
{
    unset CFLAGS
    unset CXXFLAGS

    case $1 in
        $CCTZ_DESTINATION)
            fail_check make -j $(CPUS) libcctz.a
            ;;
        *)
            ;;
    esac
}

CheckoutLib $CCTZ_REPO $CCTZ_REV $CCTZ_BRANCH $CCTZ_DESTINATION $CCTZ_SUCCESS

