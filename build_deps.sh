#!/usr/bin/env bash

DEPS_LOCATION=deps
DESTINATION=cctz

if [ -f "$DEPS_LOCATION/$DESTINATION/libcctz.a" ]; then
    echo "cctz fork already exist. delete $DEPS_LOCATION/$DESTINATION for a fresh checkout."
    exit 0
fi

REPO=https://github.com/google/cctz.git
BRANCH=master
REV=2cbee9fab10e732b62d21c976a44610d8ed94628

function fail_check
{
    "$@"
    local status=$?
    if [ $status -ne 0 ]; then
        echo "error with $1" >&2
        exit 1
    fi
}

function DownloadLib()
{
	echo "repo=$REPO rev=$REV branch=$BRANCH"

	mkdir -p $DEPS_LOCATION
	pushd $DEPS_LOCATION

	if [ ! -d "$DESTINATION" ]; then
	    fail_check git clone -b $BRANCH $REPO $DESTINATION
    fi

	pushd $DESTINATION
	fail_check git checkout $REV
	popd
	popd
}

function BuildLib()
{
	pushd $DEPS_LOCATION
	pushd $DESTINATION

    fail_check make

	popd
	popd
}

DownloadLib
BuildLib
