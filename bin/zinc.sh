#!/bin/bash

set -e

ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." && pwd )"

# zinc install
ZINC_TGZ=$(ls -1 ${ROOT}/target/universal/*.tgz | tail -n 1)
TMP="/tmp/zinc-tgz-`date +%s`"
mkdir -p $TMP
(cd $TMP && tar xzf $ZINC_TGZ)

# execute
$TMP/*/bin/zinc $*
