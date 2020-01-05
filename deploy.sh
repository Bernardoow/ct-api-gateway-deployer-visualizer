#!/bin/sh

set -e

cp ./dev/custom.html ./gh-pages/output/index.html

sed -i "s/toChange/$( date '+%s' )/g" ./gh-pages/output/index.html
