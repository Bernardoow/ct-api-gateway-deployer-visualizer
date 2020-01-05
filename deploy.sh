#!/bin/sh

set -e

cp ./dev/custom.html ./gh-pages/index.html

sed -i "s/toChange/$( date '+%s' )/g" ./gh-pages/index.html
