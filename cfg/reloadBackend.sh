#!/usr/bin/env bash

set -xe

mkdir -p /var/qwde/qwde/artifacts/latest
command curl --user admin:V8XsYkntgNBUxJETsGjLaug7 --verbose -A "Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:59.0) Gecko/20100101 Firefox/59.0" -H "Origin: http://qwde.no:8876" 'http://qwde.no:8876/restServices/archivaServices/searchService/artifact?g=qwde.frontend&a=client&v=LATEST' --output /var/qwde/qwde/artifacts/latest/all.js

#command curl --user admin:V8XsYkntgNBUxJETsGjLaug7 --verbose -A "Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:59.0) Gecko/20100101 Firefox/59.0" -H "Origin: http://qwde.no:8876" 'http://qwde.no:8876/restServices/archivaServices/searchService/artifact?g=qwde.frontend&a=server&v=LATEST' -L --output /var/qwde/qwde/artifacts/latest/all.js

#command curl --user admin:V8XsYkntgNBUxJETsGjLaug7 --verbose -A "Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:59.0) Gecko/20100101 Firefox/59.0" -H "Origin: http://qwde.no:8876" 'http://qwde.no:8876/restServices/archivaServices/searchService/artifact?g=test&a=this&v=LATEST' -L --output /var/qwde/qwde/artifacts/latest/test.txt
