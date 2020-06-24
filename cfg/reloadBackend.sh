#!/usr/bin/env bash

command curl --user admin:<secretpassword> -A "Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:59.0) Gecko/20100101 Firefox/59.0" -H "Origin: http://qwde.no:8876" 'http://qwde.no:8876/restServices/archivaServices/searchService/artifact?g=qwde&a=webapi&v=LATEST&c=all' -L --output qwde.jar

./runWeb.sh
