#!/bin/bash

mkdir -p src/highlighted
for hs in `find src -name '*.hs'`
do
  file=$(basename $hs)
  echo -n "Syntax highlighting: ${file} "
  illuminate ${hs} \
    --syntax=haskell \
    --fragment \
    --to=xhtmlcss > src/highlighted/${file}.html
  echo done
done

echo "Downloading HTML from localhost"
curl -s http://devoid.local/clay-site/src/index.html > index.html

echo "Generating pretty printed stylesheet"
runhaskell src/Style pretty   > style-pretty.css

echo "Generating compacted stylesheet"
runhaskell src/Style compact > style-compact.css

echo "Downloading newest jQuery"
curl -s http://code.jquery.com/jquery-1.8.3.min.js > src/jquery.js ||
  rm src/jquery.js

echo "Copying over JavaScript"
cp -v src/*.js .

echo "Site generated at $(date)"
echo "<!-- site generated at $(date) -->" >> index.html

