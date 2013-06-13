#!/bin/bash

mkdir -p src/highlighted
for hs in `find src -name '*.hs'`
do
  file=$(basename $hs)
  echo -n "Syntax highlighting: ${file} "
  illuminate ${hs} \
    --syntax=haskell \
    --fragment \
    --to=xhtmlcss > \
    src/highlighted/${file}.html
  echo done
done

for hs in `find src -name '*.clay.hs'`
do
  file=$(basename $hs)
  echo -n "Syntax highlighting output of: ${file} "
  runhaskell -isrc $hs | grep -v "^\s*$" | grep -v "Generated with Clay" | illuminate --syntax=css --fragment --to=xhtmlcss > \
    src/highlighted/${file}.output.html
  echo done
done

htmls="index.html code.html"

for h in $htmls
do
  echo "Downloading $h files from localhost"
  curl -s http://devoid.local/clay-site/src/$h > $h
  echo "<!-- site generated at $(date) -->" >> $h
done

echo "Generating pretty printed stylesheet"
runhaskell -isrc/style Main pretty > style-pretty.css

echo "Generating compacted stylesheet"
runhaskell -isrc/style Main compact > style-compact.css

echo "Downloading newest jQuery"
curl -s http://code.jquery.com/jquery-1.8.3.min.js > src/src/jquery.js ||
  rm src/src/jquery.js

echo "Copying over JavaScript"
cp -v src/src/*.js .

echo
echo "DONE!"

