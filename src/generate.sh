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

runhaskell src/Style > style.css

echo "Copying over JavaScript"
cp src/*.js .

echo "Site generated at $(date)"
echo "<!-- site generated at $(date) -->" >> index.html

