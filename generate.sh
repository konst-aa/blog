#!/usr/bin/env bash

rm -r docs
mkdir docs

cp robots.txt docs
cp CNAME docs
cp -r example/media docs

cp posts.scm docs
cp example/h.html docs
cp example/stylesheet.css docs
cp example/items/*.md docs
cp blog-gen docs

cd docs

for post in *.md; do
    echo "Spoiling $post"
    ./blog-gen spoiler posts.scm $post > t.md
    mv t.md $post
done
for post in *.md; do
    echo "Appending posts related to $post"
    ./blog-gen related posts.scm $post >> $post
done

echo "Generating index"
./blog-gen index posts.scm

for page in *.md; do
    output="$(basename $page .md).html"
    echo "Generating $output"
    cat $page | pandoc -f gfm -t html > t.html
    # https://stackoverflow.com/q/6790631
    cat h.html | sed '/<!-- MARKDOWN-GOES-HERE -->/{
        r t.html
    }' > $output
done

rm t.html
rm *.md
rm h.html
rm blog-gen

# enjoy the WICKED post metadata
