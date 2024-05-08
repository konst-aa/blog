#!/usr/bin/env bash

rm -r docs
mkdir docs

cp posts.scm docs
cp robots.txt docs
cp CNAME docs
cp example/h.html docs
cp example/stylesheet.css docs
cp example/items/*.md docs

cp blog-gen docs

cd docs
for post in *.md; do
    echo "Spoiling $post"
    ./blog-gen spoiler posts.scm $post
done
for post in *.md; do
    echo "Generating posts related to $post"
    ./blog-gen related posts.scm $post >> $post
done

echo "Generating index"
./blog-gen index posts.scm
for page in *.md; do
    cat $page | pandoc -f gfm -t html > t.html
    cat h.html | sed '/<!-- MARKDOWN-GOES-HERE -->/{
        r t.html
    }' > "$(basename $page .md).html"
done

rm t.html
rm *.md
rm h.html
rm blog-gen

# enjoy the WICKED post metadata
