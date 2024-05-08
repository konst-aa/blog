# pal-blog-generator

A scheme script to manage post categories, related posts, etc.  
The generator itself uses the above + sed and pandoc to write static html. This repo is also the home of my [Awesome Blog](https://ka.dreadmaw.industries/).  

This script can:
* parse post descriptions from markdown, then add them to posts.scm. `blog-gen spoiler posts.scm post.md`, (writes the post without the spoiler to stdout)
* generate an index of blog posts in markdown with a list of categories. `blog-gen index posts.scm` (requires spoilers!)
* generate markdown for related posts (writes to stdout)
`blog-gen related posts.scm post.md` (requires spoilers!)


**Invented here!**

To run: `nix develop`, then `make blog`
