# pal-blog-generator

Reinventing the wheel (poorly) to generate static html with templates. This repo is also the home of my [Awesome Blog](https://ka.dreadmaw.industries/).  

This script can:
* parse post descriptions from markdown, then add them to posts.scm `blog-gen spoiler posts.scm post.md`, (writes the post without the spoiler to stdout)
* generate an index of blog posts in markdown, with a list of categories. `blog-gen index posts.scm` (requires spoilers!)
* generate markdown for related posts (writes to stdout)
`blog-gen related posts.scm post.md` (requires spoilers!)

NOTE: [i didnt know that hyde existed, I might use it, but it wasn't invented here](http://wiki.call-cc.org/eggref/5/hyde).  

Not my best code

`make blog`
Pandoc is awesome baybee!
