# pal-blog-generator

Reinventing the wheel (poorly) to generate static html with templates. This repo is also the home of my [Awesome Blog](https://ka.dreadmaw.industries/).  

My scheme script can:
* generate spoilers `./blog-gen spoiler posts.scm post.md`
* generate an index of blog posts in markdown, (grouped by category as well)
`./blog-gen index posts.scm` 
(requires spoilers!)
* generate markdown for related posts `blog-gen related posts.scm post.md` 
(requires spoilers!)

NOTE: [i didnt know that hyde existed, I might use it, but it wasn't invented here](http://wiki.call-cc.org/eggref/5/hyde).  

Not my best code

`make blog`
Pandoc is awesome baybee!
