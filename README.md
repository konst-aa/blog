# blog-generator
This repo is the home of my [Awesome Blog](https://ka.dreadmaw.industries/)  

As normal-person tools such as Jekyll are Not Invented Here, I wrote
a scheme script to manage post categories, descriptions, the landing page, etc. Note that the script doesn't template.    

The generator (`generate.sh`) uses the above + `sed` and `pandoc` to write static HTML.  

This script can:
* parse post descriptions from markdown, then add them to posts.scm. `blog-gen spoiler posts.scm post.md`, (It also writes the post without the spoiler to stdout, so one can template without the spoiler tag)
* generate a homepage of blog posts, along with pages for each category. `blog-gen index posts.scm` (requires spoilers!)
* generate markdown for related/"recommended" posts (writes to stdout). One can then append that to the actual post.
`blog-gen related posts.scm post.md` (requires spoilers!)


To build: `nix develop`, then `make blog`
