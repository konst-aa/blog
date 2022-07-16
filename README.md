# pal-blog-generator

Generates static html with templates.
Will be able to fill in previous articles soon.

## Setup:

Make sure you have clojure and lein installed.

```
brew install clojure
brew install lein
```

or something

## Usage:

```
./render.sh -template template.html -target target-dir
    -i markdown1.cd markdown2.cd ... | -d path/to/markdowns-target
    [-history (not working rn)]
```

### Example use:

`./render.sh -template example/h.html -d example/items/ -target example-rendered`
