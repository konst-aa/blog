generator: generator.scm
	csc generator.scm -o blog-gen
blog: generator
	./generate.sh

