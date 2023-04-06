generator: generator.scm
	csc generator.scm -o generator
blog: generator
	rm -rf example-rendered
	./generator -f example/items -t example/h.html -o example-rendered

