clean:
	rm build.log

build:
	buildapp --output cl-ciph --asdf-path . --asdf-tree ~/quicklisp/dists --load-system cl-ciph --entry cl-ciph:main --logfile build.log
