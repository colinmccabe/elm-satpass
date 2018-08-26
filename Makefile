all: dest/elm.js dest/index.html dest/sat-predict.js

dest/elm.js: dest $(wildcard src/elm/*)
	elm make --output $@ src/elm/Main.elm

dest/index.html: dest src/index.html
	cp src/index.html dest

dest/sat-predict.js: dest src/js/sat-predict.js
	cp src/js/sat-predict.js dest/sat-predict.js

dest:
	mkdir dest

clean:
	rm -r dest
