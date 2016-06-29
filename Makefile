all: elm-make
	cp elm.js index.html sat-predict.js ~/htdocs/sat/

elm-make:
	elm make --output elm.js src/Main.elm

clean:
	rm elm.js
