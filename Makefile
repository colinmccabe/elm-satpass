DEST=dest

all: build

htdocs: build
	cp $(DEST)/* ~/htdocs/sat/

build:
	mkdir -p $(DEST)
	cp src/js/* $(DEST)
	cp src/index.html $(DEST)
	elm make --output $(DEST)/elm.js src/elm/Main.elm

clean:
	rm -r $(DEST)
