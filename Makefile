OBJ=Hirc/*.hs

all: main

main: main.hs $(OBJ) Config.hs
	ghc -O2 -threaded $<

clean:
	rm main

.PHONY: clean
