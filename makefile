default:
	runhaskell main.hs

build:
	ghc main.hs -o lc
	rm main.hi
	rm main.o
