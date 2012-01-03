all:	VideoFunhouse.hs Rate.hs
	ghc -O2 VideoFunhouse.hs -fforce-recomp -rtsopts -threaded -fspec-constr-count=18 -with-rtsopts="-A4M"

clean:
	rm *.hi *.o VideoFunhouse