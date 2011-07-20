all:	PerfTest.hs
	ghc -O2 PerfTest.hs -ddump-simpl-stats -fforce-recomp -rtsopts -threaded -fspec-constr-count=15

# Suggested RTS options: ./PerfTest +RTS -A4M
