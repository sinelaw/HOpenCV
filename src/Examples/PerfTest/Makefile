all:	PerfTest.hs
	ghc -O3 PerfTest.hs -ddump-simpl-stats -fforce-recomp -rtsopts -threaded -fspec-constr-count=15

# Suggested RTS options: ./PerfTest +RTS -s -A8M -N