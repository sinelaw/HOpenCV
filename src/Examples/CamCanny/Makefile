all:	CamCanny.hs Rate.hs
	ghc -O3 CamCanny.hs -ddump-simpl-stats -fforce-recomp -rtsopts -threaded -funbox-strict-fields

fun:	VideoFunhouse.hs Rate.hs
	ghc -O3 VideoFunhouse.hs -ddump-simpl-stats -fforce-recomp -rtsopts -threaded -funbox-strict-fields -fspec-constr-count=15


# Show stats on termination: ./CamCanny +RTS -s -A8M -N