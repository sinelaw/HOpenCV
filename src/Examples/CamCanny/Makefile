all:	CamCanny.hs Rate.hs
	ghc -O3 CamCanny.hs -ddump-simpl-stats -fforce-recomp -rtsopts -threaded -funbox-strict-fields

fun:	VideoFunhouse.hs Rate.hs
	ghc -O3 VideoFunhouse.hs -ddump-simpl-stats -fforce-recomp -rtsopts -threaded -funbox-strict-fields


# Show stats on termination: ./CamCanny +RTS -s