all:	CamCanny.hs
	ghc -O3 CamCanny.hs -ddump-simpl-stats -fforce-recomp -rtsopts -threaded -funbox-strict-fields

# Show stats on termination: ./CamCanny +RTS -s