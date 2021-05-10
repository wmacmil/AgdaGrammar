all:

	gf -make -output-format=haskell --haskell=gadt QueryEngRgl.gf 
	ghc --make -XGADTs -o query QuerySystem.hs
