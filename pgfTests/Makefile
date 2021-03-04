all:

	# gf -make -f haskell --haskell=data QueryEng.gf
	# ghc --make -o query QuerySystem.hs

	# gf -make -f haskell --haskell=data --haskell=gadt  QueryEng.gf
	# ghc --make -o query QuerySystem.hs
	# gf -make -output-format=haskell QueryEng.gf

	# gf -make -f haskell --haskell=gadt  QueryEng.gf
	gf -make -output-format=haskell --haskell=gadt  QueryEng.gf
	ghc --make -XGADTs -o query2 QuerySystem2.hs
