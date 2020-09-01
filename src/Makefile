build:
	ghc Main.hs -o freak
	ghc Tests.hs -o tests

link:
	ln -fs $(shell pwd)/freak /usr/local/bin/freak

link-clean: clean
	rm -f Eval.hs
	rm -f CPS.hs

link-appel: link-clean
	ln -s EvalCPS.hs Eval.hs
	ln -s CPSAppel.hs CPS.hs

link-lists: link-clean
	ln -s EvalTarget.hs Eval.hs
	ln -s CPSLists.hs CPS.hs

link-lambda: link-clean
	ln -s EvalTarget.hs Eval.hs
	ln -s CPSLambda.hs CPS.hs

clean:
	rm -f Tests
	rm -f tests
	find . -name "*.hi" -type f -delete
	find . -name "*.o" -type f -delete

lint:
	hlint .

test:
	runhaskell Tests.hs

validate_programs: build
	find programs -name "*.fk" | xargs -I % sh -c "echo %; freak -v %"

check: build lint validate_programs test

install:
	cabal install parsec
	cabal install hlint
	cabal install HUnit
	cabal install QuickCheck
	cabal install hspec
