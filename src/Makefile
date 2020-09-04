build:
	ghc Main.hs -o freak
	ghc Tests.hs -o tests

link:
	ln -fs $(shell pwd)/freak /usr/local/bin/freak

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
