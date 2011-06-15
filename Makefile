GHC = ghc
GHC_FLAGS = -fforce-recomp -itest -isrc
INCLUDE = --include=Network.SiteCheck.Data --include=Network.SiteCheck.URL --include=Network.SiteCheck --include=Network.SiteCheck.Util --include=Network.SiteCheck.Filter

sitecheck:
	$(GHC) --make -o sitecheck -isrc src/Main.hs

tests:
	$(GHC) $(GHC_FLAGS) -fhpc --make -main-is TestSuite test/TestSuite.hs
	rm -f TestSuite.tix
	./test/TestSuite

hpc-text:
	hpc report TestSuite $(INCLUDE)

hpc-markup:
	hpc markup TestSuite $(INCLUDE)
	open hpc_index.html

cov: tests hpc-markup

clean:
	rm -f sitecheck 
	rm -f test/TestSuite
	rm -f `find . -type f -name *.hi`
	rm -f `find . -type f -name *.o`
	rm -f *.html
	rm -f TestSuite.tix
