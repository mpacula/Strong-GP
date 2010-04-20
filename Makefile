classifier:
	ghc --make ColorClassifier.hs -O3 -threaded -o classifier

clean:
	rm -f classifier *.o *.hi ./GP/*.o ./GP/*.hi ./GP/Evolver/*.hi ./GP/Evolver/*.o ./GP/Generator/*.hi ./GP/Generator/*.o
