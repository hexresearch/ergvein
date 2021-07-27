{ mkDerivation, base, criterion, deepseq, exceptions, ghc-heap
, hashable, hashtables, primitive, transformers
, unordered-containers, lib
}:
mkDerivation {
    pname = "heapsize";
    version = "0.3.0.1";
    sha256 = "0v38czcdnrlpcszbz7janb2qw4bqldhmql0riqq6a9bylv3zfs0y";
    libraryHaskellDepends = [
        base deepseq exceptions ghc-heap hashable hashtables primitive
        transformers unordered-containers
    ];
    benchmarkHaskellDepends = [ base criterion deepseq primitive ];
    description = "Determine the size of runtime data structures";
    license = lib.licenses.bsd3;
}