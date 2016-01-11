{ mkDerivation, base, containers, deepseq, dependent-map
, dependent-sum, dlist, exception-transformers, haskell-src-exts
, haskell-src-meta, MemoTrie, mtl, primitive, ref-tf, semigroups
, split, stdenv, syb, template-haskell, these, transformers
, transformers-compat
}:
mkDerivation {
  pname = "reflex";
  version = "0.4.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers deepseq dependent-map dependent-sum dlist
    exception-transformers haskell-src-exts haskell-src-meta mtl
    primitive ref-tf semigroups syb template-haskell these transformers
    transformers-compat
  ];
  testHaskellDepends = [
    base containers dependent-map dependent-sum MemoTrie mtl ref-tf
    split
  ];
  homepage = "https://github.com/ryantrinkle/reflex";
  description = "Higher-order Functional Reactive Programming";
  license = stdenv.lib.licenses.bsd3;
}
