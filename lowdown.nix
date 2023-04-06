{ pkgs }:
let
  eggs = pkgs.chickenPackages.chickenEggs;
in
pkgs.eggDerivation
{
  name = "lowdown-3";
  src = ./lowdown-3;
  buildInputs = with eggs; [
    char-set-literals
    clojurian
    (pkgs.eggDerivation
      {
        name = "comparse-3";
        src = ./comparse-3;
        buildInputs = with eggs; [
          lazy-seq
          trie
          matchable
          srfi-1
          srfi-13
          srfi-14
          srfi-69
        ];
      })
    fancypants
    srfi-1
    srfi-13
    srfi-14
    srfi-69
    sxml-transforms
  ];
}
