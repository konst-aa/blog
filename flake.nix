{
  description = "A blog generator written in Chicken Scheme";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
  };
  outputs = { self, nixpkgs }: {

    packages.x86_64-linux.default =
      let
        pkgs = nixpkgs.legacyPackages.x86_64-linux;
        stdenv = pkgs.stdenv;
        eggs = pkgs.chickenPackages.chickenEggs;
      in
      pkgs.stdenv.mkDerivation {
        name = "blog-gen";
        src = ./.;
        buildInputs = [
          pkgs.chicken
          pkgs.pandoc
          eggs.args
          eggs.ersatz
          eggs.intarweb
          eggs.openssl
          eggs.regex
          eggs.spiffy
          eggs.srfi-1
          eggs.srfi-18
          eggs.srfi-19
          eggs.srfi-69
          eggs.uri-common
          eggs.json
        ];
      };

  };
}
