{
  description = "A very basic flake";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
  };
  outputs = { self, nixpkgs }: {

    packages.x86_64-linux.default =
      let
        pkgs = nixpkgs.legacyPackages.x86_64-linux;
        stdenv = pkgs.stdenv;
        #eggs = (import ./eggs.nix {inherit pkgs stdenv; }); 
        eggs = pkgs.chickenPackages.chickenEggs;
      in
      pkgs.stdenv.mkDerivation {
        name = "blog-gen";
        src = ./.;
        buildInputs = [
          pkgs.chicken
          eggs.args
          eggs.ersatz
          (import ./lowdown.nix { inherit pkgs; })
        ];
      };

  };
}
