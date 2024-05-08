{
  description = "A blog generator written in Chicken Scheme";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
  };
  outputs = { self, nixpkgs }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      eggs = pkgs.chickenPackages.chickenEggs;
    in
    {
      packages.x86_64-linux.default =
        pkgs.stdenv.mkDerivation {
          name = "blog-gen";
          src = ./.;
          buildPhase = ''
            make generator
          '';
          installPhase = ''
            mkdir -p $out/bin
            mv blog-gen $out/bin
            for f in $out/bin/*
            do
              wrapProgram $f \
                --prefix CHICKEN_REPOSITORY_PATH : $CHICKEN_REPOSITORY_PATH \
                --prefix CHICKEN_INCLUDE_PATH : $CHICKEN_INCLUDE_PATH \
                --prefix PATH : $CHICKEN_REPOSITORY_PATH
            done
          '';
          buildInputs = [
            pkgs.gnumake
            pkgs.makeWrapper
            pkgs.chicken
            eggs.srfi-1
          ];
        };
      devShell.x86_64-linux = pkgs.mkShell {
        name = "blog-gen-shell";
        packages = [
          pkgs.pandoc
        ];
        inputsFrom = [
          self.packages.x86_64-linux.default
        ];
      };
    };
}
