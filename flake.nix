{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, utils, ... }:
    utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ]
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          compiler = "ghc94";
          packageDependencies = (ps: [
            ps.bytestring
            ps.containers
            ps.directory
            ps.lens
            ps.megaparsec
            ps.mtl
            ps.vector
            ps.wreq
          ]);
          devDependencies = with pkgs.haskell.packages.${compiler}; [
            cabal-install
            cabal-fmt
            haskell-language-server
            hlint
          ];
          testDependencies = (ps: [
            ps.hspec
            ps.hspec-discover
          ]);
          haskell = pkgs.haskell.packages.${compiler}.ghcWithPackages
            (ps: packageDependencies ps ++ testDependencies ps);
        in
        {
          devShells.default = pkgs.mkShell
            {
              packages = [ haskell ] ++ devDependencies;
            };

          packages = {
            core = pkgs.haskell.packages.${compiler}.callCabal2nix "aoclib-core" ./. { };
            testing = pkgs.haskell.packages.${compiler}.callCabal2nix "aoclib-testing" ./. { };
          };
        }
      );
}
