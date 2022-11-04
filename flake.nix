{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        haskellPackages = pkgs.haskellPackages;
        haskellDeps = drv: builtins.concatLists (builtins.attrValues drv.getCabalDeps);
        ldap = haskellPackages.callCabal2nix "Technomancy" ./. { };
      in
      {
        packages.ldap = ldap;
        packages.default = ldap;

        devShells.default = pkgs.mkShell {
          nativeBuildInputs = [
            (haskellPackages.ghcWithPackages (ps: haskellDeps ldap))
            haskellPackages.cabal-install
            haskellPackages.haskell-language-server
            haskellPackages.hpack
          ];
        };
      });
}
