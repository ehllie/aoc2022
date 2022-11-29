{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    let
      compiler_version = "ghc92";
      get_compiler = pkgs: pkgs.haskell.packages.${compiler_version};
    in
    {
      overlays.default = _: prev: {
        aoc2022 =
          let
            compiler = get_compiler prev;
            inherit (prev.lib) pipe;
            inherit (compiler) developPackage;
            inherit (prev.haskell.lib.compose) overrideCabal;
            overrides = [
              (overrideCabal (old: {
                preBuild = ''
                  export INPUTS_DIR=$out/share/aoc2022/inputs
                  mkdir -p $INPUTS_DIR
                  cp $src/inputs/* $INPUTS_DIR/
                '';
              }))
            ];
            pkg = (developPackage { root = ./.; });
          in
          pipe pkg overrides;
      };
    } //
    (flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlays.default ];
        };
        inherit (pkgs) aoc2022;
        compiler = get_compiler pkgs;
      in
      {
        packages = {
          inherit aoc2022;
          default = aoc2022;
        };
        devShells.default = aoc2022.env.overrideAttrs (old: {
          buildInputs = old.buildInputs ++ [ compiler.haskell-language-server ];
        });
      }));
}
