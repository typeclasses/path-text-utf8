{ pkgs }:

let
  inherit (pkgs.lib) fold composeExtensions concatMap attrValues;

  hls = pkgs.haskell-language-server.override {
    supportedGhcVersions = [ "96" ];
  };

  combineOverrides = old:
    fold composeExtensions (old.overrides or (_: _: { }));

  testConfigurations =
    let
      makeTestConfiguration = { ghcVersion, overrides ? new: old: { } }:
        let
          inherit (pkgs.haskell.lib) dontCheck packageSourceOverrides;
        in
        (pkgs.haskell.packages.${ghcVersion}.override (old: {
          overrides =
            combineOverrides old [
              (packageSourceOverrides { path-text-utf8 = ../path-text-utf8; })
              overrides
            ];
        })).path-text-utf8;
    in
    rec {
      ghc-9-6 = makeTestConfiguration {
        ghcVersion = "ghc96";
        overrides = new: old: {
          file-io = new.callPackage ./haskell/file-io.nix { };
        };
      };
      all = pkgs.symlinkJoin {
        name = "path-text-utf8-tests";
        paths = [ ghc-9-6 ];
      };
    };

in
{

  packages = { inherit testConfigurations; };

  devShells.default = pkgs.mkShell {
    inputsFrom = [ testConfigurations.ghc-9-6.env ];
    buildInputs = [ hls pkgs.cabal-install ];
  };

}
