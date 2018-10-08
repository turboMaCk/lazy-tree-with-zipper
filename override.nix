{pkgs ? import <nixpkgs> {
   inherit system;
}, system ? builtins.currentSystem }:

let
    nodePackages = import ./default.nix {
                 inherit pkgs system;
    };
    elmInterface = import ./elm-interface-to-json.nix {};
in
nodePackages // {
    elm-test = nodePackages.elm-test.override (oldAttrs: {
      buildInputs =
        oldAttrs.buildInputs ++ [ (pkgs.haskellPackages.callPackage elmInterface) ];
    });
}
