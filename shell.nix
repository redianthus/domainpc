{ pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz";
  }) {}
}:

pkgs.mkShell {
  name = "domainpc-dev-shell";
  dontDetectOcamlConflicts = true;
  nativeBuildInputs = with pkgs.ocamlPackages; [
    dune_3
    findlib
    ocaml
    ocamlformat
    ocp-browser
    odoc
  ];
  buildInputs = with pkgs.ocamlPackages; [
    processor
  ];
}
