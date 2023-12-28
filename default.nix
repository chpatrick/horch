{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                    then pkgs.haskellPackages
                    else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  libtorch = pkgs.writeTextFile {
    name = "libtorch-pkgconfig";
    text = ''
      Name: libtorch
      Description: libtorch
      Version: ${pkgs.libtorch-bin.version}
      Cflags: -I${pkgs.libtorch-bin.dev}/include
      Libs: -L${pkgs.libtorch-bin}/lib -ltorch -ltorch_cpu
    '';
    destination = "/lib/pkgconfig/libtorch.pc";
  };

  drv = haskellPackages.callCabal2nix "horch" ./. { inherit libtorch; };

in

  if pkgs.lib.inNixShell then drv.env.overrideAttrs (old: { buildInputs = old.buildInputs ++ [ pkgs.haskell-language-server ];}) else drv