{pkgs ? import <nixpkgs> {} }:

(pkgs.buildFHSUserEnv {
  name = "anaenv";
  targetPkgs = pkgs: (with pkgs; [
    vim ncurses emacs git

    udev mesa_glu
  ]) ++  (with pkgs. xorg; [
      libX11 libXcursor libXrandr libXrender libXau libXext libICE libSM
  ]);
}).env
