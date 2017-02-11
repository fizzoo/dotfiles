with import <nixpkgs> {};

let p = python35Packages;
in
{
  env = stdenv.mkDerivation {
    name = "pysci";
    buildInputs = [ python35 p.numba p.numpy p.matplotlib p.scipy gnuplot p.tkinter qt5.full p.pyqt5 ];
    src = ./.;
  };
}
