{ mkDerivation, algebraic-graphs, base, bytestring, cryptonite
, hashable, hpack, iproute, mtl, network, random, stdenv
, unordered-containers
}:
mkDerivation {
  pname = "automaton-experiment";
  version = "0.0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    algebraic-graphs base bytestring cryptonite hashable iproute mtl
    network random unordered-containers
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    algebraic-graphs base bytestring cryptonite hashable iproute mtl
    network random unordered-containers
  ];
  testHaskellDepends = [
    algebraic-graphs base bytestring cryptonite hashable iproute mtl
    network random unordered-containers
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/MatrixAI/Relay#readme";
  license = stdenv.lib.licenses.gpl3;
}
