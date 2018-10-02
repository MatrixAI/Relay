{ mkDerivation, base, bytestring, cryptonite, hashable, hpack
, iproute, matrix, mtl, network, stdenv, unordered-containers
}:
mkDerivation {
  pname = "automaton-experiment";
  version = "0.0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring cryptonite hashable iproute matrix mtl network
    unordered-containers
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base bytestring cryptonite hashable iproute matrix mtl network
    unordered-containers
  ];
  testHaskellDepends = [
    base bytestring cryptonite hashable iproute matrix mtl network
    unordered-containers
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/MatrixAI/Relay#readme";
  license = stdenv.lib.licenses.gpl3;
}
