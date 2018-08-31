{ mkDerivation, base, bytestring, cryptonite, hexstring, hpack
, network, stdenv
}:
mkDerivation {
  pname = "automaton-experiment";
  version = "0.0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring cryptonite hexstring network
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base bytestring cryptonite hexstring network
  ];
  testHaskellDepends = [
    base bytestring cryptonite hexstring network
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/MatrixAI/Relay#readme";
  license = stdenv.lib.licenses.asl20;
}
