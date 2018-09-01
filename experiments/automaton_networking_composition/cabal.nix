{ mkDerivation, base, bytestring, cryptonite, hpack, iproute
, network, stdenv
}:
mkDerivation {
  pname = "automaton-experiment";
  version = "0.0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring cryptonite iproute network
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base bytestring cryptonite iproute network
  ];
  testHaskellDepends = [
    base bytestring cryptonite iproute network
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/MatrixAI/Relay#readme";
  license = stdenv.lib.licenses.asl20;
}
