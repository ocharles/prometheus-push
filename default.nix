{ mkDerivation, base, bytestring, hspec, http-client, http-types
, prometheus-client, stdenv, tasty, tasty-discover, tasty-hspec
}:
mkDerivation {
  pname = "prometheus-push";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [
    base bytestring hspec http-client http-types prometheus-client
    tasty tasty-discover tasty-hspec
  ];
  homepage = "https://github.com/ocharles/prometheus-push";
  description = "Push prometheus-client metrics to a push gateway";
  license = stdenv.lib.licenses.bsd3;
}
