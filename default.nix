{ mkDerivation, base, bytestring, hakyll, hakyll-sass, hjsmin
, stdenv
}:
mkDerivation {
  pname = "personal-website";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring hakyll hakyll-sass hjsmin
  ];
  description = "Personal Website";
  license = stdenv.lib.licenses.mit;
}
