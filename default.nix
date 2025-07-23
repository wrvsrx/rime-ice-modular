{
  stdenvNoCC,
  haskellPackages,
  source,
}:
stdenvNoCC.mkDerivation {
  inherit (source) pname src;
  version = "0-unstable-" + source.date;
  env.LANG = "C.UTF-8";
  nativeBuildInputs = [
    (haskellPackages.ghcWithPackages (
      ps: with ps; [
        yaml
        shake
        raw-strings-qq
        extra
        utf8-string
      ]
    ))
  ];
  buildPhase = ''
    shake
  '';
  installPhase = ''
    mkdir -p $out
    cp -r build/* $out/
  '';
}
