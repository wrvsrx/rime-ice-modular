{
  stdenvNoCC,
  haskellPackages,
}:
stdenvNoCC.mkDerivation {
  name = "rime-ice-modular";
  src = ./.;
  nativeBuildInputs = [
    (haskellPackages.ghcWithPackages (
      ps: with ps; [
        yaml
        shake
        raw-strings-qq
        extra
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
