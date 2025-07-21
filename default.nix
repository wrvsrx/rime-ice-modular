{
  stdenvNoCC,
  haskellPackages,
}:
stdenvNoCC.mkDerivation {
  name = "rime-ice-modular";
  src = ./.;
  env.LANG = "C.UTF-8";
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
