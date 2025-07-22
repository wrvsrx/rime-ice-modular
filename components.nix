{ rime-ice-modular, stdenv }:
let
  components = builtins.fromJSON (builtins.readFile ./components.json);
  components' = builtins.listToAttrs (
    map (x: {
      name = "rime-ice-" + x;
      value = stdenv.mkDerivation {
        name = "rime-ice-" + x;
        src = rime-ice-modular;
        passthru.rimeDependencies = map (y: components'.${y}) components.${x}.dependencies;
        buildPhase = "true";
        installPhase = ''
          ${builtins.concatStringsSep "\n" (
            map (y: ''
              install -D --mode=644 ${rime-ice-modular}/${y} $out/${y}
            '') components.${x}.outputs
          )}
        '';
      };
    }) (builtins.attrNames components)
  );
in
components'
