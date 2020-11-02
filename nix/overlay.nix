final: previous:
with final.haskell.lib;

{
  habitsciplineCasts =
    let
      mkCastDerivation = import (
        builtins.fetchGit {
          url = "https://github.com/NorfairKing/autorecorder";
          rev = "da5bf9d61108a4a89addc8203b1579a364ce8c01";
          ref = "master";
        } + "/nix/cast.nix"
      ) { pkgs = final // final.habitsciplinePackages; };
    in
      {
        habitscipline-basics-cast = mkCastDerivation {
          name = "habitscipline-basics-cast";
          src = ../casts/basics.yaml;
        };
      };
  habitsciplinePackages =
    let
      habitsciplinePkg =
        name:
          doBenchmark (
            addBuildDepend (
              failOnAllWarnings (
                disableLibraryProfiling (
                  final.haskellPackages.callCabal2nix name (final.gitignoreSource (../. + "/${name}")) {}
                )
              )
            ) (final.haskellPackages.autoexporter)
          );
      habitsciplinePkgWithComp =
        exeName: name:
          generateOptparseApplicativeCompletion exeName (habitsciplinePkg name);
      habitsciplinePkgWithOwnComp = name: habitsciplinePkgWithComp name name;

    in
      {
        "habitscipline-api" = habitsciplinePkg "habitscipline-api";
        "habitscipline-api-gen" = habitsciplinePkg "habitscipline-api-gen";
        "habitscipline-api-server" = habitsciplinePkgWithOwnComp "habitscipline-api-server";
        "habitscipline-api-server-data" = habitsciplinePkg "habitscipline-api-server-data";
        "habitscipline-api-server-data-gen" = habitsciplinePkg "habitscipline-api-server-data-gen";
        "habitscipline-api-server-gen" = habitsciplinePkg "habitscipline-api-server-gen";
        "habitscipline-cli" = habitsciplinePkgWithComp "habitscipline" "habitscipline-cli";
        "habitscipline-client" = habitsciplinePkg "habitscipline-client";
        "habitscipline-client-data" = habitsciplinePkg "habitscipline-client-data";
        "habitscipline-data" = habitsciplinePkg "habitscipline-data";
        "habitscipline-data-gen" = habitsciplinePkg "habitscipline-data-gen";
        "habitscipline-tui" = habitsciplinePkgWithComp "habit" "habitscipline-tui";
      };
  haskellPackages =
    previous.haskellPackages.override (
      old:
        {
          overrides =
            final.lib.composeExtensions (
              old.overrides or (
                _:
                _:
                  {}
              )
            ) (
              self: super:
                let
                  # envparse
                  envparseRepo =
                    final.fetchFromGitHub {
                      owner = "supki";
                      repo = "envparse";
                      rev = "de5944fb09e9d941fafa35c0f05446af348e7b4d";
                      sha256 =
                        "sha256:0piljyzplj3bjylnxqfl4zpc3vc88i9fjhsj06bk7xj48dv3jg3b";
                    };
                  envparsePkg =
                    dontCheck (
                      self.callCabal2nix "envparse" envparseRepo {}
                    );
                  cursorBrickRepo =
                    final.fetchFromGitHub {
                      owner = "NorfairKing";
                      repo = "cursor-brick";
                      rev = "a7b47b03c8c5525234aaccc0c372e49a80134b9d";
                      sha256 = "sha256:1wk2sixf1ld48j6a14zgfadg41si6rl8gwmwdlkn0cqjiw9n7f4p";
                    };
                  cursorBrickPkg = self.callCabal2nix "cursor-brick" (cursorBrickRepo + "/cursor-brick") {};
                  typedUuidRepo =
                    final.fetchFromGitHub {
                      owner = "NorfairKing";
                      repo = "typed-uuid";
                      rev = "02bcaa59392cee08fa1b26847d099e69d75fcf15";
                      sha256 = "sha256:169vvch3wy8gcxnv2ifqdly3vjs8jh5xyxgg8a2znzf7csfb4m14";
                    };
                  typedUuidPkg = name: self.callCabal2nix name (typedUuidRepo + "/${name}") {};
                  typedUuidPkgs = final.lib.genAttrs [
                    "typed-uuid"
                    "genvalidity-typed-uuid"
                  ] typedUuidPkg;

                in
                  final.habitsciplinePackages // typedUuidPkgs // {
                    envparse = envparsePkg;
                    cursor-brick = cursorBrickPkg;
                  }
            );
        }
    );
}
