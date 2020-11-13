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
        "habitscipline-cli" = habitsciplinePkgWithOwnComp "habitscipline-cli";
        "habitscipline-client" = habitsciplinePkg "habitscipline-client";
        "habitscipline-client-data" = habitsciplinePkg "habitscipline-client-data";
        "habitscipline-data" = habitsciplinePkg "habitscipline-data";
        "habitscipline-data-gen" = habitsciplinePkg "habitscipline-data-gen";
        "habitscipline-tui" = habitsciplinePkgWithOwnComp "habitscipline-tui";
      };

  habitsciplineRelease =
    final.symlinkJoin {
      name = "sparep-release";
      paths = final.lib.attrValues final.habitsciplinePackages;
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

                in
                  final.habitsciplinePackages // {
                    envparse = envparsePkg;
                  }
            );
        }
    );
}
