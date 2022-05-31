final: previous:
with final.lib;
with final.haskell.lib;

{
  habitsciplinePackages =
    let
      habitsciplinePkg = name:
        overrideCabal
          (
            final.haskellPackages.callCabal2nixWithOptions name
              (final.gitignoreSource (../. + "/${name}"))
              "--no-hpack"
              { }
          )
          (old: {
            doBenchmark = true;
            doHaddock = false;
            doCoverage = false;
            doHoogle = false;
            doCheck = false; # Only check the release version.
            hyperlinkSource = false;
            enableLibraryProfiling = false;
            enableExecutableProfiling = false;

            configureFlags = (old.configureFlags or [ ]) ++ [
              # Optimisations
              "--ghc-options=-O2"
              # Extra warnings
              "--ghc-options=-Wall"
              "--ghc-options=-Wincomplete-uni-patterns"
              "--ghc-options=-Wincomplete-record-updates"
              "--ghc-options=-Wpartial-fields"
              "--ghc-options=-Widentities"
              "--ghc-options=-Wredundant-constraints"
              "--ghc-options=-Wcpp-undef"
              "--ghc-options=-Werror"
            ];
            buildDepends = (old.buildDepends or [ ]) ++ [
              final.haskellPackages.autoexporter
            ];
            # Ugly hack because we can't just add flags to the 'test' invocation.
            # Show test output as we go, instead of all at once afterwards.
            testTarget = (old.testTarget or "") + " --show-details=direct";
          });
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

  habitsciplineReleasePackages = mapAttrs
    (_: pkg: justStaticExecutables (doCheck pkg))
    final.habitsciplinePackages;


  habitsciplineRelease =
    final.symlinkJoin {
      name = "habitscipline-release";
      paths = final.lib.attrValues final.habitsciplineReleasePackages;
    };

  haskellPackages =
    previous.haskellPackages.override (
      old:
      {
        overrides =
          final.lib.composeExtensions
            (
              old.overrides or (
                _:
                _:
                { }
              )
            )
            (
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
                      self.callCabal2nix "envparse" envparseRepo { }
                    );

                in
                final.habitsciplinePackages // {
                  envparse = envparsePkg;
                }
            );
      }
    );
}
