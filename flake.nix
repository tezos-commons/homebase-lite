# SPDX-FileCopyrightText: 2023 Tezos Commons
# SPDX-License-Identifier: LicenseRef-MIT-TC

{
  description = "The homebase-lite flake";

  nixConfig.flake-registry = "https://gitlab.com/morley-framework/morley-infra/-/raw/main/flake-registry.json";

  inputs.morley-infra.url = "gitlab:morley-framework/morley-infra";

  outputs = { self, flake-utils, morley-infra, ... }:
    (flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = morley-infra.legacyPackages.${system};

        pkgsStatic = pkgs.pkgsCross.musl64;

        inherit (morley-infra.utils.${system}) linker-workaround weeder-hacks run-chain-tests;

        # all local packages and their subdirectories
        # we need to know subdirectories for weeder and for cabal check
        local-packages = [
          { name = "homebase-lite"; subdirectory = "./."; }
        ];

        # names of all local packages
        local-packages-names = map (p: p.name) local-packages;

        # source with gitignored files filtered out
        projectSrc = pkgs.haskell-nix.haskellLib.cleanGit {
          name = "homebase-lite";
          src = ./.;
        };

        # haskell.nix package set
        # parameters:
        # - release -- 'true' for master and producion branches builds, 'false' for all other builds.
        #   This flag basically disables weeder related files production, haddock and enables stripping
        # - static -- build statically linked executables
        # - commitSha, commitDate -- git revision info used during compilation of packages with autodoc
        # - optimize -- 'true' to enable '-O1' ghc flag, we use it for publishing docker image for morley
        hs-pkgs = { release, static ? true, optimize ? false, commitSha ? null, commitDate ? null}:
          let
            haskell-nix = if static then pkgsStatic.haskell-nix else pkgs.haskell-nix;
          in haskell-nix.stackProject {
            src = projectSrc;

            # use .cabal files for building because:
            # 1. haskell.nix fails to work for package.yaml with includes from the parent directory
            # 2. with .cabal files haskell.nix filters out source files for each component, so only the changed components will rebuild
            ignorePackageYaml = true;

            modules = [
              # common options for all local packages:
              {
                packages = pkgs.lib.genAttrs local-packages-names (packageName: {
                  ghcOptions = with pkgs.lib;
                    # we use O1 for production binaries in order to improve their performance
                    # for end-users
                    [ (if optimize then "-O1" else "-O0") "-Werror"]
                    # override linker to work around issues with Template Haskell
                    ++ optionals static [ "-pgml=${linker-workaround pkgsStatic}" ]
                    # produce *.dump-hi files, required for weeder:
                    ++ optionals (!release) ["-ddump-to-file" "-ddump-hi"]
                    # do not erase any 'assert' calls
                    ++ optionals (!release) ["-fno-ignore-asserts"];
                  dontStrip = !release;  # strip in release mode, reduces closure size
                  doHaddock = !release;  # don't haddock in release mode

                  # in non-release mode collect all *.dump-hi files (required for weeder)
                  postInstall = if release then null else weeder-hacks.collect-dump-hi-files;
                  preBuild = ''
                    export MORLEY_DOC_GIT_COMMIT_SHA=${if release then pkgs.lib.escapeShellArg commitSha else "UNSPECIFIED"}
                    export MORLEY_DOC_GIT_COMMIT_DATE=${if release then pkgs.lib.escapeShellArg commitDate else "UNSPECIFIED"}
                  '';
                });
              }

              {
                # don't haddock dependencies
                doHaddock = false;
              }
            ];
        };

        hs-pkgs-development = hs-pkgs { release = false; };

        flake = hs-pkgs-development.flake {};

        packages = pkgs.lib.genAttrs local-packages-names (packageName: hs-pkgs-development."${packageName}".components);

        # returns a list of all components (library + exes + tests + benchmarks) for a package
        get-package-components = pkg: with pkgs.lib;
          optional (pkg ? library) pkg.library
          ++ attrValues pkg.exes
          ++ attrValues pkg.tests
          ++ attrValues pkg.benchmarks;

        # per-package list of components
        components = pkgs.lib.mapAttrs (pkgName: pkg: get-package-components pkg) packages;

      in pkgs.lib.lists.foldr pkgs.lib.recursiveUpdate {} [

        { inherit (flake) packages apps; }

        {
          legacyPackages = pkgs;

          devShells.default = (flake).devShell;

          packages = {
            # a derivation which generates a script for running weeder
            weeder-script = morley-infra.utils.${system}.weeder-script {
              hs-pkgs = hs-pkgs-development;
              inherit local-packages;
            };

            default = self.packages.${system}.all-components;

            # a list of all components from all packages in the project
            all-components = with pkgs.lib; pkgs.linkFarmFromDrvs "all-components" (flatten (attrValues components));

            haddock = with pkgs.lib; pkgs.linkFarmFromDrvs "haddock" (flatten (attrValues
              (mapAttrs (pkgName: pkg: optional (pkg ? library) pkg.library.haddock) packages)));

            contract-doc-dev = self.utils.${system}.contract-doc { release = false; };
          };

          checks = {
            trailing-whitespace = pkgs.build.checkTrailingWhitespace ./.;

            reuse-lint = pkgs.build.reuseLint ./.;
          };

          utils = {
            inherit run-chain-tests;

            # run some executables to produce contract documents
            contract-doc = { release, commitSha ? null, commitDate ? null }@releaseArgs: pkgs.runCommand "contract-doc" {
              buildInputs = [
                (hs-pkgs releaseArgs).homebase-lite.components.exes.homebase-lite
              ];
            } ''
              mkdir $out
              homebase-lite document --output $out/documentation.md
            '';

            contract-doc-release = { sha, date }: self.utils.${system}.contract-doc ({ release = true; commitSha = sha; commitDate = date; });

            build-release = { sha, date }:
              (hs-pkgs { release = true; optimize = true; commitSha = sha; commitDate = date; }).homebase-lite.components.exes.homebase-lite;
          };
        }
      ]));
}
