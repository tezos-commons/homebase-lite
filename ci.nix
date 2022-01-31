# SPDX-FileCopyrightText: 2022 Tezos Commons
#
# SPDX-License-Identifier: LicenseRef-MIT-TC

rec {
  sources = import ./nix/sources.nix;
  # pkgs for builtins.currentSystem
  pkgs = import ./nix/nixpkgs-with-haskell-nix.nix {};
  pkgsStatic = pkgs.pkgsCross.musl64;
  xrefcheck = import sources.xrefcheck;
  weeder-hacks = import sources.haskell-nix-weeder { inherit pkgs; };
  tezos-client = (import "${sources.tezos-packaging}/nix/build/pkgs.nix" {}).ocamlPackages.tezos-client;

  # all local packages and their subdirectories
  # we need to know subdirectories for weeder and for cabal check
  local-packages = [
    { name = "homebase-lite"; subdirectory = "./."; }
  ];

  # names of all local packages
  local-packages-names = map (p: p.name) local-packages;

  # source with gitignored files filtered out
  projectSrc = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "morley";
    src = ./.;
  };

  # GHC has issues with compiling statically linked executables when Template Haskell
  # is used, this wrapper for ld fixes it. Another solution we could use is to link
  # GHC itself statically, but it seems to break haddock.
  # See https://github.com/input-output-hk/haskell.nix/issues/914#issuecomment-897424135
  linker-workaround = pkgs.writeShellScript "linker-workaround" ''
    # put all flags into 'params' array
    source ${pkgsStatic.stdenv.cc}/nix-support/utils.bash
    expandResponseParams "$@"

    # check if '-shared' flag is present
    hasShared=0
    for param in "''${params[@]}"; do
      if [[ "$param" == "-shared" ]]; then
        hasShared=1
      fi
    done

    if [[ "$hasShared" -eq 0 ]]; then
      # if '-shared' is not set, don't modify the params
      newParams=( "''${params[@]}" )
    else
      # if '-shared' is present, remove '-static' flag
      newParams=()
      for param in "''${params[@]}"; do
        if [[ ("$param" != "-static") ]]; then
          newParams+=( "$param" )
        fi
      done
    fi

    # invoke the actual linker with the new params
    exec x86_64-unknown-linux-musl-cc @<(printf "%q\n" "''${newParams[@]}")
  '';

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
              ++ optionals static [ "-pgml=${linker-workaround}" ]
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

  packages = pkgs.lib.genAttrs local-packages-names (packageName: hs-pkgs-development."${packageName}".components);

  # returns a list of all components (library + exes + tests + benchmarks) for a package
  get-package-components = pkg: with pkgs.lib;
    optional (pkg ? library) pkg.library
    ++ attrValues pkg.exes
    ++ attrValues pkg.tests
    ++ attrValues pkg.benchmarks;

  # per-package list of components
  components = pkgs.lib.mapAttrs (pkgName: pkg: get-package-components pkg) packages;

  # a list of all components from all packages in the project
  all-components = with pkgs.lib; flatten (attrValues components);

  haddock = with pkgs.lib; flatten (attrValues
    (mapAttrs (pkgName: pkg: optional (pkg ? library) pkg.library.haddock) packages));

  # run some executables to produce contract documents
  contract-doc = { release, commitSha ? null, commitDate ? null }@releaseArgs: pkgs.runCommand "contract-doc" {
    buildInputs = [
      (hs-pkgs releaseArgs).homebase-lite.components.exes.homebase-lite
    ];
  } ''
    mkdir $out
    homebase-lite document --name Homebase-Lite --output \
      $out/documentation.md
  '';
  contract-doc-dev = contract-doc {release = false;};
  contract-doc-release = { sha, date }@commitInfo: contract-doc ({release = true; commitSha = sha; commitDate = date;});
  build-release = { sha, date }@commitInfo:
    (hs-pkgs {release = true; optimize = true; commitSha = sha; commitDate = date;}).homebase-lite.components.exes.homebase-lite;

  # nixpkgs has weeder 2, but we use weeder 1
  weeder-legacy = pkgs.haskellPackages.callHackageDirect {
    pkg = "weeder";
    ver = "1.0.9";
    sha256 = "0gfvhw7n8g2274k74g8gnv1y19alr1yig618capiyaix6i9wnmpa";
  } {};

  # a derivation which generates a script for running weeder
  weeder-script = weeder-hacks.weeder-script {
    hs-pkgs = hs-pkgs-development;
    local-packages = local-packages;
    weeder = weeder-legacy;
  };

  # stack2cabal is broken because of strict constraints, set 'jailbreak' to ignore them
  stack2cabal = pkgs.haskell.lib.overrideCabal pkgs.haskellPackages.stack2cabal (drv: {
    jailbreak = true;
    broken = false;
  });
}
