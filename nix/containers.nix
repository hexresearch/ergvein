{ isProd ? false
, isProfile ? false
, isHotfix  ? false
, containerTag ? "latest"
, prefixName ? ""
, gitHash             # commit hash
, gitTag      ? null  # current tag
, gitBranch           # branch name
, buildNumber ? null  # CI build number
, doAllCheck  ? true  # allows to disable all tests for fast build
}:
let
  release = import ./release.nix { inherit
    isProd
    isProfile
    isHotfix
    gitHash
    gitTag
    gitBranch
    buildNumber
    doAllCheck; };
  pkgs = release.pkgs;

  takeOnly = name: path: pkgs.runCommandNoCC "only-${name}" {} ''
    mkdir -p $out
    cp ${path} $out/${name}
  '';

  baseImage = pkgs.dockerTools.pullImage {
    imageName = "alpine";
    imageDigest = "sha256:e1871801d30885a610511c867de0d6baca7ed4e6a2573d506bbec7fd3b03873f";
    sha256 = "05wcg38vsygjzf59cspfbb7cq98c7x18kz2yym6rbdgx960a0kyq";
  };

  make-ergvein-docker = name: executable: cnts: pkgs.dockerTools.buildImage {
    name = "${prefixName}${name}";
    fromImage = baseImage;
    tag = containerTag;
    contents = [
      (takeOnly executable "${pkgs.haskellPackages.ergvein}/bin/${executable}")
      ] ++ cnts;
    config = {
      Entrypoint = [
        "/${executable}"
      ];
    };
  };

  # As we place all executables in single derivation the derivation takes them
  # from it and allows us to make thin containers for each one.

  index-server-docker = make-ergvein-docker "ergvein-index-server" "ergvein-index-server" [];
in {
  inherit
    index-server-docker;
}
