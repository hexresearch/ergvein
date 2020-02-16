{ isProd      ? false
, isDebug     ? false
, isHotfix    ? false
, isProfile   ? false
, gitHash             # commit hash
, gitTag      ? null  # current tag
, gitBranch           # branch name
, buildNumber ? null  # CI build number
, containerTag ? "latest"
, prefixName ? ""
}:
let
  reflex-platform = (import ../reflex-platform.nix) {
    nixpkgsOverlays = [
      (self: super: import ../nixpkgs-overlays/default.nix self super )
    ];
    config = {
      android_sdk.accept_license = true;
      allowBroken = true;
    };
  };
  pkgs = reflex-platform.nixpkgs;

  baseImage = pkgs.dockerTools.pullImage {
      imageName = "alpine";
      imageDigest = "sha256:e1871801d30885a610511c867de0d6baca7ed4e6a2573d506bbec7fd3b03873f";
      sha256 = "05wcg38vsygjzf59cspfbb7cq98c7x18kz2yym6rbdgx960a0kyq";
    };

  # As we place all executables in single derivation the derivation takes them
  # from it and allows us to make thin containers for each one.
  takeOnly = name: path: innerPath: pkgs.runCommandNoCC "only-${name}" {} ''
    mkdir -p $out/$(dirname ${innerPath})
    cp ${path} $out/${innerPath}
  '';
  takeFolder = name: path: innerPath: pkgs.runCommandNoCC "folder-${name}" {} ''
    mkdir -p $out/${innerPath}
    cp -r ${path}/* $out/${innerPath}
  '';

  mkDockerImage = name: cnts: pkgs.dockerTools.buildImage {
    name = "${prefixName}${name}";
    fromImage = baseImage;
    tag = containerTag;
    contents = cnts;
    config.WorkingDir = "/index-server";
    config.Cmd = ["--listen" "./configuration.yaml"];
  };

  index-server-container = mkDockerImage "index-server" [
    (takeFolder "index-server" ../index-server "srv")
  ];
in { inherit
  index-server-container
  ;
}