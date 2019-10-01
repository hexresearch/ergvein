# Pin version of nixpkgs to ensure build reproduceability
import ((import <nixpkgs> {}).fetchFromGitHub {
  owner = "reflex-frp";
  repo = "reflex-platform";
  rev = "37b7bdd46e204f7ac2e8906caf7c08bb0625c8ec";
  sha256  = "0d2qhi5rlvn5rdcj9ahrwqd2s0zk8nd9p13fk8580c65spz46m6b";
})
