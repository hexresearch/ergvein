# Pin version of nixpkgs to ensure build reproduceability
import ((import <nixpkgs> {}).fetchFromGitHub {
  owner = "hexresearch";
  repo = "reflex-platform";
  rev = "c33192410d202edbd135b6a017cf5e3c2a6cc013";
  sha256  = "1gnc784lldk7b585shknh07w3wjaz046mj3bpy2q0360ml081qn5";
})
/* import ../reflex-platform  */
