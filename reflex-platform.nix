# Pin version of nixpkgs to ensure build reproduceability
import ((import <nixpkgs> {}).fetchFromGitHub {
  owner = "hexresearch";
  repo = "reflex-platform";
  rev = "c87ab9bb1e760836c299a82dbcf1234d04a9ca9c";
  sha256  = "0m72g99rb6g6b89b5wwwwr6ss40rsaj9ild7wswyzxgzq1cphwj0";
})
/* import ../reflex-platform */
