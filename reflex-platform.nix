# Pin version of nixpkgs to ensure build reproduceability
import ((import <nixpkgs> {}).fetchFromGitHub {
  owner = "hexresearch";
  repo = "reflex-platform";
  rev = "51dd4ef19141d055daee63e1ee8736dec5037ae7";
  sha256  = "0fb0kaa31bnr1m326660vwrpmmyha3831kbyvw0a8v68ggy4kx8i";
})
/* import ../reflex-platform  */
