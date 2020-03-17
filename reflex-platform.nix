# Pin version of nixpkgs to ensure build reproduceability
import ((import <nixpkgs> {}).fetchFromGitHub {
  owner = "hexresearch";
  repo = "reflex-platform";
  rev = "292f86c54527d914111854178694c26752ee068f";
  sha256  = "06vv907mkxl1wz130j12752ycsymg4q5vlnck82a1w0k30r9ym5d";
})
# import ../reflex-platform
