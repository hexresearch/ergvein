{ stdenv, autoreconfHook, fetchFromGitHub, libcxx }:

stdenv.mkDerivation rec {
  name = "patchelf-0.9";
  nativeBuildInputs = [ autoreconfHook ];
  buildInputs = [ ]; /* stdenv.lib.optionalString stdenv.isAarch32  */
  NIX_CFLAGS_COMPILE = "-I${libcxx}/include/c++/v1";

  src = fetchFromGitHub {
    owner = "NixOS";
    repo = "patchelf";
    rev = "2ba64817ec6f3b714503ea6e6aa8439505bb7393";
    sha256 = "0sw0qgkxga151sabf4rgsm75k32l4b0bwi5j12wjj82rljpgl468";
  };

  meta = {
    homepage = http://nixos.org/patchelf.html;
    license = "GPL";
    description = "A small utility to modify the dynamic linker and RPATH of ELF executables";
    maintainers = [ stdenv.lib.maintainers.eelco ];
    platforms = stdenv.lib.platforms.all;
  };
}
