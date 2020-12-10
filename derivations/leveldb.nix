{ stdenv, fetchFromGitHub, fixDarwinDylibNames, snappy, cmake }:

stdenv.mkDerivation rec {
  pname = "leveldb";
  version = "1.22";

  src = fetchFromGitHub {
    owner = "google";
    repo = "leveldb";
    rev = "${version}";
    sha256 = "0qrnhiyq7r4wa1a4wi82zgns35smj94mcjsc7kfs1k6ia9ys79z7";
  };

  buildInputs = [ snappy ];

  nativeBuildInputs = [cmake]
    ++ stdenv.lib.optional stdenv.isDarwin fixDarwinDylibNames ;

  buildPhase = ''
    mkdir -p ../tmp
    cmake -DBUILD_SHARED_LIBS:BOOL=ON -DBUILD_STATIC_LIBS:BOOL=ON -DCMAKE_BUILD_TYPE=Release .. && cmake --build .
    cp  ./lib* ../tmp
    cmake -DCMAKE_BUILD_TYPE=Release .. && cmake --build .
    cp  ./lib* ../tmp
  '';

  installPhase = "
    mkdir -p $out/{bin,lib,include}
    cp -r ../include $out
    cp ../helpers/memenv/memenv.h $out/include/leveldb/helpers
    cp ../build/leveldbutil $out/bin
    cp -r ../tmp/lib* $out/lib
  ";

  meta = with stdenv.lib; {
    homepage = "https://github.com/google/leveldb";
    description = "Fast and lightweight key/value database library by Google";
    license = licenses.bsd3;
    platforms = platforms.all;
  };
}
