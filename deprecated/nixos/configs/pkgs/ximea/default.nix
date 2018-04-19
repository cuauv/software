{pkgs, stdenv, fetchurl}:

pkgs.stdenv.mkDerivation rec {
  name = "ximea-${version}";
  version = "4.13.11";
  enableParallelBuilding = true;

  src = pkgs.fetchurl {
    url = "https://cuauv.org/nix-res-private/XIMEA_Linux_SP.tgz";
    sha256 = "5779b4f3e6133e709a3dac951c86cfcbbfa7998b9d305525a398c5561ac127cc";
  };

  # Stripping after patchelf corrupts executable,
  # see https://github.com/NixOS/patchelf
  dontStrip = true;

  libPath = stdenv.lib.makeLibraryPath (with pkgs; [
    stdenv.cc.cc
    libtiff
  ]);

  installPhase = ''
    patchInterpreter() {
      patchelf --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" "$1"
    }

    # No harm in adding extra paths, extras stripped away
    patchRpath() {
      patchelf --set-rpath "$out"/lib:"$libPath" "$1"
    }

    mkdir -p "$out"/lib
    mv libs/{gentl,libraw1394,libusb}/X64/* api/X64/* "$out"/lib/
    ln -s "$out"/lib/libm3api.so.2 "$out"/lib/libm3api.so
    for file in "$out"/lib/*; do
      patchRpath "$file"
    done

    mkdir -p "$out"/include/m3api/
    mv include/* "$out"/include/m3api

    mkdir -p "$out"/lib/udev/rules.d/
    mv libs/libusb/99-ximea.rules "$out"/lib/udev/rules.d/
  '';
}
