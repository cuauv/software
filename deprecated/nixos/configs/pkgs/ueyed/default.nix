{pkgs, stdenv, fetchurl}:

stdenv.mkDerivation rec {
  name = "ueyed-${version}";
  version = "4.40";
  enableParallelBuilding = true;

  # Stripping after patchelf corrupts executable,
  # see https://github.com/NixOS/patchelf
  dontStrip = true;

  libPath = stdenv.lib.makeLibraryPath (with pkgs; [
    stdenv.cc.cc
    libcap
    qt4
    xorg.libX11
  ]);

  src = pkgs.fetchurl {
    url = "https://cuauv.org/nix-res-private/uEye-Linux-440-64.tgz";
    sha256 = "129d79e6ded03179ae3752081f556d0c2bb0ec54b4c680402e571e200c882144";

    #url = "https://cuauv.org/nix-res-private/uEye-Linux-4.82.00-64-bit.tgz";
    #sha512 = "6997e8a3781e1884047d51ec239ba38acc4b09c3a79374ce3228bef4ce112acff84b04ceccf1a50ddea54f2e45e3a1ee0e7266b1d40ff08cf8c7e5df55f26742";
  };

  unpackPhase = ''
    tar -xf "$src"
    cd uEye_Linux_4.40_64_Bit
    bash ueyesdk-setup-${version}-eth-amd64.gz.run --noexec --target ueyeethd
  '';

  installPhase = ''
    patchInterpreter() {
      patchelf --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" "$1"
    }

    # No harm in adding extra paths, extras stripped away
    patchRpath() {
      patchelf --set-rpath "$out"/lib:"$libPath" "$1"
    }

    mkdir -p "$out"
    mv ueyeethd/bin64 "$out/bin"
    mv ueyeethd/lib64 "$out/lib"
    mv ueyeethd/include "$out/"
    mkdir -p "$out/doc"
    mv ueyeethd/doc "$out/doc/ids"

    for file in "$out"/bin/*; do
      patchInterpreter "$file"
      patchRpath "$file"
    done

    for file in "$out"/lib/*; do
      patchRpath "$file"
    done
    
    ln -s "$out"/lib/libueye_api64.so.${version} "$out"/lib/libueye_api.so

    mv ueyeethd/ueyeethdnotify "$out/bin/"
    sed -i -e 's#/usr/local/share/ueye/bin/##' "$out/bin/ueyeethdnotify"
    chmod +x "$out/bin/ueyeethdnotify"

    mv ueyeethd/ueyeethdrc "$out/bin/"
    sed -i -E \
      -e "s#^N=.+#N=ueyeethdrc#" \
      -e "s#^DAEMON_BINDIR=\".+\"#DAEMON_BINDIR=\"$out/bin\"#" \
      -e "s#^DAEMON_GROUP=.+\$#DAEMON_GROUP=\"ueyed\"#" \
      -e "s#^DAEMON_CONF=\".+\"#DAEMON_CONF=\"$out/etc/ueyeethd.conf\"#" \
      -e "s#^PATH=.+##" \
      "$out/bin/ueyeethdrc"

    mkdir -p "$out/etc/"
    cat > "$out"/etc/ueyeethd.conf <<EOF
    [Parameters]
    Interfaces = cam0
    EOF
  '';
}
