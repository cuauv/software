let pkgs = import <nixpkgs> {}; in

let customPythonPackages = (pythonPackages: builtins.attrValues rec {
  nanomsg = let
    pname = "nanomsg";
    version = "1.0";
  in
  pythonPackages.buildPythonPackage rec {
    name = "${pname}-${version}";

    src = pkgs.fetchurl {
      url = "mirror://pypi/${builtins.substring 0 1 pname}/${pname}/${name}.tar.gz";
      sha256 = "843be41258219d9d319cf434a68cac7669834ab9c993ea4bab5b3d87f62a7a13";
    };

    buildInputs = [ pkgs.pkgs.nanomsg ];

    doCheck = false;

    meta = {
      description = "Python library for nanomsg which does not compromise on usability or performance.";
      homepage = "https://github.com/tonysimpson/nanomsg-python";
    };
  };

  python-engineio = let
    pname = "python-engineio";
    version = "1.3.0";
  in
  pythonPackages.buildPythonPackage rec {
    name = "${pname}-${version}";

    src = pkgs.fetchurl {
      url = "mirror://pypi/${builtins.substring 0 1 pname}/${pname}/${name}.tar.gz";
      sha256 = "410dca1e0b69314126ea7ac7bd71bf30a6600f1f9e39d5f79412f8418d3d0267";
    };

    propagatedBuildInputs = [ pythonPackages.six ];

    doCheck = false;

    meta = {
      description = "TODO";
      homepage = "TODO";
    };
  };

  python-socketio = let
    pname = "python-socketio";
    version = "1.7.2";
  in
  pythonPackages.buildPythonPackage rec {
    name = "${pname}-${version}";

    src = pkgs.fetchurl {
      url = "mirror://pypi/${builtins.substring 0 1 pname}/${pname}/${name}.tar.gz";
      sha256 = "aa612e5d2901c94946c3004b0e58bb9fb08b214afee59ce4aeafcf8df4deb79a";
    };

    propagatedBuildInputs = [ python-engineio pythonPackages.six ];

    doCheck = false;

    meta = {
      description = "TODO";
      homepage = "TODO";
    };
  };

  flask-socketio = let
    pname = "flask-socketio";
    version = "2.8.5";
  in
  pythonPackages.buildPythonPackage rec {
    name = "Flask-SocketIO-${version}";

    src = pkgs.fetchurl {
      url = "mirror://pypi/${builtins.substring 0 1 pname}/${pname}/${name}.tar.gz";
      sha256 = "1f4fc467b2b4b94786d51c7c210f2bd05a419e4adc7cbfa4e4218e74a4c96fdd";
    };

    propagatedBuildInputs = [ pythonPackages.flask python-socketio python-engineio ];

    doCheck = false;

    meta = {
      description = "TODO";
      homepage = "TODO";
    };
  };
}); in

let opencvOverrides = { enableFfmpeg = true; }; in

pkgs.stdenv.mkDerivation rec {
  name = "cuauv-software";
  enableParallelBuilding = true;

  buildInputs = 
  (with pkgs; [
    boost
    eigen
    glfw
    glm
    gnome3.gtk
    gnome3.gtkmm
    gtest
    libconfig
    libdc1394
    nanomsg
    ncurses
    opencv3
    pkgconfig
    popt
    protobuf

    (pkgs.callPackage ./pkgs/ueyed/default.nix {})
    (pkgs.callPackage ./pkgs/ximea/default.nix {})
  ]) ++ [

    (pkgs.python35.withPackages (pypkgs: with pypkgs; [ 
      # numpy is much faster with blas over openblas
      (pkgs.hiPrio (pypkgs.numpy.overrideAttrs (oldAttrs: rec {
        passthru = oldAttrs.passthru // { blas = pkgs.blas; };
        propagatedBuildInputs = [ pkgs.blas ];
        preBuild = ''
          echo "Creating site.cfg file..."
          cat << EOF > site.cfg
          [blas]
          include_dirs = ${passthru.blas}/include
          library_dirs = ${passthru.blas}/lib
          EOF
        '';
      })))

      (opencv3.override opencvOverrides)

      cython
      eventlet
      flask
      gevent
      ipython
      paramiko
      pyserial
      pyyaml
      redis
      scipy
      six
      tabulate
      termcolor
      tornado
      watchdog
    ] ++ customPythonPackages pkgs.python35Packages))

    (pkgs.python27.withPackages (pypkgs: with pypkgs; [ 
      (opencv3.override opencvOverrides)

      eventlet
      eventlet
      gevent
      matplotlib
      numpy
      posix_ipc
      pycairo
      pygame
      pygobject3
      pysqlite
      redis
      six
      systemd
      termcolor
      wxPython
    ] ++ customPythonPackages pkgs.python27Packages))
  ];
}
