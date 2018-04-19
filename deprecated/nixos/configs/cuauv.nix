{config, lib, pkgs, ...}:

with lib;

{
  imports = [
    ./hardware-configuration.nix
    ./secrets/users.nix
  ];

  environment.systemPackages = with pkgs; [
    arc-theme
    autossh
    bc
    chicken
    cmake
    ctags
    emacs
    file
    git
    git-up
    gitAndTools.git-annex
    glxinfo
    gst_plugins_bad
    htop
    iotop
    jq
    lshw
    neovim
    ninja
    nodejs
    nodePackages.webpack
    oh-my-zsh
    pciutils
    protobuf
    psmisc
    ripgrep
    shellcheck
    stow
    sysstat
    termite
    tmux
    usbutils
    wget
    xorg.xauth
    xsel
  ];

  environment.etc."xdg/gtk-3.0/settings.ini" = {
    text = ''
      [Settings]
      gtk-theme-name=Arc-Darker
    '';
    mode = "444";
  };

  programs.zsh = {
    enable = true;

    interactiveShellInit = "source \${CUAUV_SOFTWARE}/nixos/shell.zsh";
  };

  nixpkgs.config.packageOverrides = pkgs: {
    neovim = pkgs.neovim.override {
      withPython = true;
      withPython3 = true;
      vimAlias = true;
    };

    ximea = pkgs.callPackage ./pkgs/ximea/default.nix {};
  };

  environment.sessionVariables = rec {
    CUAUV_SOFTWARE = "/home/software/cuauv/software/";
    CUAUV_CONTEXT = "development";
    CUAUV_LOG = "/home/software/cuauv/log";
    CUAUV_LOCALE = "transdec";
    NIXOS = "1";

    PYTHONPATH = "${CUAUV_SOFTWARE}";
    GOPATH = "${CUAUV_SOFTWARE}gocode";
    PATH = "$PATH:${CUAUV_SOFTWARE}link-stage:${GOPATH}/bin";
    CHICKEN_REPOSITORY = "${CUAUV_SOFTWARE}link-stage/chicken";
    C_INCLUDE_PATH = CUAUV_SOFTWARE;
    CGO_LDFLAGS = "-L${CUAUV_SOFTWARE}link-stage";
    CSC_OPTIONS = ''
      -L$CUAUV_SOFTWARE/link-stage -C -I$CUAUV_SOFTWARE -I$CUAUV_SOFTWARE \
      -Wl,-rpath,${CUAUV_SOFTWARE}link-stage
    '';
    VISION_TEST_PATH = "/home/software/vid";

    ZSH = "${pkgs.oh-my-zsh}/share/oh-my-zsh";
    ZSH_CACHE_DIR = "$HOME/.cache/oh-my-zsh";
    EDITOR = "vim";
    GTK2_RC_FILES = "${pkgs.arc-theme}/share/themes/Arc-Darker/gtk-2.0/gtkrc:$GTK2_RC_FILES";
  };

  users = {
    mutableUsers = false;

    extraUsers.software = {
      isNormalUser = true;
      description = "CUAUV";
      extraGroups = [ "wheel" "networkmanager" "hostvideo" ];
      shell = "/run/current-system/sw/bin/zsh";
    };
    extraGroups.hostvideo.gid = 44; # GID of video group on host computer when
                                    # running NixOS in container
    extraUsers.ueyed = {};
    extraGroups.ueyed = {};
  };

  security.sudo.extraConfig = "%wheel ALL=(ALL) NOPASSWD: ALL";

  services.openssh = {
    enable = true;
    permitRootLogin = "yes";
    forwardX11 = true;
  };
  services.redis.enable = true;

  hardware.opengl.enable = true;
  time.timeZone = "America/Los_Angeles";
  networking.firewall.enable = false;

  nix.gc.automatic = true;
  nix.buildCores = 0; # Detect number of cores present
  nixpkgs.config.allowUnfree = true;
}
