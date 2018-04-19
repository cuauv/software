{config, pkgs, ...}:

{
  imports = [ ./cuauv.nix ];

  boot = {
    loader.grub.enable = false;
    isContainer = true;
  };

  networking.hostName = "auvbox";
  environment.sessionVariables = { CUAUV_VEHICLE = "artemis"; };
}
