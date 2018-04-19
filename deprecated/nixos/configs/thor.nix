{config, pkgs, ...}:

{
  imports = [ ./sub.nix ];

  networking.hostName = "polaris";
  environment.sessionVariables = { CUAUV_VEHICLE = "thor"; };
}
