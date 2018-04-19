{config, pkgs, ...}:

{
  imports = [ ./sub.nix ];

  networking.hostName = "loki";
  environment.sessionVariables = { CUAUV_VEHICLE = "loki"; };
}
