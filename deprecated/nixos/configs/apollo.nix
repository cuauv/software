{config, pkgs, ...}:

let subName = "apollo"; in

{
  imports = [ ./sub.nix ];

  networking = {
    hostName = subName;

    interfaces = {
      net0.ip4 = [ { address = "192.168.0.91"; prefixLength = 24; } ];
      wifi0.ip4 = [ { address = "192.168.0.92"; prefixLength = 24; } ];
    };
  };

  environment.sessionVariables = { CUAUV_VEHICLE = subName; };

  services.udev.extraRules = ''
    SUBSYSTEM=="net", ACTION=="add", ATTR{address}=="b8:ae:ed:7b:d9:e3", NAME="net0"
    SUBSYSTEM=="net", ACTION=="add", ATTR{address}=="00:21:5c:b7:6c:72", NAME="wifi0"
  '';
}
