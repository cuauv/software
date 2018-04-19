{config, pkgs, ...}:

let subName = "artemis"; in

{
  imports = [ ./sub.nix ];

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # boot.loader.grub.efiSupport = true;
  # boot.loader.grub.efiInstallAsRemovable = true;
  # boot.loader.efi.efiSysMountPoint = "/boot/efi";
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda";

  networking = {
    hostName = subName;

    interfaces = {
      net0.ip4 = [ { address = "192.168.0.93"; prefixLength = 24; } ];
      cam0.ip4 = [ { address = "169.254.178.93"; prefixLength = 24; } ];
      #wifi0.ip4 = [ { address = "192.168.0.94"; prefixLength = 24; } ];
    };

    nameservers = [ "192.168.0.1" "8.8.8.8" "8.8.4.4" ];
    defaultGateway = "192.168.0.1";

    wireless.enable = true;
    wireless.networks = {
      "CUAUV-LAB".psk = "seamonkey";
      "CUAUV-POOL".psk = "seamonkey";
    };
  };

  environment.sessionVariables = { CUAUV_VEHICLE = subName; };

  services.udev.extraRules = ''
    SUBSYSTEM=="net", ACTION=="add", ATTR{address}=="00:0c:8b:20:04:14", NAME="net0"
    SUBSYSTEM=="net", ACTION=="add", ATTR{address}=="00:30:64:10:df:8a", NAME="cam0"
  '';
  #SUBSYSTEM=="net", ACTION=="add", ATTR{address}=="00:21:5c:b7:6c:72", NAME="wifi0"
}
