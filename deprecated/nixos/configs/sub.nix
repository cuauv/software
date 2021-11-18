{config, pkgs, ...}:

{
  imports = [ ./cuauv.nix ];

  environment.systemPackages = with pkgs; [
    ximea
    stack # Only needed for auv-pooltest, takes long to build
  ];

  environment.sessionVariables = { CUAUV_CONTEXT = "vehicle"; };

  users.extraUsers.software.extraGroups = [ "plugdev" ]; # XIMEA group

  services.timesyncd.enable = true;

  services.udev = {
    extraRules = ''
      ## Artemis / Apollo Hubless Serial Board

      # FTDI 1
      KERNEL=="ttyUSB*", DRIVERS=="ftdi_sio", ATTRS{interface}=="Arpolis 1", ATTRS{bInterfaceNumber}=="00", SYMLINK+="ttyUSB_arpolis_GX4", MODE="0666", TAG+="systemd"
      KERNEL=="ttyUSB*", DRIVERS=="ftdi_sio", ATTRS{interface}=="Arpolis 1", ATTRS{bInterfaceNumber}=="01", SYMLINK+="ttyUSB_arpolis_GPIO", MODE="0666", TAG+="systemd"
      KERNEL=="ttyUSB*", DRIVERS=="ftdi_sio", ATTRS{interface}=="Arpolis 1", ATTRS{bInterfaceNumber}=="02", SYMLINK+="ttyUSB_arpolis_VROOOOM", MODE="0666", TAG+="systemd"
      KERNEL=="ttyUSB*", DRIVERS=="ftdi_sio", ATTRS{interface}=="Arpolis 1", ATTRS{bInterfaceNumber}=="03", SYMLINK+="ttyUSB_arpolis_ACT", MODE="0666", TAG+="systemd"

      # FTDI 2
      KERNEL=="ttyUSB*", DRIVERS=="ftdi_sio", ATTRS{interface}=="Arpolis 2", ATTRS{bInterfaceNumber}=="00", SYMLINK+="ttyUSB_arpolis_PD", MODE="0666", TAG+="systemd"
      KERNEL=="ttyUSB*", DRIVERS=="ftdi_sio", ATTRS{interface}=="Arpolis 2", ATTRS{bInterfaceNumber}=="01", SYMLINK+="ttyUSB_arpolis_VROOM", MODE="0666", TAG+="systemd"
      KERNEL=="ttyUSB*", DRIVERS=="ftdi_sio", ATTRS{interface}=="Arpolis 2", ATTRS{bInterfaceNumber}=="02", SYMLINK+="ttyUSB_arpolis_POD", MODE="0666", TAG+="systemd"
      KERNEL=="ttyUSB*", DRIVERS=="ftdi_sio", ATTRS{interface}=="Arpolis 2", ATTRS{bInterfaceNumber}=="03", SYMLINK+="ttyUSB_arpolis_H2O", MODE="0666", TAG+="systemd"

      # FTDI 3
      KERNEL=="ttyUSB*", DRIVERS=="ftdi_sio", ATTRS{interface}=="Arpolis 3", ATTRS{bInterfaceNumber}=="00", SYMLINK+="ttyUSB_arpolis_LED", MODE="0666", TAG+="systemd"
      KERNEL=="ttyUSB*", DRIVERS=="ftdi_sio", ATTRS{interface}=="Arpolis 3", ATTRS{bInterfaceNumber}=="01", SYMLINK+="ttyUSB_arpolis_X1", MODE="0666", TAG+="systemd"
      KERNEL=="ttyUSB*", DRIVERS=="ftdi_sio", ATTRS{interface}=="Arpolis 3", ATTRS{bInterfaceNumber}=="02", SYMLINK+="ttyUSB_arpolis_X3", MODE="0666", TAG+="systemd"
      KERNEL=="ttyUSB*", DRIVERS=="ftdi_sio", ATTRS{interface}=="Arpolis 3", ATTRS{bInterfaceNumber}=="03", SYMLINK+="ttyUSB_arpolis_X2", MODE="0666", TAG+="systemd"

      # FTDI 4
      KERNEL=="ttyUSB*", DRIVERS=="ftdi_sio", ATTRS{interface}=="Arpolis 4", ATTRS{bInterfaceNumber}=="00", SYMLINK+="ttyUSB_arpolis_DVL", MODE="0666", TAG+="systemd"
      KERNEL=="ttyUSB*", DRIVERS=="ftdi_sio", ATTRS{interface}=="Arpolis 4", ATTRS{bInterfaceNumber}=="01", SYMLINK+="ttyUSB_arpolis_MERGE", MODE="0666", TAG+="systemd"
      KERNEL=="ttyUSB*", DRIVERS=="ftdi_sio", ATTRS{interface}=="Arpolis 4", ATTRS{bInterfaceNumber}=="02", SYMLINK+="ttyUSB_arpolis_HIM", MODE="0666", TAG+="systemd"
      KERNEL=="ttyUSB*", DRIVERS=="ftdi_sio", ATTRS{interface}=="Arpolis 4", ATTRS{bInterfaceNumber}=="03", SYMLINK+="ttyUSB_arpolis_POE", MODE="0666", TAG+="systemd"

      ## LCD Board
      # Ignore the C232H cable (for LCD)
      # We're using MPSSE mode for this, and it doesn't work if the ft232_sio driver loads it 
      # NOTE: libmpsse must be modified to avoid probing any other VID/PID
      # Why is argo here??
      SUBSYSTEMS=="usb", ATTRS{product}=="C232HM-DDHSL-0", ATTRS{idProduct}=="6014", ATTRS{idVendor}=="0403", SYMLINK+="ttyUSB_argo_lcd_0", MODE="0666", TAG+="systemd"
    '';

    packages = with pkgs; [ ximea ];
  };
}

# vim:nowrap
