# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  environment.systemPackages = with pkgs; [
    vim
    git
    htop
    gcc
    binutils
    file

    tmux
    wget
    ag
    fzf

    dina-font
    termite
    emacs
    firefox
    slop maim
    dunst
    libnotify
    haskellPackages.xmobar
  ];

  programs.bash.interactiveShellInit = ''
    bind '"\e[A":history-search-backward'
    bind '"\e[B":history-search-forward'
  '';

  programs.bash.enableCompletion = true;

  fonts.fonts = with pkgs; [ dina-font ];
  fonts.enableDefaultFonts = true;

  networking = {
    hostName = "arc";
    networkmanager = {
      enable = true;
      unmanaged = [ "enp0s20f0u3c2" ];
    };
  };

  # Select internationalisation properties.
  i18n = {
    consoleKeyMap = "sv-latin1";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Europe/Stockholm";

  services.xserver = {
    enable = true;
    layout = "se";
    xkbOptions = "ctrl:nocaps";
    displayManager.slim = {
      enable = true;
      autoLogin = true;
      defaultUser = "fizzo";
    };
    desktopManager.default = "none";
    windowManager.default = "xmonad";
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = haskellPackages: [ haskellPackages.network haskellPackages.xmobar];
    };
    libinput = {
      enable = true;
      tapping = false;
    };
    # synaptics.enable = true;
    # synaptics.twoFingerScroll = true;
  };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  hardware = {
    pulseaudio.enable = true;
    pulseaudio.support32Bit = true;
  };
  
  users.extraUsers.fizzo = {
    isNormalUser = true;
    home = "/home/fizzo";
    extraGroups = [ "wheel" "networkmanager" ];
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.09";

}
