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
    vim git tree htop file
    gcc binutils gnumake
    pciutils

    nix-repl
    
    st
    tmux wget ag fzf ncurses ranger
    atool zip unzip
    dina-font dmenu termite
    dunst libnotify
    emacs evince firefox
    slop maim mediainfo
    pavucontrol baobab lxappearance
    ffmpeg-full lame
    deadbeef-with-plugins picard
    discord
    arc-theme arc-icon-theme gtk-engine-murrine
    mpv sxiv
    sshfs-fuse
    postgresql
    networkmanagerapplet

    ghc
    haskellPackages.xmobar trayer
  ];

  environment.variables = {
    NO_AT_BRIDGE = "1";
    GTK_DATA_PREFIX = "/run/current-system/sw/";
    EDITOR = "vim";
    VISUAL = "vim";
    BROWSER = "firefox";
  };

  services.postgresql.enable = true;
  services.postgresql.authentication = "local all all ident";
  nixpkgs.config.allowUnfree = true;

  #virtualisation.docker.enable = true;

  # Workaround for docker cgroups, as systemd 232 broke something
  #boot.kernelParams = [ "systemd.legacy_systemd_cgroup_controller=yes" ];

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
    displayManager.sddm = {
      enable = true;
      autoLogin = {
        enable = true;
        user = "fizzo";
      };
    };
    desktopManager.default = "none";
    windowManager.default = "xmonad";
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = hp: [ hp.hostname ];
    };
    libinput = {
      enable = true;
      tapping = false;
    };
    # synaptics.enable = true;
    # synaptics.twoFingerScroll = true;
  };

  services.nscd.enable = false;
  services.openssh.enable = true;
  services.printing.enable = true;


  hardware = {
    pulseaudio.enable = true;
    pulseaudio.support32Bit = true;
    opengl.driSupport32Bit = true;
  };
  
  users.extraUsers.fizzo = {
    isNormalUser = true;
    home = "/home/fizzo";
    extraGroups = [ "wheel" "networkmanager" "docker" ];
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.09";

  systemd.services.powertop = {
    description = "One-shot powertop configuration script";
    wantedBy = [ "multi-user.target" ];
    after = [ "multi-user.target" ];
    script = "/b/powertop-tune";
    serviceConfig.Type = "oneshot";
    path = [ pkgs.ethtool ];
  };

  # Allegedly overrides argument of derivation
  nixpkgs.config.packageOverrides = pkgs: rec {
    st = pkgs.st.override {
      patches = ["/k/dotfiles/nix/st-font.patch"
                 "/k/dotfiles/nix/st-term.patch"];
    };
  };

}
