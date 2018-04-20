(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    common = ./common;
    server = ./server;
    client = ./client;
  };

  shells = {
    ghc = ["common" "server" "client"];
    ghcjs = ["common" "client"];
  };

  android.client = {
    executableName = "client";
    applicationId = "prikhi.github.analytics.client";
    displayName = "Github Analytics";
  };

  ios.client = {
    executableName = "client";
    bundleIdentifier = "prikhi.github.analytics.client";
    bundleName = "Github Analytics";
  };
})
