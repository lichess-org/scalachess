{
  pkgs,
  lib,
  config,
  inputs,
  ...
}:

{
  languages = {
    scala = {
      enable = true;
      sbt.enable = true;
    };
  };
}
