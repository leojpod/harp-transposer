# This defines a function taking `pkgs` as parameter, and uses
# `nixpkgs` by default if no argument is passed to it.
{ pkgs ? import <nixpkgs> {
  config.allowUnfree = true;
} }:

with pkgs;

mkShell {
  # Sets the build inputs, i.e. what will be available in our
  # local environment.
  buildInputs = [
    elmPackages.elm
    elmPackages.elm-format
    elmPackages.elm-json
    elmPackages.elm-test
    elmPackages.elm-review
    nodejs-18_x
  ] ++ (if pkgs.system == "x86_64-darwin" then [] else [inotify-tools]);

  shellHook = ''
    export PATH="${toString ./node_modules/.bin}:$HOME/.npm-global/bin:$PATH"
    '';
}

