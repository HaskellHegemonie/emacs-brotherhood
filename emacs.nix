{pkgs, ...}@inputs:
let
  files = [./private ./emacs.el];
in
{
  # emacs29 baby
  package = pkgs.emacs29;
  enable = true;
  extraPackages = epkgs: with epkgs; [
    # git
    magit
    forge
    git-timemachine
    diff-hl

    org
    org-roam
    evil
    evil-collection
    use-package

    # lsp-haskell
    dap-mode
    nix-mode
    rust-mode
    cmake-mode
    haskell-mode
    proof-general
    agda2-mode
    popper
    # god-mode
    # xah-fly-keys

    vterm
    # projectile

    # search engine
    vertico
    marginalia
    orderless
    consult

    doom-themes
    doom-modeline
    # from doom
    hl-todo
    # modeline
    direnv
    rainbow-delimiters
    all-the-icons

    keychain-environment

    ement

    pdf-tools
    unicode-math-input
    auctex
    cdlatex
    eimp
    rg

    # https://nixos.wiki/wiki/Julia says plots don't work within nixos :(
    julia-mode
    julia-repl
    julia-vterm
    mu4e
  ];
  # overrides = (self: super: rec {
  #   haskell-mode = self.melpaPackages.haskell-mode;
  # });

  extraConfig = builtins.readFile ./emacs.el;
}
