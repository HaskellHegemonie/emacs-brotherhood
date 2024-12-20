{pkgs, ...}@inputs:
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
    nixos-options
    rust-mode
    cmake-mode
    haskell-mode
    rust-mode
    gnu-apl-mode
    purescript-mode
    psc-ide
    nim-mode
    racket-mode
    paredit
    proof-general
    popper
    # god-mode
    # xah-fly-keys

    vterm
    multi-vterm
    # projectile

    # search engine
    vertico
    marginalia
    orderless
    consult
    corfu
    sly
    sly-asdf

    racket-mode
    quack
    guix
    geiser
    geiser-racket
    geiser-guile

    doom-themes
    doom-modeline
    # from doom
    hl-todo
    # modeline
    direnv
    rainbow-delimiters
    all-the-icons
    haskell-emacs

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
