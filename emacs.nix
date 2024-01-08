{pkgs, ...}@inputs:
let
  files = [./private ./emacs.el];
in
{
  # emacs29 baby
  package = pkgs.emacs29;
  enable = true;
  extraPackages = epkgs: with epkgs; [
    magit
    org
    org-roam
    evil
    evil-collection
    use-package

    lsp-mode
    lsp-haskell
    dap-mode
    nix-mode
    rust-mode
    cmake-mode
    haskell-mode
    scala-mode

    vterm
    projectile

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
    pdf-tools
    kotlin-mode
    unicode-math-input
    bazel
  ];
  # overrides = (self: super: rec {
  #   haskell-mode = self.melpaPackages.haskell-mode;
  # });

  extraConfig = builtins.readFile ./emacs.el;
}
