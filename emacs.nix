{pkgs, ...}@inputs:
{
  package = pkgs.emacs;
  enable = true;
  extraPackages = epkgs: with epkgs;
    let
      essentials = [
        # projectile
        org
        org-roam
        evil
        evil-collection
        use-package
        direnv
        rainbow-delimiters
        all-the-icons
        paredit
        vertico
        marginalia
        orderless
        consult
        corfu
        pdf-tools
        unicode-math-input
        rg
        eimp
        mu4e
        popper
        doom-themes
        doom-modeline
        keychain-environment
        vterm
        multi-vterm
        hl-todo
        ement
      ];
      latex = [
        auctex
        cdlatex
      ];
      languages = [
        # lsp-haskell
        dap-mode
        nix-mode
        # nixos-options
        rust-mode
        # cmake-mode
        haskell-mode
        rust-mode
        gnu-apl-mode
        # purescript-mode
        # psc-ide
        # nim-mode
        # racket-mode
        
        unison
        unison-mode
        unisonlang-mode

        proof-general

        sly
        sly-asdf

        guix
        geiser
        geiser-racket
        geiser-guile
        haskell-emacs
      ];
      gitPackages = [
        magit
        forge
        git-timemachine
        diff-hl
      ];
    in
      builtins.concatLists [ essentials latex languages gitPackages ];
  extraConfig = builtins.readFile ./emacs.el;
}
