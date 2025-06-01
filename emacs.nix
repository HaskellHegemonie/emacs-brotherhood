{pkgs, ...}@inputs:
{
  package = pkgs.emacs;
  enable = true;
  extraPackages = epkgs: with epkgs;
    let
      essentials = [
        # projectile
        org
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
        embark
        embark-consult
        corfu
        pdf-tools
        unicode-math-input
        rg
        eimp
        popper
        doom-themes
        keychain-environment
        vterm
        multi-vterm
        hl-todo
        ement
        ledger-mode
        org-contrib
        emms
        osm
      ];
      latex = [
        auctex
        cdlatex
      ];
      typst = [
        typst-ts-mode
        ox-typst
        tree-sitter-langs
      ];
      languages = [
        # lsp-haskell
        dap-mode
        nix-mode
        # nixos-options
        rust-mode
        rustic
        cargo
        haskell-mode
        julia-mode
        # julia-snail
        julia-repl
        gnu-apl-mode
        
        unison
        unison-mode
        unisonlang-mode

        sly
        sly-asdf

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
      builtins.concatLists [ essentials latex typst languages gitPackages ];
  extraConfig = builtins.readFile ./emacs.el;
}
