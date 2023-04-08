#! /usr/bin/env nix-shell
#! nix-shell -i "csi -s" -p chicken chickenPackages.chickenEggs.srfi-19

(import srfi-19)
(write-date-literal (current-date))
