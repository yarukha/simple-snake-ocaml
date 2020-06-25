# simple-snake-ocaml

Un modeste jeu snake réalisé en OCaml.
Il est encore incomplet.
Le but de ce projet était de découvrir les possibilités, finalement très limitées de la libraire Graphics.


### Compilation

En ayant installé les libraires Graphics et Unix, executer dans le dossier du fichier .ml, :
```
eval $(opam env)
```
puis, pour obtenir un executable:
```
ocamlc -I $(ocamlfind query unix) unix.cma -I $(ocamlfind query graphics) graphics.cma snake.ml -o snake
```
