# Rvm

I wanted to write a jvm so here's the beginnings of one.
Currently, there's only a partially-completed class file parser, and a class file pretty-printer.

## Class File Pretty Printer:
- `pretty-class -d` - Print the class **d**efinition. Stuff like `public class Foo extend Bar` and all its members.
- `pretty-class -c` - Print the class **c**onstant pool.
- `pretty-class -C` - Show each method's **C**ode.
- `pretty-class --no-color` - Don't output any ANSI control codes.
