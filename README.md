# dunGen
dunGen is a Haskell command-line tool for generating random grid-based dungeon maps for D&D and other tabletop roleplaying games. Choose the size of your map and your desired density, and dunGen will render an image for you.

![Example dungeon](images/generated/example.bmp "Example dungeon")

# Building
To build and run dunGen, you'll need [Stack](https://docs.haskellstack.org/en/stable/README/).

Build with `stack build`. Run with `stack run`. dunGen currently looks in the current directory for the dungeon tile sprites, so run the program from the `images` directory if you are using the default tiles.

As for parameters I'd suggest using
```
Grid Width: 40
Grid Height: 40
Density: 30
```
as a starting point.
