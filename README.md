# perfect-flow
A kind of rhythm game disguised as WoW-style combat.

## How to install

At the moment, Perfect Flow can only be built from source.

### Windows

Download the [SDL2 binaries](https://www.libsdl.org/download-2.0.php) as well as the [SDL2_ttf binaries](https://www.libsdl.org/projects/SDL_ttf/).
You want the zip file from the runtime binaries / windows section.

Extract the DLLs from the zip files and place them somewhere on your `%PATH%`.

Install [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

Clone this repository with `git clone https://github.com/Solonarv/perfect-flow`, or download the [ZIP archive](https://github.com/Solonarv/perfect-flow/archive/master.zip).

Open a terminal and navigate to the directory where you extracted the archive.

Run the following commands:

    stack setup
    stack install

This will set up the Haskell compiler toolchain (warning: takes a while!), and then compile the project.

To run the game, simply run the following command:

    perfect-flow <level-file>

where `<level-file>` is the path to a YAML file like those in `data/`.