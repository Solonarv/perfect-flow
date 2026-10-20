# To do / roadmap

## Improve rendering

 - Font rendering is as stupidly straightforward, and therefore bad, as possible. Make a sprite atlas? Store pre-rendered text sprites per renderable thingy?
 - display castable skills as icons instead of bars - (need sprites; there are open game sprites, cf. orb of creation)
 - display cooldowns on skill icon/bar
 - display keybind on skill icon/bar

### Minor cosmetic fixes

 - different colors for resource bars
 - make/find an icon for the executable / main window

## Level data

 - More example levels in Haskell
 - separate level description from world state?
   - if yes, "hydration" code for level description -> world state
 - lua interface so levels can be safely loaded from untrusted code (mod support!) and without recompiling 

## Installation / cross-platform / usability

 - Stop hardcoding font to load (probably bundle a font instead)
 - create installer (using self-extract?) - DONE! (itch.io, no installer needed)
   - compile for other platforms
 - allow startup without commandline arguments, prompt user for level to load - 
 - configuration / user settings
    - custom key binds
        - additional layer of abstraction over key binds?
          - instead of key -> cast: skill, do key -> cast skill #x and assign each usable skill a generic index
 
## Not feature creep I swear

 - implement more Actions
 - add modifier system; rewrite damage, cost etc. calculations to look through modifiers
