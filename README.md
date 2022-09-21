# Furball Cat's Game Engine
This is a source code of Furball Cat's Game Engine personal project.
The goal is to build game engine in C (with little to no C++) for third-person perspective action adventure games.
It will probably take forever. But who cares. It's fun!

## Design choices
### Simplicity of C11
Even though C++ is a great language and sometimes I miss array_view\<type\> kind of templates or virtual methods,
the choice no #1 is to make it as simple as possible, without the boilerplate of C++. This allows to see when virtual methods or allocation is actually required.

### Init by zero
Every structure should strive to be default initialised by zeroing out.
This is because C doesn't have constructors, so it's simple and easy to just memset to zero.

### No dynamic allocations
No dynamic allocations allowed at runtime (frame update). Allocation is only allowed at engine startup.
The goal is to be aware of how many bytes you need.
However, there's scratchpad buffer and arena allocators in use for temporal allocations in frame.
Both are using stack allocator approach, then at certain time the memory is reused.
Scratchpad buffer's life time is single system update. Arena memory's life time is single or double frame.
Levels are loaded into level heap, then the data stays.
Streaming is achieved through swapping sectors data on level heap.

### Minimalistic tools
Most of the pipelines like importing or packaging are commandline based (ccmd).
Scripts are written using Racket (yes, LISP style). There's no debugging to scripts. It's a program that generates data readable for engine.
Scripts are also holding the definitions for game structures or game objects.
Debug tools are simple rect + text UI that can be navigated with PlayStation Gamepad (press left stick + options to open debug menu).
World editor is achieved through Blender add-on (version 3.3.0+).

### Fast launch
Engine should take no longer than 5 seconds to launch. Level loading might take longer though.
Compilation from scratch should be under 10 minutes.
Project generation (.sln, .xcworkspace) is automated and should take no longer than 5 seconds.

### Directories
There are three main source code directories: shared, common, and game. Code can naturally flow from Game, through Common, up to Shared if it makes sense.
Shared - engine components that can be shared without any conflicts between games.
Common - game specific engine components, that are general enough that with little bit of \#ifdefs it can compile for multiple games.
Game - game specific code that cannot be reused in another game.

### Naming convention
Code uses snake_naming_convention with prefix for specific engine component: 'fr_' rendering, 'fa_' animation, 'fc_' core, etc., and postfix for types '_t'.
Since Racket is used for scripts and game data structure definitions, the naming here is skewer-style: 'player-idle-01', 'rock-01', and so on.
Each asset has to be identified by unique file name. This simplifies resource pipelines.

### No includes in headers
To speed up compilation, there are no includes in headers except for build-in types like \#include \<inttypes.h\> and \#include \<stdbool.h\> and API include \#include "api.h".
Other instances should be cleaned up. I might allow include for math types definitions though (like fm_xform).
Also, "public.h" files for projects do have includes in them, but only of specific header files of particular module that are most commonly used.
