~~~~
Lace
~~~~

   - Provides a set of general Ada components intended to ease the development of game, sim and GUI Ada applications.
   
   - Components range from low-level 'events' and 'math' to high-level 'user interface' (ala GTK and QT).
   - Potential applications areas include: 3d simulations, games, visualisations and editors.

   - Supports distributed applications (DSA - see Annex E of the Ada Language Reference Manual).

   - Portable across desktop and embedded platforms.
   - Portable across X11 platforms (with ability to slot in Wayland/Windows/OS2 support, at need).

   - Requires an Ada12 compiler (ie GCC4.8+ with gnat).
   - Requires an implementation of the Ada POSIX API (i.e. FLORIST).


~~~~~~~
Content
~~~~~~~

   - Components are organised into dependendy tiers (ie layers).
   - Each component in a tier can *only* depend on components in lower tiers.
   - The result is that components are naturally organised into Base, Low, Mid, High And Top tiers.

   - Diagram: 'lace/document/components/lace-components.png'.

   ~~~~~~~~~~~~
   Tier 5 ~ Top
   ~~~~~~~~~~~~
      'User Applet'       ~ The user application.

   ~~~~~~~~~~~~~
   Tier 4 ~ High
   ~~~~~~~~~~~~~
      'mmi'               ~ Man Machine Interface with OpenGL based rendering.

  ~~~~~~~~~~~~~
   Tier 3 ~ Mid
   ~~~~~~~~~~~~
      'media'             ~ Audio and video support.
      
      'speech'            ~ Synthesis and recognition support.

   ~~~~~~~~~~~~
   Tier 2 ~ Low
   ~~~~~~~~~~~~
      'graphics/opengl'   : - OpenGL rendering support (2d/3d).
      
      'physics/impact'    : - Physics space and dynamics support (2d/3d).

   ~~~~~~~~~~~~~
   Tier 1 ~ Base
   ~~~~~~~~~~~~~
      'lace'              : - Provides low level events and other core types/operations.
    
      'lace/events'       : - Provides an event mechansism for event-driven architectures.
                            - Contains Subject, Observer, Event and Response abstractions.
                            - Supports DSA.
                            - See  http://en.wikipedia.org/wiki/Event-driven_architecture
                            -      http://en.wikipedia.org/wiki/Event-driven_programming
    
      'lace/math'         : - Provides core math functionality.
    
    
   
~~~~~~~~~~~~
Installation
~~~~~~~~~~~~

   ~~~~~~~~
   Building
   ~~~~~~~~

      In the top-level lace-stable folder:
   
      $ ./configure
      $ make
   
      This will build the library and applets (demos/tests) for every component.
   
   ~~~~~
   Using 
   ~~~~~

      Adding the following lines to ~/.bashrc (or equivalent) will set the ADA_PROJECT_PATH for all lace gnat project files:

            LACE=/path/to/lace
            source $LACE/lace-gpr_paths.sh

      This allows any Lace component to be 'with'ed in a user applications gnat project file.
