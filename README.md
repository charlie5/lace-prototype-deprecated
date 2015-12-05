~~~~
Lace - a Work in Progress*
~~~~

   - Provides a set of general Ada components intended to ease the development of game, sim and GUI Ada applications.
   
   - Components range from low-level 'events' and 'math' to high-level 'user interface' (ala GTK and QT).
   - Potential applications areas include: 3d simulations, games, visualisations and editors.

   - Supports distributed applications (DSA - see Annex E of the Ada Language Reference Manual).

   - Portable across desktop and embedded platforms.
   - Portable across X11 platforms (with ability to slot in Wayland/Windows/OS2 support, at need).

   - Requires an Ada12 compiler (ie GCC4.8+ with gnat).
   - Requires an implementation of the Ada POSIX API (i.e. FLORIST).


* 
  - Most components mentioned are in a private prototype repository (some are more up to date than others). 
  - Main idea is to tidy/add each repo, in turn, beginning with the base tier.
  


~~~~~~~
Content
~~~~~~~

   - Components are organised into dependendy tiers (ie layers).
   - Each component in a tier can only depend on components in lower tiers.

   - Diagram: 'lace/document/components/lace-components.png'.

   ~~~~~~~~~~~~
   Tier 5 ~ Top
   ~~~~~~~~~~~~
      'User Applet'       ~ The user application.

   ~~~~~~~~~~~~~
   Tier 4 ~ High
   ~~~~~~~~~~~~~
      'mmi'               ~ Man Machine Interface (with OpenGL based rendering).

  ~~~~~~~~~~~~~
   Tier 3 ~ Mid
   ~~~~~~~~~~~~
      'media'             ~ Audio and video support.
      
      'speech'            ~ Synthesis and recognition support.

   ~~~~~~~~~~~~
   Tier 2 ~ Low
   ~~~~~~~~~~~~
      'graphics/opengl'   : - OpenGL rendering support (2d/3d).
      
      'physics/impact'    : - Physics space/dynamics support (2d/3d).

   ~~~~~~~~~~~~~
   Tier 1 ~ Base
   ~~~~~~~~~~~~~
      'lace'              : - Provides core types and a namespace fot the Lace package family.
    
      'lace/events'       : - Provides an event mechansism for event-driven architectures.
                            - Contains Subject, Observer, Event and Response abstractions.
                            - Supports DSA.
                            - See  http://en.wikipedia.org/wiki/Event-driven_architecture
                            -      http://en.wikipedia.org/wiki/Event-driven_programming
    
      'lace/math'         : - Provides core math functionality.
    
    
   
~~~~~~~~~~~~
Installation
~~~~~~~~~~~~

   The following development packages need to be installed on your OS.

   - box2d
   - bullet3d


   The cBound ada bindings project is also required:

   - $ git clone https://github.com/charlie5/cBound.git


   Adding the following lines to ~/.bashrc (or equivalent) will set the ADA_PROJECT_PATH for all gnat project files:

            export opengl_profile=desk
            export opengl_platform=glx
            export restrictions=xgc
            export OS=Linux
            export FLORIST_BUILD=default

            ADA_PROJECT_PATH=/path/to/lumen:$ADA_PROJECT_PATH

            export CBOUND=/path/to/cBound
            source $CBOUND/cbound-gpr_paths.sh

            LACE=/path/to/lace
            source $LACE/lace-gpr_paths.sh

   Of course, substitute  /path/to  with the actual paths you use.

   This should allow any Lace component to be 'with'ed in a user applications 'gnat project' file.



~~~~~~~
Testing
~~~~~~~

   $ cd $LACE/4-high/mmi/applet/demo/skinning/rig
   $ gprbuild -P rig_demo.gpr
   $ ./launch_rig_demo





