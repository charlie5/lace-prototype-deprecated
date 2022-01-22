Status
======

This prototype project has been deprecated in favour of 'lace-alire'.

https://github.com/charlie5/lace-alire



Lace
====

- Provides a set of general Ada components intended to ease the development of game, simulation and GUI Ada applications.
- Components range from low-level ('events' and 'math') to high-level ('game engine').
- Potential applications areas include: 3d simulations, games, visualisations and editors.
- Supports distributed applications (DSA - see Annex E of the Ada Language Reference Manual).
- Portable across desktop and embedded platforms.
- Portable across OS platforms.
- Requires an Ada12 compiler.
- Requires an implementation of the Ada POSIX API (i.e. FLORIST).
- Several additional components are in a private prototype repository.
- Main idea is to tidy/add each private component, in turn, beginning with the base tier.

Content
=======

   - Components are organised into dependency tiers.
   - Each component in a tier can only depend on components in lower tiers.

   - Diagram: 'lace/document/components/lace-components.png'.

|Tier    |Component  |Description                                                      |
|--------|-----------|-----------------------------------------------------------------|
|5 ~ Top |User Applet|The user application.                                            |
|4 ~ High|gel        |Game Engine Library.                                             |
|3 ~ Mid |opengl     |OpenGL rendering support (2D/3D).                                |
|        |physics    |Physics space/dynamics support (2D/3D).                          |
|2 ~ Low |collada    |Provides a Collada parser.                                       |
|1 ~ Base|lace       |Provides core types and a namespace for the Lace package family. |
|        |lace/events|Provides an event mechanism for event-driven applets.            |
|        |math       |Provides core math functionality.                                |
|        |xml        |Provides a simple XML parser.                                    |


Lace/Events additionally:
- Provides Subject/Observer and Event/Response abstractions.
- Is task safe.
- Supports DSA.
- See  http://en.wikipedia.org/wiki/Event-driven_architecture
- and  http://en.wikipedia.org/wiki/Event-driven_programming
   

Installation
============
The development packages for the following projects need to be installed on your OS.

- Bullet3d
- Florist
- Freetype
- Expat
- SDL

Example for Debian/Ubuntu:

```
apt-get install libbullet-dev libflorist2016-dev libfreetype6-dev libexpat1-dev libsdl2-dev
```

Adding the following lines to ~/.bashrc (or equivalent) will set the GPR_PROJECT_PATH for all gnat project files:

```bash
export opengl_profile=desk
export opengl_platform=glx
export restrictions=xgc
export OS=Linux
export FLORIST_BUILD=default

export LACE=/path/to/lace
source $LACE/lace-gpr_paths.sh
```

Of course, substitute  /path/to  with the actual paths you use.

This should allow any Lace component to be 'with'ed in a user applications gnat project file.


Lace/openGL contains a set of assets (fonts, shaders, etc). These need to be available in each openGL demo folder.

- `$ cd $LACE/3-mid/opengl/applet`
- `$ sudo cp create_opengl_assets.sh /usr/local/bin`


Lace/mmi contains a set of assets (fonts, etc). These need to be available in each mmi demo folder.

- `$ cd $LACE/4-high/gel/applet`
- `$ sudo cp create_gel_assets.sh /usr/local/bin`


Alire
=====
   The following crates are available:

Libraries:
- lace
- lace_box2d
- lace_bullet
- lace_collada
- lace_gel
- lace_math
- lace_opengl
- lace_physics
- lace_shared
- lace_swig
- lace_xml

Demos:

- lace_gel_animation_demo
- lace_gel_full_demo


Testing
=======

* `$ cd $LACE/4-high/gel/applet/demo/sprite/mixed_shapes`
* `$ create_opengl_assets.sh`
* `$ create_gel_assets.sh`
* `$ gprbuild -P mixed_shapes.gpr`
* `$ ./launch_mixed_shapes`

... or ...

* `$ cd $LACE/4-high/gel/applet/demo/skinning/rig/human_rig`
* `$ create_opengl_assets.sh`
* `$ create_gel_assets.sh`
* `$ gprbuild -P human_rig_demo.gpr`
* `$ ./launch_human_rig_demo golfer`
