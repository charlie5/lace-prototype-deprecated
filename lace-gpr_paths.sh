GPR_PROJECT_PATH=$LACE/0-floor/lace_shared:$GPR_PROJECT_PATH

GPR_PROJECT_PATH=$LACE/1-base/lace/library:$GPR_PROJECT_PATH
GPR_PROJECT_PATH=$LACE/1-base/math/library:$GPR_PROJECT_PATH
GPR_PROJECT_PATH=$LACE/1-base/swig/library:$GPR_PROJECT_PATH
GPR_PROJECT_PATH=$LACE/1-base/xml/library:$GPR_PROJECT_PATH


GPR_PROJECT_PATH=$LACE/2-low/collada/library:$GPR_PROJECT_PATH
GPR_PROJECT_PATH=$LACE/2-low/neural/library:$GPR_PROJECT_PATH


GPR_PROJECT_PATH=$LACE/3-mid/opengl/library:$GPR_PROJECT_PATH

GPR_PROJECT_PATH=$LACE/3-mid/physics/interface/library:$GPR_PROJECT_PATH
GPR_PROJECT_PATH=$LACE/3-mid/physics/implement/c_math/library:$GPR_PROJECT_PATH
GPR_PROJECT_PATH=$LACE/3-mid/physics/implement/bullet/library:$GPR_PROJECT_PATH
GPR_PROJECT_PATH=$LACE/3-mid/physics/implement/box2d/contrib:$GPR_PROJECT_PATH
GPR_PROJECT_PATH=$LACE/3-mid/physics/implement/box2d/library:$GPR_PROJECT_PATH
GPR_PROJECT_PATH=$LACE/3-mid/physics/implement/box2d/contrib/box2d-2.4.1:$GPR_PROJECT_PATH
GPR_PROJECT_PATH=$LACE/3-mid/physics/implement/impact/library:$GPR_PROJECT_PATH


GPR_PROJECT_PATH=$LACE/4-high/gel/library:$GPR_PROJECT_PATH
GPR_PROJECT_PATH=$LACE/4-high/gel/library/sdl:$GPR_PROJECT_PATH


export GPR_PROJECT_PATH
