with
     openGL.Visual;


package openGL.Terrain
--
--  Provides a constructor for heightmap terrain.
--
is

   function new_Terrain (heights_File : in     String;
                         texture_File : in     String        := "";
                         Scale        : in     math.Vector_3 := (1.0, 1.0, 1.0)) return openGL.Visual.Grid;

end openGL.Terrain;
