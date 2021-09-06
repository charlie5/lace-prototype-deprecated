with
     openGL.Visual;

package openGL.Terrain
--
--  Provides a constructor for heightmap terrain.
--
is

   function new_Terrain (heights_File : in asset_Name;
                         texture_File : in asset_Name    := null_Asset;
                         Scale        : in math.Vector_3 := (1.0, 1.0, 1.0)) return Visual.Grid;

end openGL.Terrain;
