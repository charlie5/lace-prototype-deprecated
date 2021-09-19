with
     gel.Sprite,
     gel.World;

package gel.Terrain
--
--  Provides a constructor for heightmap terrain.
--
is

   function new_Terrain (World        : in gel.World.view;
                         heights_File : in String;
                         texture_File : in String        := "";
                         Scale        : in math.Vector_3 := (1.0, 1.0, 1.0)) return access gel.Sprite.Grid;

end gel.Terrain;
