with
     mmi.Sprite,
     mmi.World;


package mmi.Terrain
--
--  Provides a constructor for heightmap terrain.
--
is

   function new_Terrain (World        : access mmi.World.item'Class;
                         heights_File : in     String;
                         texture_File : in     String        := "";
                         Scale        : in     math.Vector_3 := (1.0, 1.0, 1.0)) return access mmi.Sprite.Grid;

end mmi.Terrain;
