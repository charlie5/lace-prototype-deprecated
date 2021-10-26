with
     physics.Model,
     Physics,

     openGL.Model.terrain,
     openGL.IO,

     ada.unchecked_Deallocation,
     ada.unchecked_Conversion;

package body gel.Terrain
is
   type Heightfield_view is access all physics.Heightfield;

   type height_Map_view is access all opengl.height_Map;
   type height_map_Grid is array (math.Index range <>,
                                  math.Index range <>) of height_Map_view;


   function Width (Self : in opengl.height_Map) return math.Real
   is
   begin
      return math.Real (self'Length (2) - 1);
   end Width;


   function Depth (Self : in opengl.height_Map) return math.Real
   is
   begin
      return math.Real (self'Length (1) - 1);
   end Depth;



   function new_Terrain (World        : in gel.World.view;
                         heights_File : in String;
                         texture_File : in String        := "";
                         Scale        : in math.Vector_3 := (1.0, 1.0, 1.0)) return access gel.Sprite.Grid
   is
      use Math;

      the_Pixels  : opengl.IO.height_Map_view := opengl.IO.to_height_Map (openGL.to_Asset (heights_File));

      tile_Width  : constant Positive := 8 * 32 - 1;
      tile_Depth  : constant Positive := 8 * 32 - 1;

      total_Width : constant Real     := Real (the_Pixels'Length (2) - 1) * Scale (1);
      total_Depth : constant Real     := Real (the_Pixels'Length (1) - 1) * Scale (3);

      base_Centre : constant Vector_3 := (0.0, 0.0, 0.0);


      function Grid_last (total_Size, tile_Size : in Positive) return math.Index
      is
         Last : constant math.Index := math.Index (  1
                                                   + (total_Size - 1) / tile_Size);
      begin
         return Last;
      end Grid_last;


      the_heightmap_Grid : height_map_Grid (1  ..  Grid_last (the_Pixels'Length (1), tile_Depth),
                                            1  ..  Grid_last (the_Pixels'Length (2), tile_Width));

      the_Sprite_Grid    : constant gel.Sprite.Grid_view := new gel.Sprite.Grid (the_heightmap_Grid'Range (1),
                                                                                 the_heightmap_Grid'Range (2));

      procedure free is new ada.unchecked_Deallocation (opengl.height_Map,
                                                        opengl.IO.height_Map_view);

      procedure flip (Self : opengl.IO.height_Map_view)
      is
         use type opengl.Index_t;

         the_Map : opengl.IO.height_Map_view := new opengl.height_Map' (Self.all);

      begin
         for Row in Self'Range (1)
         loop
            for Col in Self'Range (2)
            loop
               Self (Row, Col) := the_Map (Self'Last (1) - Row + 1,  Col);
            end loop;
         end loop;

         free (the_Map);
      end flip;


   begin
      flip (the_Pixels.all'unchecked_Access);

      --  Create each grid elements 'heightmap'.
      --
      declare
         use openGL;

         row_First, row_Last,
         col_First, col_Last : math.Index;     -- Row and col ranges for each sub-matrix.

      begin
         for Row in the_sprite_Grid'Range (1)
         loop
            row_First := math.Index (tile_Depth - 1) * (Row - 1) + 1;
            row_Last  := math.Index'Min (row_First + math.Index (tile_Depth - 1),
                                         math.Index (the_Pixels'Last (1)));

            for Col in the_sprite_Grid'Range (2)
            loop
               col_First := math.Index (tile_Width - 1) * (Col - 1) + 1;
               col_Last  := math.Index'Min (col_First + math.Index (tile_Width - 1),
                                            math.Index (the_Pixels'Last (2)));

               the_heightmap_Grid (Row, Col)
                 := new opengl.height_Map' (Region (the_Pixels.all, (Index_t (row_First), Index_t (row_Last)),
                                                                    (Index_t (col_First), Index_t (col_Last))));
            end loop;
         end loop;
      end;

      --  Create the Sprite for each grid element.
      --
      declare
         site_X_offset,
         site_Z_offset : Real := 0.0;
         site_Y_Offset : Real;

         tile_X_Offset : Real := 0.0;
         tile_Z_Offset : Real := total_Depth;

         tile_X_Scale  : Real;
         tile_Z_Scale  : Real;

      begin
         for Row in the_sprite_Grid'Range (1)
         loop
            site_X_offset := 0.0;

            tile_X_Offset := 0.0;
            tile_Z_Offset := tile_Z_Offset - Depth (the_heightmap_Grid (Row, 1).all) * Scale (3);

            for Col in the_sprite_Grid'Range (2)
            loop
               tile_Z_Scale := Depth (the_heightmap_Grid (Row,   1).all) / total_Depth;
               tile_X_Scale := Width (the_heightmap_Grid (Row, Col).all) / total_Width;

               declare
                  the_Region       : constant height_Map_view := the_heightmap_Grid (Row, Col);
                  the_height_Range : constant opengl.Vector_2 := openGL.height_Extent (the_Region.all);

                  Tiling : constant opengl.texture_Transform_2d
                    :=  (S => (opengl.Real (tile_X_Offset / total_Width) / opengl.Real (tile_X_Scale * Scale (1)),
                               opengl.Real (tile_X_Scale * Scale (1))),
                         T => (opengl.Real (tile_Z_Offset / total_Depth) / opengl.Real (tile_Z_Scale * Scale (3)),
                               opengl.Real (tile_Z_Scale * Scale (3))));

                  the_ground_Model : constant access openGL.Model.terrain.item
                    := openGL.Model.terrain.new_Terrain (heights_Asset => openGL.to_Asset (heights_File),
                                                         Row           => Row,
                                                         Col           => Col,
                                                         Heights       => the_Region.all'Access,
                                                         color_Map     => openGL.to_Asset (texture_File),
                                                         Tiling        => Tiling);

                  function to_Physics is new ada.unchecked_Conversion (height_Map_view,
                                                                       Heightfield_view);

                  the_ground_physics_Model : constant physics.Model.view
                    := new physics.Model.item' (Id          => physics.null_model_Id,
                                                --  Site        => Origin_3d,
                                                Scale       => Scale,
                                                shape_Info  => (physics.Model.Heightfield,
                                                                Heights      => to_Physics (the_Region),
                                                                height_range => (the_height_Range (1),
                                                                                 the_height_Range (2))),
                                                Shape       => null,
                                                Mass        => 0.0,
                                                Friction    => 0.5,
                                                Restitution => 0.5,
                                                is_Tangible => True);

                  the_height_Extents : constant opengl.Vector_2 := opengl.height_Extent (the_Region.all);
                  the_Sprite         : gel.Sprite.view     renames the_sprite_Grid (Row, Col);
                  the_Site           : vector_3;
               begin
                  --  the_ground_Model.Scale := (Scale (1),
                  --                             Scale (2),
                  --                             Scale (3));

                  the_Site := (0.0, 0.0, 0.0);

                  the_Sprite := gel.Sprite.Forge.new_Sprite ("Terrain" & Row'Image & Col'Image,
                                                             sprite.World_view (World),
                                                             the_Site,
                                                             the_ground_Model,
                                                             the_ground_physics_Model,
                                                             owns_Graphics => True,
                                                             owns_Physics  => True);

                  site_y_Offset := math.Real (  the_height_Extents (1)
                                              + (the_height_Extents (2) - the_height_Extents (1)) / 2.0);

                  --  the_sprite_Grid (Row, Col).Site_is (the_Site + base_Centre);
                  the_Sprite. Site_is (the_Site + base_Centre);
                  the_Sprite.Scale_is (Scale);

                  tile_X_Offset := tile_X_Offset + Width (the_heightmap_Grid (Row, Col).all) * Scale (1);

                  if Col /= the_sprite_Grid'Last (2)
                  then
                     site_X_offset := site_X_offset
                                    + Width (the_heightmap_Grid (Row, Col    ).all) * Scale (1) / 2.0
                                    + Width (the_heightmap_Grid (Row, Col + 1).all) * Scale (1) / 2.0;
                  end if;
               end;
            end loop;

            if Row /= the_sprite_Grid'Last (1)
            then
               site_Z_offset := site_Z_offset + Depth (the_heightmap_Grid (Row,     1).all) * Scale (3) / 2.0
                                              + Depth (the_heightmap_Grid (Row + 1, 1).all) * Scale (3) / 2.0;
            end if;
         end loop;
      end;

      free (the_Pixels);

      return the_Sprite_Grid;
   end new_Terrain;


end gel.Terrain;
