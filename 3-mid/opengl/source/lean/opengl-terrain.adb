with
     openGL.Model.terrain,
     openGL.IO,

     ada.unchecked_Deallocation,
     ada.unchecked_Conversion;


package body openGL.Terrain
is


   type height_Map_view is access all openGL.height_Map;

   type height_map_Grid is array (math.Index range <>,
                                  math.Index range <>) of height_Map_view;



   function Width  (Self : in openGL.height_Map) return math.Real
   is
   begin
      return math.Real (self'Length (2) - 1);
   end Width;


   function Depth (Self : in openGL.height_Map) return math.Real
   is
   begin
      return math.Real (self'Length (1) - 1);
   end Depth;



   function new_Terrain (heights_File : in     String;
                         texture_File : in     String        := "";
                         Scale        : in     math.Vector_3 := (1.0, 1.0, 1.0)) return openGL.Visual.Grid
   is
      use type math.Index, openGL.Real;

      the_Pixels  :          openGL.io.height_Map_view
                                      := openGL.io.to_height_Map (heights_File);

      tile_Width  : constant Positive := 8 * 32 - 1;
      tile_Depth  : constant Positive := 8 * 32 - 1;

      total_Width : constant Real     := Real (the_Pixels'Length (2) - 1) * Scale (1);
      total_Depth : constant Real     := Real (the_Pixels'Length (1) - 1) * Scale (3);

      base_Centre : constant Vector_3 := (0.0,
                                          0.0,
                                          0.0);

      function Grid_last (total_Size, tile_Size : in Positive) return math.Index
      is
         Last : constant math.Index := math.Index (  1
                                                   + (total_Size - 1) / tile_Size);
      begin
         return Last;
      end Grid_last;


      the_heightmap_Grid : height_map_Grid (1  ..  Grid_last (the_Pixels'Length (1), tile_Depth),
                                            1  ..  Grid_last (the_Pixels'Length (2), tile_Width));

      the_Visual_Grid    : openGL.Visual.Grid (the_heightmap_Grid'Range (1),
                                               the_heightmap_Grid'Range (2));

      procedure free is new ada.unchecked_Deallocation (openGL.height_Map, openGL.IO.height_Map_view);


      procedure flip (Self : openGL.io.height_Map_view)
      is
         use type openGL.Index_t;
         Pad : openGL.io.height_Map_view := new openGL.height_Map' (Self.all);

      begin
         for Row in Self'Range (1)
         loop
            for Col in Self'Range (2)
            loop
               Self (Row, Col) := Pad (Self'Last (1) - Row + 1,  Col);
            end loop;
         end loop;

         free (Pad);
      end flip;


   begin
      flip (the_Pixels.all'Unchecked_Access);

      --  Create each grid elements 'heightmap'.
      --
      declare
         use openGL;

         row_First, row_Last,
         col_First, col_Last  : math.Index;   -- Row and col ranges for each sub-matrix.
      begin
         for Row in the_visual_Grid'Range (1)
         loop
            row_First := math.Index (tile_Depth - 1) * (Row - 1) + 1;
            row_Last  := math.Index'Min (row_First + math.Index (tile_Depth - 1),
                                         math.Index (the_Pixels'Last (1)));

            for Col in the_visual_Grid'Range (2)
            loop
               col_First := math.Index (tile_Width - 1) * (Col - 1) + 1;
               col_Last  := math.Index'Min (col_First + math.Index (tile_Width - 1),
                                            math.Index (the_Pixels'Last (2)));

               the_heightmap_Grid (Row, Col)
                 := new openGL.height_Map' (Region (the_Pixels.all, (Index_t (row_First), Index_t (row_Last)),
                                                                    (Index_t (col_First), Index_t (col_Last))));
            end loop;
         end loop;
      end;

      --  Create the Visual for each grid element.
      --
      declare
         site_X_offset : Real;
         site_Z_offset : Real := Real (tile_Depth) / 2.0 * Scale (3);
         site_Y_Offset : Real;

         tile_X_Offset : Real := 0.0;
         tile_Z_Offset : Real := total_Depth;

         tile_X_Scale  : Real;
         tile_Z_Scale  : Real;

      begin
         for Row in the_visual_Grid'Range (1)
         loop
            site_X_offset := Real (tile_Width) / 2.0 * Scale (1);

            tile_X_Offset := 0.0;
            tile_Z_Offset := tile_Z_Offset - Depth (the_heightmap_Grid (Row, 1).all) * Scale (3);

            for Col in the_visual_Grid'Range (2)
            loop
               tile_Z_Scale := Depth (the_heightmap_Grid (Row,   1).all) / total_Depth;
               tile_X_Scale := Width (the_heightmap_Grid (Row, Col).all) / total_Width;

               declare
                  use math.Vectors;
                  the_Region       : constant height_Map_view := the_heightmap_Grid (Row, Col);

                  Tiling           : constant openGL.texture_Transform_2d
                    :=  (s => (openGL.Real (tile_X_Offset / total_Width) / openGL.Real (tile_X_Scale * Scale (1)),
                               openGL.Real (tile_X_Scale * Scale (1))),
                         t => (openGL.Real (tile_Z_Offset / total_Depth) / openGL.Real (tile_Z_Scale * Scale (3)),
                               openGL.Real (tile_Z_Scale * Scale (3))));

                  the_ground_Model : constant openGL.Model.terrain.view
                    := openGL.Model.terrain.Forge.new_Item (heights_asset => openGL.to_Asset (heights_File),
                                                            row           => Row,
                                                            col           => Col,
                                                            heights       => the_Region.all'Access,
                                                            color_map     => openGL.to_Asset (texture_File),
                                                            tiling        => Tiling);

                  the_height_Extents : openGL.Vector_2    :=      openGL.height_Extent (the_Region.all);
                  the_Visual         : openGL.Visual.view renames the_visual_Grid      (Row, Col);
                  the_Site           : vector_3;
               begin
                  the_ground_Model.Scale := (Scale (1),
                                             Scale (2),
                                             Scale (3));

                  the_Visual := openGL.Visual.Forge.new_Visual (the_ground_Model.all'Access,
                                                                (1.0, 1.0, 1.0),
                                                                is_Terrain => True);

                  site_y_Offset := math.Real (  the_height_Extents (1)
                                              + (the_height_Extents (2) - the_height_Extents (1)) / 2.0);

                  the_Site := (site_X_offset - (total_Width / 2.0) * 1.0,
                               site_Y_Offset * Scale (2),
                               site_Z_offset - (total_Depth / 2.0) * 1.0);


                  the_visual_Grid (Row, Col).Site_is (the_Site + base_Centre);

                  tile_X_Offset := tile_X_Offset + Width (the_heightmap_Grid (Row, Col).all) * Scale (1);

                  if Col /= the_visual_Grid'Last (2)
                  then
                     site_X_offset := site_X_offset
                                    + Width (the_heightmap_Grid (Row, Col    ).all) * Scale (1) / 2.0
                                    + Width (the_heightmap_Grid (Row, Col + 1).all) * Scale (1) / 2.0;
                  end if;
               end;
            end loop;

            if Row /= the_visual_Grid'Last (1)
            then
               site_Z_offset := site_Z_offset + Depth (the_heightmap_Grid (Row,     1).all) * Scale (3) / 2.0
                                              + Depth (the_heightmap_Grid (Row + 1, 1).all) * Scale (3) / 2.0;
            end if;
         end loop;
      end;

      free (the_Pixels);

      return the_Visual_Grid;
   end new_Terrain;


end openGL.Terrain;
