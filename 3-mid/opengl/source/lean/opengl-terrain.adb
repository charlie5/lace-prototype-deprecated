with
     openGL.Model.Terrain,
     openGL.IO,

     ada.unchecked_Deallocation,
     ada.unchecked_Conversion;

package body openGL.Terrain
is
   type Heightmap_view is access all height_Map;
   type Heightmap_Grid is array (math.Index range <>,
                                 math.Index range <>) of Heightmap_view;


   function Width (Self : in openGL.height_Map) return math.Real
   is
   begin
      return math.Real (Self'Length (2) - 1);
   end Width;


   function Depth (Self : in openGL.height_Map) return math.Real
   is
   begin
      return math.Real (Self'Length (1) - 1);
   end Depth;



   function new_Terrain (heights_File : in asset_Name;
                         texture_File : in asset_Name    := null_Asset;
                         Scale        : in math.Vector_3 := (1.0, 1.0, 1.0)) return Visual.Grid
   is
      the_Pixels  : openGL.IO.height_Map_view := IO.to_height_Map (heights_File);

      Tile_Width  : constant Positive := 8 * 32 - 1;
      Tile_Depth  : constant Positive := 8 * 32 - 1;

      total_Width : constant Real     := Real (the_Pixels'Length (2) - 1) * Scale (1);
      total_Depth : constant Real     := Real (the_Pixels'Length (1) - 1) * Scale (3);

      base_Centre : constant Vector_3 := math.Origin_3D;


      function Grid_last (total_Size, tile_Size : in Positive) return math.Index
      is
         Last : constant math.Index := math.Index (  1
                                                   + (total_Size - 1) / tile_Size);
      begin
         return Last;
      end Grid_last;


      the_Heightmap_Grid : Heightmap_Grid (1  ..  Grid_last (the_Pixels'Length (1), Tile_Depth),
                                           1  ..  Grid_last (the_Pixels'Length (2), Tile_Width));

      the_Visual_Grid    : Visual.Grid (the_Heightmap_Grid'Range (1),
                                        the_Heightmap_Grid'Range (2));
   begin
      --  Create each grid elements 'heightmap'.
      --
      declare
         row_First, row_Last,
         col_First, col_Last : math.Index;     -- Row and col ranges for each sub-matrix.
      begin
         for Row in the_Visual_Grid'Range (1)
         loop
            row_First := math.Index (Tile_Depth - 1) * (Row - 1) + 1;
            row_Last  := math.Index'Min (row_First + math.Index (Tile_Depth - 1),
                                         math.Index (the_Pixels'Last (1)));

            for Col in the_Visual_Grid'Range (2)
            loop
               col_First := math.Index (Tile_Width - 1) * (Col - 1) + 1;
               col_Last  := math.Index'Min (col_First + math.Index (Tile_Width - 1),
                                            math.Index (the_Pixels'Last (2)));

               the_Heightmap_Grid (Row, Col)
                 := new height_Map' (Region (the_Pixels.all, (Index_t (row_First), Index_t (row_Last)),
                                                             (Index_t (col_First), Index_t (col_Last))));
            end loop;
         end loop;
      end;

      --  Create the Visual for each grid element.
      --
      declare
         site_X_Offset : Real;
         site_Z_Offset : Real := Real (Tile_Depth) / 2.0 * Scale (3);
         site_Y_Offset : Real;

         tile_X_Offset : Real := 0.0;
         tile_Z_Offset : Real := total_Depth;

         tile_X_Scale  : Real;
         tile_Z_Scale  : Real;

      begin
         for Row in the_Visual_Grid'Range (1)
         loop
            site_X_Offset := Real (Tile_Width) / 2.0 * Scale (1);

            tile_X_Offset := 0.0;
            tile_Z_Offset := Real (Row - 1) * Depth (the_Heightmap_Grid (Row, 1).all) * Scale (3);

            for Col in the_Visual_Grid'Range (2)
            loop
               tile_Z_Scale := Depth (the_Heightmap_Grid (Row,   1).all) / total_Depth;
               tile_X_Scale := Width (the_Heightmap_Grid (Row, Col).all) / total_Width;

               declare
                  the_Region       : constant Heightmap_view := the_Heightmap_Grid (Row, Col);

                  Tiling           : constant texture_Transform_2D
                    :=  (s => ((tile_X_Offset / total_Width) / (tile_X_Scale * Scale (1)),  tile_X_Scale * Scale (1)),
                         t => ((tile_Z_Offset / total_Depth) / (tile_Z_Scale * Scale (3)),  tile_Z_Scale * Scale (3)));

                  the_ground_Model : constant Model.Terrain.view
                    := Model.Terrain.new_Terrain (heights_Asset => heights_File,
                                                  Row           => Row,
                                                  Col           => Col,
                                                  Heights       => the_Region.all'Access,
                                                  color_Map     => texture_File,
                                                  Tiling        => Tiling);

                  the_height_Extents : constant Vector_2    :=      height_Extent (the_Region.all);
                  the_Visual         :          Visual.view renames the_Visual_Grid (Row, Col);
                  the_Site           :          Vector_3;
               begin
                  the_Visual := Visual.Forge.new_Visual (Model      => the_ground_Model.all'Access,
                                                         Scale      => Scale,
                                                         is_Terrain => True);

                  site_Y_Offset :=    the_height_Extents (1)
                                   + (the_height_Extents (2) - the_height_Extents (1)) / 2.0;

                  the_Site := (  site_X_Offset - (total_Width / 2.0),
                                 site_Y_Offset * Scale (2),
                               -(site_Z_Offset - (total_Depth / 2.0)));


                  the_Visual_Grid (Row, Col).Site_is (the_Site + base_Centre);

                  tile_X_Offset := tile_X_Offset + Width (the_Heightmap_Grid (Row, Col).all) * Scale (1);

                  if Col /= the_Visual_Grid'Last (2)
                  then
                     site_X_Offset := site_X_Offset
                                    + Width (the_Heightmap_Grid (Row, Col    ).all) * Scale (1) / 2.0
                                    + Width (the_Heightmap_Grid (Row, Col + 1).all) * Scale (1) / 2.0;
                  end if;
               end;
            end loop;

            if Row /= the_Visual_Grid'Last (1)
            then
               site_Z_Offset := site_Z_Offset + Depth (the_Heightmap_Grid (Row,     1).all) * Scale (3) / 2.0
                                              + Depth (the_Heightmap_Grid (Row + 1, 1).all) * Scale (3) / 2.0;
            end if;
         end loop;
      end;

      declare
         procedure free is new ada.unchecked_Deallocation (   height_Map,
                                                           IO.height_Map_view);
      begin
         free (the_Pixels);
      end;

      return the_Visual_Grid;
   end new_Terrain;


end openGL.Terrain;
