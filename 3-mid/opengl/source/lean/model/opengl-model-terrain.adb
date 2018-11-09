with
     openGL.Geometry.lit_colored_textured,
     openGL.Primitive.indexed,
     openGL.Palette,
     openGL.Texture.Coordinates,
     openGL.IO,

     ada.unchecked_Deallocation;


package body openGL.Model.terrain
is
   use openGL.Texture;


   --------
   -- Forge
   --

   function new_Item (heights_Asset : in asset_Name;
                      Row, Col      : in Integer;
                      Heights       : in height_Map_view;
                      color_Map     : in asset_Name;
                      Tiling        : in openGL.texture_Transform_2d := (s => (0.0, 1.0),
                                                                         t => (0.0, 1.0))) return View
   is
      the_Model : constant View := new Item' (openGL.Model.item with
                                              heights_Asset => heights_Asset,
                                              Heights       => Heights,
                                              Row           => Row,
                                              Col           => Col,
                                              color_Map     => color_Map,
                                              tiling        => Tiling);
   begin
      the_Model.set_Bounds;
      return the_Model;
   end new_Item;



   overriding
   procedure destroy (Self : in out Item)
   is
      procedure free is new ada.Unchecked_Deallocation (openGL.height_Map, openGL.Model.terrain.height_Map_view);
   begin
      openGL.Model.destroy (openGL.Model.Item (Self));

      free (self.Heights);
   end destroy;



   -------------
   -- Attributes
   --

   type Geometry_view is access all openGL.Geometry.lit_colored_textured.item'class;


   overriding
   function  to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                     Fonts    : in     Font.font_id_Maps_of_font.Map) return openGL.Geometry.views
   is
      pragma Unreferenced (Textures, Fonts);

      use openGL.Geometry,
          openGL.Palette,
          openGL.Geometry.lit_colored_textured;

      Heights       :          height_Map_view renames Self.Heights;

      row_Count     : constant Index_t      := Heights'Length (1) - 1;
      col_Count     : constant Index_t      := Heights'Length (2) - 1;

      vertex_Count  : constant Index_t      := Heights'Length (1) * Heights'Length (2);

      indices_Count : constant long_Index_t :=   (2 * (long_Index_t (Heights'Length (2)) + 1)) * (long_Index_t (row_Count) - 1)
                                               +  2 * (long_Index_t (Heights'Length (2)));

      the_Sites     : aliased  openGL.Sites := (1 .. vertex_Count => <>);

      the_Vertices  : aliased  Geometry.lit_colored_textured.Vertex_array := (1 .. vertex_Count  => <>);
      the_Indices   : aliased  Indices                                    := (1 .. indices_Count => <>);

      the_Geometry  : constant Geometry_view := Geometry.lit_colored_textured.new_Geometry (texture_is_alpha => False).all'Access;
      the_Bounds    :          openGL.Bounds := null_Bounds;


--        procedure free is new ada.Unchecked_Deallocationhecked_Deallocation (openGL.height_Map, openGL.IO.height_Map_view);


--        procedure flip (Self : height_Map_view)
--        is
--           use type openGL.Index_t;
--           Pad : openGL.io.height_Map_view := new openGL.height_Map' (Self.all);
--
--        begin
--           for Row in Self'Range (1)
--           loop
--              for Col in Self'Range (2)
--              loop
--                 Self (Row, Col) := Pad (Self'Last (1) - Row + 1,  Col);
--              end loop;
--           end loop;
--
--           free (Pad);
--        end flip;


   begin
--        flip (Heights);

      set_Sites :
      declare
         vert_Id          :          Index_t  := 0;
         the_height_Range : constant Vector_2 := height_Extent (Heights.all);
         Middle           : constant Real     := (the_height_Range (1) + the_height_Range (2))  /  2.0;
         flipped_Row      :          Index_t;
      begin
         for Row in 1 .. row_Count + 1
         loop
            for Col in 1 .. col_Count + 1
            loop
               vert_Id     := vert_Id + 1;
               flipped_Row := 2 + row_Count - Row;     -- Flipping the row simplifies building the triangle strip below.

               the_Sites (vert_Id) := (Real (Col)                 - Real (col_Count) / 2.0 - 1.0,
                                       Heights (flipped_Row, Col) - Middle,
                                       Real (Row)                 - Real (row_Count) / 2.0 - 1.0);

               the_Bounds.Box.Lower (1) := real'Min (the_Bounds.Box.Lower (1),  the_Sites (vert_Id)(1));
               the_Bounds.Box.Lower (2) := real'Min (the_Bounds.Box.Lower (2),  the_Sites (vert_Id)(2));
               the_Bounds.Box.Lower (3) := real'Min (the_Bounds.Box.Lower (3),  the_Sites (vert_Id)(3));

               the_Bounds.Box.Upper (1) := real'Max (the_Bounds.Box.Upper (1),  the_Sites (vert_Id)(1));
               the_Bounds.Box.Upper (2) := real'Max (the_Bounds.Box.Upper (2),  the_Sites (vert_Id)(2));
               the_Bounds.Box.Upper (3) := real'Max (the_Bounds.Box.Upper (3),  the_Sites (vert_Id)(3));

               the_Bounds.Ball := Real'Max (the_Bounds.Ball,
                                            abs (the_Sites (vert_Id)));

               the_Vertices (vert_Id).Site  := the_Sites (vert_Id);
               the_Vertices (vert_Id).Color := (primary => White,
                                                opacity => openGL.Opaque);
            end loop;
         end loop;

         the_Bounds.Ball := the_Bounds.Ball * 1.1;     -- TODO: Why the '* 1.1' ?
      end set_Sites;


      set_Indices  :
      declare
         Cursor : long_Index_t := 0;
         Start,
         Upper,
         Lower  : Index_t;
      begin
         Start := 1;

         for Row in 1 .. row_Count
         loop
            Upper := Start;
            Lower := Start + col_Count + 1;

            for Col in 1 .. col_Count + 1
            loop
               Cursor := Cursor + 1;   the_Indices (Cursor) := Upper;
               Cursor := Cursor + 1;   the_Indices (Cursor) := Lower;

               if Col /= col_Count + 1
               then
                  Upper := Upper + 1;
                  Lower := Lower + 1;
               end if;
            end loop;

            if Row /= row_Count   -- Not the last row.
            then
               -- Add 1st redundant triangle to allow for next strip.
               Cursor := Cursor + 1;   the_Indices (Cursor) := Lower;

               -- Advance Start index.
               Start  := Start + col_Count + 1;

               -- Add 2nd redundant triangle to allow for next strip.
               Cursor := Cursor + 1;   the_Indices (Cursor) := Start;
            end if;
         end loop;

      end set_Indices;


      set_Normals :
      declare
         type Normals_view is access all openGL.Normals;

         the_Normals : Normals_view := openGL.Geometry.Normals_of (Primitive.triangle_Strip,
                                                                   the_Indices,
                                                                   the_Sites).all'Access;
         procedure free is new ada.Unchecked_Deallocation (openGL.Normals, Normals_view);

      begin
         for Each in the_Vertices'Range
         loop
            the_Vertices (Each).Normal := the_Normals (Each);
         end loop;

         free (the_Normals);
      end set_Normals;


      if Self.color_Map /= null_Asset
      then
         set_texture_Coords :
         declare
            x_Length : constant Real := Real (the_Bounds.Box.upper (1) - the_Bounds.Box.lower (1));
            x_Min    : constant Real := Real (the_Bounds.Box.lower (1));

--              y_Length : constant Real := Real (the_Bounds.Box.upper (2) - the_Bounds.Box.lower (2));
--              y_Min    : constant Real := Real (the_Bounds.Box.lower (2));

            z_Length : constant Real := Real (the_Bounds.Box.upper (3) - the_Bounds.Box.lower (3));
            z_Min    : constant Real := Real (the_Bounds.Box.lower (3));

            upper_Generator : constant openGL.texture.Coordinates.xz_Generator
              := (normalise => (s => (-x_Min, 1.0/x_Length),
                                t => (-z_Min, 1.0/z_Length)),
                  tile      =>  Self.Tiling);

            the_Coords : constant Coordinates_2D := upper_Generator.to_Coordinates (the_Sites'Access);
         begin
            for Each in the_Coords'Range
            loop
               the_Vertices (Each).Coords := the_Coords (Each);
            end loop;
         end set_texture_Coords;


         set_Texture :
         declare
            the_Image   : constant openGL.Image          := openGL.io.to_Image (Self.color_Map);
            the_Texture : constant openGL.Texture.object := to_Texture (the_Image);
         begin
            the_Geometry.Texture_is (the_Texture);
         end set_Texture;
      end if;


      the_Geometry.is_Transparent (False);
      the_Geometry.Vertices_are   (the_Vertices);

      Self.Bounds := the_Bounds;

      declare
         the_Primitive : constant Primitive.indexed.view
           := openGL.Primitive.indexed.new_Primitive (primitive.triangle_Strip,
                                                      the_Indices).all'Access;
      begin
         the_Geometry.add (openGL.Primitive.view (the_Primitive));
      end;

      return (1 => the_Geometry.all'Access);
   end to_GL_Geometries;



   overriding
   procedure set_Bounds (Self : in out Item)
   is
      Heights      :          height_Map_view renames Self.Heights;

      row_Count    : constant openGL.Index_t  := Heights'Length (1) - 1;
      col_Count    : constant openGL.Index_t  := Heights'Length (2) - 1;

      vertex_Count : constant openGL.Index_t  := Heights'Length (1) * Heights'Length (2);

      the_Sites    : aliased  openGL.Sites    := (1 .. vertex_Count => <>);
      the_Bounds   :          openGL.Bounds   := null_Bounds;

   begin
      set_Sites :
      declare
         vert_Id          :          Index_t  := 0;
         the_height_Range : constant Vector_2 := height_Extent (Heights.all);
         Middle           : constant Real     :=   (the_height_Range (1) + the_height_Range (2))
                                                 / 2.0;
      begin
         for Row in 1 .. row_Count + 1
         loop
            for Col in 1 .. col_Count + 1
            loop
               vert_Id := vert_Id + 1;

               the_Sites (vert_Id) := (Real (Col)         - Real (col_Count) / 2.0 - 1.0,
                                       Heights (Row, Col) - Middle,
                                       Real (Row)         - Real (row_Count) / 2.0 - 1.0);

               the_Bounds.Box.Lower (1) := real'Min (the_Bounds.Box.Lower (1),  the_Sites (vert_Id)(1));
               the_Bounds.Box.Lower (2) := real'Min (the_Bounds.Box.Lower (2),  the_Sites (vert_Id)(2));
               the_Bounds.Box.Lower (3) := real'Min (the_Bounds.Box.Lower (3),  the_Sites (vert_Id)(3));

               the_Bounds.Box.Upper (1) := real'Max (the_Bounds.Box.Upper (1),  the_Sites (vert_Id)(1));
               the_Bounds.Box.Upper (2) := real'Max (the_Bounds.Box.Upper (2),  the_Sites (vert_Id)(2));
               the_Bounds.Box.Upper (3) := real'Max (the_Bounds.Box.Upper (3),  the_Sites (vert_Id)(3));

               the_Bounds.Ball := Real'Max (the_Bounds.Ball,
                                            abs (the_Sites (vert_Id)));
            end loop;
         end loop;

         the_Bounds.Ball := the_Bounds.Ball * 1.1;     -- TODO: Why the '* 1.1' ?
      end set_Sites;

      Self.Bounds := the_Bounds;
   end set_Bounds;


end openGL.Model.terrain;
