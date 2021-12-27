with
     openGL.Geometry.lit_colored_textured,
     openGL.GlyphImpl.Texture,
     openGL,
     openGL.Primitive.indexed,
     openGL.Texture,

     ada.Directories;


package body openGL.Model.Text.lit_colored
is
   ---------
   --- Forge
   --

   function new_Text (Text     : in String;
                      Font     : in openGL.Font.font_Id;
                      Color    : in lucid_Color;
                      Centered : in Boolean := True) return View
   is
      Font_Name : constant String  := to_String (Font.Name);
      Exists    : constant Boolean := ada.Directories.Exists (Font_Name);
   begin
      if not Exists
      then
         raise no_such_Font with Font_Name;
      end if;

      declare
         Self : constant View := new Item;
      begin
         Self.Text     := new String' (Text);
         Self.Font_Id  := Font;
         Self.Color    := +Color;
         Self.Centered := Centered;
         Self.Bounds   := null_Bounds;

         return Self;
      end;
   end new_Text;


   --------------
   --- Attributes
   --

   overriding
   procedure Text_is (Self : in out Item;   Now : in String)
   is
   begin
      Self.Text          := new String (1 .. Now'Length);
      Self.Text.all      := Now;                             -- NB: This results in Text'First = 1.
      Self.needs_Rebuild := True;
   end Text_is;


   overriding
   function Text (Self : in Item) return String
   is
   begin
      return Self.Text.all;
   end Text;



   overriding
   function  Font (Self : in Item) return openGL.Font.view
   is
   begin
      return Self.Font.all'Access;
   end Font;



   overriding
   function to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                    Fonts    : in     openGL.Font.font_id_Map_of_font) return Geometry.views
   is
      pragma unreferenced (Textures);

      text_Scale : constant Vector_3 := (2.0 * 4.0 / 78.0,                -- TODO: Fix scaling.
                                         2.0 * 4.0 / 95.0,
                                         1.0 / 1.0);
   begin
      if Self.Text.all = ""
      then
         return (1 .. 0 => <>);
      end if;

      declare
         use Geometry,
             Geometry.lit_colored_textured,
             Texture;

         num_Characters : constant Positive     := Self.Text.all'Length;
         num_Indices    : constant long_Index_t := long_Index_t (num_Characters) * 2 * 3;   -- For each character, 2 triangles each with 3 indices.
         num_Vertices   : constant      Index_t :=      Index_t (num_Characters) * 4;       -- For each character, 2 triangles sharing 4 vertices.

         the_Indices    : aliased Indices (1 .. num_Indices);
         the_Vertices   : aliased Geometry.lit_colored_textured.Vertex_array := (1 .. num_Vertices => <>);

         --- Procedure to 'add' a character.
         --

         pen_Site       : Vector_3     := Origin_3D;

         indices_Count  : long_Index_t := 0;
         vertex_Count   :      Index_t := 0;


         procedure add (the_Character : in Character;
                        Next          : in Character)
         is
            pragma unreferenced (Next);
            the_Quad : GlyphImpl.Texture.Quad_t := Self.Font.Quad (the_Character);
         begin
            --- Add indices.
            --

            --  Triangle 1.
            indices_Count               := indices_Count + 1;
            the_Indices (indices_Count) := vertex_Count  + 1;

            indices_Count               := indices_Count + 1;
            the_Indices (indices_Count) := vertex_Count  + 2;

            indices_Count               := indices_Count + 1;
            the_Indices (indices_Count) := vertex_Count  + 3;


            --  Triangle 2.
            indices_Count               := indices_Count + 1;
            the_Indices (indices_Count) := vertex_Count  + 3;

            indices_Count               := indices_Count + 1;
            the_Indices (indices_Count) := vertex_Count  + 4;

            indices_Count               := indices_Count + 1;
            the_Indices (indices_Count) := vertex_Count  + 1;


            --- Scale the Quad sites and advance to pixel units.
            --
            the_Quad.NW.Site (1) := the_Quad.NW.Site (1) * text_Scale (1);     -- TODO: Scaling should be done by the shader.
            the_Quad.NW.Site (2) := the_Quad.NW.Site (2) * text_Scale (2);

            the_Quad.NE.Site (1) := the_Quad.NE.Site (1) * text_Scale (1);
            the_Quad.NE.Site (2) := the_Quad.NE.Site (2) * text_Scale (2);

            the_Quad.SW.Site (1) := the_Quad.SW.Site (1) * text_Scale (1);
            the_Quad.SW.Site (2) := the_Quad.SW.Site (2) * text_Scale (2);

            the_Quad.SE.Site (1) := the_Quad.SE.Site (1) * text_Scale (1);
            the_Quad.SE.Site (2) := the_Quad.SE.Site (2) * text_Scale (2);

            the_Quad.Advance (1) := the_Quad.Advance (1) * text_Scale (1);
            the_Quad.Advance (2) := the_Quad.Advance (2) * text_Scale (2);


            --- Add vertices.
            --

            --  top left (NW)
            --
            vertex_Count := vertex_Count + 1;
            declare
               the_Vertex : Geometry.lit_colored_textured.Vertex renames the_Vertices (vertex_Count);
            begin
               the_Vertex.Site   := pen_Site + the_Quad.NW.Site;
               the_Vertex.Normal := (0.0, 0.0, 1.0);
               the_Vertex.Shine  := 0.5;
               the_Vertex.Color  := Self.Color;
               the_Vertex.Coords := the_Quad.NW.Coords;

               Self.Bounds.Box := Self.Bounds.Box or the_Vertex.Site;
            end;

            --  bottom left (SW)
            --
            vertex_Count := vertex_Count + 1;
            declare
               the_Vertex : Geometry.lit_colored_textured.Vertex renames the_Vertices (vertex_Count);
            begin
               the_Vertex.Site   := pen_Site + the_Quad.SW.Site;
               the_Vertex.Normal := (0.0, 0.0, 1.0);
               the_Vertex.Shine  := 0.5;
               the_Vertex.Color  := Self.Color;
               the_Vertex.Coords := the_Quad.SW.Coords;

               Self.Bounds.Box := Self.Bounds.Box or the_Vertex.Site;
            end;

            --  bottom right (SE)
            --
            vertex_Count := vertex_Count + 1;
            declare
               the_Vertex : Geometry.lit_colored_textured.Vertex renames the_Vertices (vertex_Count);
            begin
               the_Vertex.Site   := pen_Site + the_Quad.SE.Site;
               the_Vertex.Normal := (0.0, 0.0, 1.0);
               the_Vertex.Shine  := 0.5;
               the_Vertex.Color  := Self.Color;
               the_Vertex.Coords := the_Quad.SE.Coords;

               Self.Bounds.Box := Self.Bounds.Box or the_Vertex.Site;
            end;

            --  top right (NE)
            --
            vertex_Count := vertex_Count + 1;
            declare
               the_Vertex : Geometry.lit_colored_textured.Vertex renames the_Vertices (vertex_Count);
            begin
               the_Vertex.Site   := pen_Site + the_Quad.NE.Site;
               the_Vertex.Normal := (0.0, 0.0, 1.0);
               the_Vertex.Shine  := 0.5;
               the_Vertex.Color  := Self.Color;
               the_Vertex.Coords := the_Quad.NE.Coords;

               Self.Bounds.Box := Self.Bounds.Box or the_Vertex.Site;
            end;

            pen_Site        := pen_Site + the_Quad.Advance;
            Self.Bounds.Box := Self.Bounds.Box or pen_Site;
         end add;


         use      Primitive;
         use type openGL.Font.texture.view;

         the_Geometry   : Geometry.lit_colored_textured.view;
         the_Primitive  : Primitive.indexed.view;

         unused         : Vector_3;
         next_Character : Character;

      begin
         if Self.Font = null
         then
            Self.Font := openGL.Font.texture.view (Fonts.Element (Self.Font_Id));
         end if;

         -- Add vertices and indices for each character in the text.
         --
         unused := Self.Font.check_Glyphs (Self.Text.all);   -- Make sure the glyphs, for each character in 'Self.Text' exist in the font.

         for i in Self.Text'Range
         loop
            if i /= Self.Text'Last
            then   next_Character := Self.Text (i + 1);
            else   next_Character := ' ';
            end if;

            add (Self.Text (i), next_Character);
         end loop;

         -- Center the vertex sites, if requested.
         --
         if Self.Centered
         then
            declare
               the_Bounds : constant openGL.Bounds := Self.Font.BBox (Self.Text.all);
            begin
               for i in the_Vertices'Range
               loop
                  the_Vertices (i).Site (1) := the_Vertices (i).Site (1)  -  (the_Bounds.Box.Upper (1) / 2.0) * text_Scale (1);
                  the_Vertices (i).Site (2) := the_Vertices (i).Site (2)  -  (the_Bounds.Box.Upper (2) / 2.0) * text_Scale (2);
               end loop;
            end;
         end if;

         set_Ball_from_Box (Self.Bounds);

         -- Setup the geometry.
         --
         the_Primitive := Primitive.indexed            .new_Primitive (Triangles, the_Indices);
         the_Geometry  := Geometry.lit_colored_textured.new_Geometry  (texture_is_Alpha => True);

         the_Geometry.add          (Primitive.view (the_Primitive));
         the_Geometry.Vertices_are (the_Vertices);
         the_Geometry.Texture_is   (Texture.Forge.to_Texture (Self.Font.gl_Texture));
         the_Geometry.is_Transparent;

         return (1 => Geometry.view (the_Geometry));
      end;
   end to_GL_Geometries;


end openGL.Model.Text.lit_colored;
