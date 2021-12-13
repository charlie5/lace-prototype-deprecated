with
     openGL.Palette,
     openGL.Primitive.non_indexed;


package body openGL.Model.grid
is

   function Line_Count (Extent : in Positive) return Positive
   is
   begin
      if Extent mod 2  /=  0
      then
         return Extent;
      else
         return Extent + 1;
      end if;
   end Line_Count;


   ---------
   --- Forge
   --

   function to_grid_Model (Color  : openGL.Color;
                           Width  : Integer;
                           Height : Integer) return Item
   is
      Self : Item;

      vertex_Count : constant Positive := (  line_Count (Width)
                                           + line_Count (Height))  *  2;

      half_Width   : constant Real := Real (Width)  / 2.0;
      half_Height  : constant Real := Real (Height) / 2.0;
   begin
      Self.Color  := +Color;
      Self.Width  := Width;
      Self.Height := Height;
      Self.Bounds := (Ball => <>,
                      Box  => (lower => (-half_Width, -half_Height, -0.01),
                               upper => ( half_Width,  half_Height,  0.01)));
      set_Ball_from_Box (Self.Bounds);

      Self.Vertices := new Geometry.colored.Vertex_array (1 .. Index_t (vertex_Count));

      return Self;
   end to_grid_Model;


   function new_grid_Model (Color  : openGL.Color;
                            Width  : Integer;
                            Height : Integer) return View
   is
   begin
      return new Item' (to_grid_Model (Color, Width, Height));
   end new_grid_Model;



   --------------
   --- Attributes
   --

   overriding
   function to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                    Fonts    : in     Font.font_id_Map_of_font) return Geometry.views
   is
      pragma unreferenced (Textures, Fonts);

      use Palette,
          Geometry.colored;

      the_Primitive : Primitive.non_indexed.view;

   begin
      if Self.Geometry = null
      then
         Self.Geometry := Geometry.colored.new_Geometry;
      end if;

      set_Sites :
      declare
         row_Count    : constant Positive := line_Count (Self.Height);
         col_Count    : constant Positive := line_Count (Self.Width);
         vertex_Count :          Index_t  := 0;

         half_Width   : constant Real     := Real (Self.Width)  / 2.0;
         half_Height  : constant Real     := Real (Self.Height) / 2.0;

         x_Adjust,
         y_Adjust : Real;

         Color : openGL.rgb_Color := Self.Color;

      begin
         if Self.Width  mod 2 = 0
         then   x_Adjust := 0.0;
         else   x_Adjust := 0.5;
         end if;

         if Self.Height mod 2 = 0
         then   y_Adjust := 0.0;
         else   y_Adjust := 0.5;
         end if;

         for Row in 1 .. row_Count
         loop
            if Row = row_Count / 2 + 1
            then
               Color := +White;
            end if;

            vertex_Count                       := vertex_Count + 1;
            Self.Vertices (vertex_Count).Site  := (-half_Width,
                                                   Real (Row - 1) - half_Height + y_Adjust,
                                                   0.16);
            Self.Vertices (vertex_Count).Color := (primary => Color,
                                                   Alpha => opaque_Value);

            vertex_Count                       := vertex_Count + 1;
            Self.Vertices (vertex_Count).Site  := (half_Width,
                                                   Real (Row - 1) - half_Height + y_Adjust,
                                                   0.16);
            Self.Vertices (vertex_Count).Color := (primary => Color,
                                                   Alpha   => opaque_Value);
            if Row = row_Count / 2 + 1
            then
               Color := Self.Color;
            end if;
         end loop;

         for Col in 1 .. col_Count
         loop
            if Col = col_Count / 2 + 1
            then
               Color := +White;
            end if;

            vertex_Count                       := vertex_Count + 1;
            Self.Vertices (vertex_Count).Site  := (Real (Col - 1) - half_Width + x_Adjust,
                                                   -half_Height,
                                                   0.16);
            Self.Vertices (vertex_Count).Color := (primary => Color,
                                                   Alpha   => opaque_Value);

            vertex_Count                       := vertex_Count + 1;
            Self.Vertices (vertex_Count).Site  := (Real (Col - 1) - half_Width + x_Adjust,
                                                   half_Height,
                                                   0.16);
            Self.Vertices (vertex_Count).Color := (primary => Color,
                                                   Alpha => opaque_Value);
            if Col = col_Count / 2 + 1
            then
               Color := Self.Color;
            end if;
         end loop;
      end set_Sites;

      Self.Geometry.is_Transparent (False);
      Vertices_are (Self.Geometry.all,
                    Self.Vertices.all);

      the_Primitive := Primitive.non_indexed.new_Primitive (openGL.primitive.Lines,
                                                            Self.Vertices'Length);
      Self.Geometry.add (Primitive.view (the_Primitive));

      return (1 => Self.Geometry.all'Access);
   end to_GL_Geometries;


end openGL.Model.grid;
