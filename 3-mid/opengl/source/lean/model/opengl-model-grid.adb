with
     openGL.Palette,
     openGL.Primitive.non_indexed;


package body openGL.Model.Grid
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
                           Height : Integer) return Model.Grid.item
   is
      Self         :          Model.Grid.item;

      vertex_Count : constant Positive  := (  line_Count (Width)
                                            + line_Count (Height))  *  2;

      half_Width   : constant math.Real := math.Real (Width)  / 2.0;
      half_Height  : constant math.Real := math.Real (Height) / 2.0;
   begin
      Self.Color  := Color;
      Self.Width  := Width;
      Self.Height := Height;
      Self.Bounds := (ball => <>,
                      box  => (lower => (-half_Width, -half_Height, -0.01),
                               upper => ( half_Width,  half_Height,  0.01)));
      set_Ball_from_Box (Self.Bounds);

      Self.Vertices := new openGL.Geometry.colored.Vertex_array (1 .. openGL.Index_t (vertex_Count));

      return Self;
   end to_grid_Model;


   function new_grid_Model (Color  : openGL.Color;
                            Width  : Integer;
                            Height : Integer) return Model.Grid.view
   is
   begin
      return new Item' (to_grid_Model (Color, Width, Height));
   end new_grid_Model;



   --------------
   --- Attributes
   --

   overriding
   function  to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                     Fonts    : in     Font.font_id_Maps_of_font.Map) return openGL.Geometry.views
   is
      pragma Unreferenced (Textures, Fonts);

      use openGL.Palette,
          openGL.Geometry.colored;

      the_Primitive : openGL.Primitive.non_indexed.view;

   begin
      if Self.Geometry = null
      then
         Self.Geometry := openGL.Geometry.colored.new_Geometry;
      end if;

      set_Sites :
      declare
         row_Count    : constant Positive       := line_Count (Self.Height);
         col_Count    : constant Positive       := line_Count (Self.Width);
         vertex_Count :          openGL.Index_t := 0;

         half_Width   : constant openGL.Real    := openGL.Real (Self.Width)  / 2.0;
         half_Height  : constant openGL.Real    := openGL.Real (Self.Height) / 2.0;

         x_Adjust,
         y_Adjust     :          openGL.Real;

         Color        :          openGL.Color   := Self.Color;

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
               Color := White;
            end if;

            vertex_Count                       := vertex_Count + 1;
            Self.Vertices (vertex_Count).Site  := (-half_Width,
                                                   openGL.Real (Row - 1) - half_Height + y_Adjust,
                                                   0.16);
            Self.Vertices (vertex_Count).Color := (primary => Color,
                                                   opacity => openGL.Opaque);

            vertex_Count                       := vertex_Count + 1;
            Self.Vertices (vertex_Count).Site  := (half_Width,
                                                   openGL.Real (Row - 1) - half_Height + y_Adjust,
                                                   0.16);
            Self.Vertices (vertex_Count).Color := (primary => Color,
                                                   opacity => openGL.Opaque);
            if Row = row_Count / 2 + 1
            then
               Color := Self.Color;
            end if;
         end loop;

         for Col in 1 .. col_Count
         loop
            if Col = col_Count / 2 + 1
            then
               Color := White;
            end if;

            vertex_Count                       := vertex_Count + 1;
            Self.Vertices (vertex_Count).Site  := (openGL.Real (Col - 1) - half_Width + x_Adjust,
                                                   -half_Height,
                                                   0.16);
            Self.Vertices (vertex_Count).Color := (primary => Color,
                                                   opacity => openGL.Opaque);

            vertex_Count                       := vertex_Count + 1;
            Self.Vertices (vertex_Count).Site  := (openGL.Real (Col - 1) - half_Width + x_Adjust,
                                                   half_Height,
                                                   0.16);
            Self.Vertices (vertex_Count).Color := (primary => Color,
                                                   opacity => openGL.Opaque);
            if Col = col_Count / 2 + 1
            then
               Color := Self.Color;
            end if;
         end loop;
      end set_Sites;

      Self.Geometry.is_Transparent (False);
      Vertices_are (Self.Geometry.all, Self.Vertices.all);

      the_Primitive := openGL.Primitive.non_indexed.new_Primitive (openGL.primitive.Lines,  Self.Vertices'Length);
      Self.Geometry.add (openGL.Primitive.view (the_Primitive));

      return (1 => Self.Geometry.all'Access);
   end to_GL_Geometries;


end openGL.Model.Grid;
