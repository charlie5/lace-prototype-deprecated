with
     openGL.Palette,
     openGL.Geometry.colored,
     openGL.Primitive.non_indexed;


package body openGL.Model.Grid
is

   type State is
      record
         Vertices : access openGL.geometry.colored.Vertex_array;
         Geometry : access openGL.Geometry.colored.item'Class;
      end record;



   function Line_Count (Extent : in Positive) return Positive
   is
   begin
      if Extent mod 2  /=  0 then
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
      Self : Model.Grid.item;

      vertex_Count : constant Positive := (  line_Count (Width)
                                           + line_Count (Height))  *  2;

      half_Width  : constant math.Real := math.Real (Width)  / 2.0;
      half_Height : constant math.Real := math.Real (Height) / 2.0;

   begin
      Self.Color  := Color;
      Self.Width  := Width;
      Self.Height := Height;
      Self.Bounds := (ball => <>,
                      box  => (lower => (-half_Width, -half_Height, -0.01),
                               upper => ( half_Width,  half_Height,  0.01)));
      set_Ball_from_Box (Self.Bounds);

      Self.State          := new State;
      Self.State.Vertices := new openGL.Geometry.colored.Vertex_array (1 .. openGL.Index_t (vertex_Count));

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
      if Self.State.Geometry = null
      then
         Self.State.Geometry := openGL.Geometry.colored.new_Geometry;
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

            vertex_Count                             := vertex_Count + 1;
            Self.State.Vertices (vertex_Count).Site  := (-half_Width,
                                                         openGL.Real (Row - 1) - half_Height + y_Adjust,
                                                         0.16);
            Self.State.Vertices (vertex_Count).Color := (primary => Color,
                                                         opacity => openGL.Opaque);

            vertex_Count                             := vertex_Count + 1;
            Self.State.Vertices (vertex_Count).Site  := ( half_Width,
                                                         openGL.Real (Row - 1) - half_Height + y_Adjust,
                                                         0.16);
            Self.State.Vertices (vertex_Count).Color := (primary => Color,
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

            vertex_Count                             := vertex_Count + 1;
            Self.State.Vertices (vertex_Count).Site  := (openGL.Real (Col - 1) - half_Width + x_Adjust,
                                                         -half_Height,
                                                         0.16);
            Self.State.Vertices (vertex_Count).Color := (primary => Color,
                                                         opacity => openGL.Opaque);

            vertex_Count                             := vertex_Count + 1;
            Self.State.Vertices (vertex_Count).Site  := (openGL.Real (Col - 1) - half_Width + x_Adjust,
                                                         half_Height,
                                                         0.16);
            Self.State.Vertices (vertex_Count).Color := (primary => Color,
                                                         opacity => openGL.Opaque);
            if Col = col_Count / 2 + 1
            then
               Color := Self.Color;
            end if;
         end loop;
      end set_Sites;

      Self.State.Geometry.is_Transparent (False);
      Vertices_are (Self.State.Geometry.all, Self.State.Vertices);

      the_Primitive := openGL.Primitive.non_indexed.new_Primitive (openGL.primitive.Lines,  Self.State.Vertices'Length);
      Self.State.Geometry.add (openGL.Primitive.view (the_Primitive));

      return (1 => Self.State.Geometry.all'Access);
   end to_GL_Geometries;



   procedure set_Bounds (Self : in out Item)
   is
      use math.Geometry;
   begin
      Self.Bounds     := null_Bounds;
      Self.Bounds.Box := Self.Bounds.Box or Vector_3' (Self.state.Vertices (1).Site);
      Self.Bounds.Box := Self.Bounds.Box or Vector_3' (Self.state.Vertices (2).Site);

      set_Ball_from_Box (Self.Bounds);
   end set_Bounds;



   function Site (Self : in  Item;   for_End : in Integer) return math.Vector_3
   is
      use openGL.Geometry.colored;
   begin
      return Self.State.Vertices (openGL.Index_t (for_End)).Site;
   end Site;



   procedure Site_is (Self : in out Item;   Now     : in math.Vector_3;
                                            for_End : in Integer)
   is
      use openGL.Geometry.colored;
   begin
      Self.State.Vertices (openGL.Index_t (for_End)).Site := Now;

      Vertices_are (self.State.Geometry.all, self.State.Vertices);
      Self.set_Bounds;
   end Site_is;



   overriding
   function  Bounds (Self : in Item) return openGL.Bounds
   is
   begin
      return Self.Bounds;
   end Bounds;


end openGL.Model.Grid;
