with
     openGL.Geometry.colored,
     openGL.Primitive.indexed;


package body openGL.Model.line.colored
is

   type State is
      record
         Vertices : access openGL.geometry.colored.Vertex_array := new openGL.geometry.colored.Vertex_array (1 .. 2);
         Geometry : access openGL.Geometry.colored.item'Class;
      end record;



   ---------
   --- Forge
   --

   function to_line_Model (Color : in openGL.Color;
                           End_1,
                           End_2 : in math.Vector_3 := Origin_3d) return Model.line.colored.item
   is
      use openGL.Geometry.colored;
      Self : Model.line.colored.item;
   begin
      Self.Color                   := Color;
      Self.State                   := new State;
      Self.State.Vertices (1).Site := End_1;
      Self.State.Vertices (2).Site := End_2;

      Self.set_Bounds;

      return Self;
   end to_line_Model;



   function new_line_Model (Color : in openGL.Color;
                            End_1,
                            End_2 : in math.Vector_3 := Origin_3d) return Model.line.colored.view
   is
   begin
      return new Line.colored.item' (to_line_Model (Color, End_1, End_2));
   end new_line_Model;



   --------------
   --- Attributes
   --

   overriding
   function  to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                     Fonts    : in     Font.font_id_Maps_of_font.Map) return openGL.Geometry.views
   is
      pragma Unreferenced (Textures, Fonts);

      use openGL.Geometry.colored;

      indices_Count : constant openGL.long_Index_t := 2;
      the_Indices   : aliased  Indices             := (1 .. indices_Count => <>);
      the_Primitive :          openGL.Primitive.indexed.view;

   begin
      if Self.State.Geometry = null
      then
         Self.State.Geometry := openGL.Geometry.colored.new_Geometry;
      end if;

      set_Sites :
      declare
         use type math.Real;
      begin
         self.State.Vertices (1).Color := (primary => self.Color,  opacity => openGL.Opaque);
         self.State.Vertices (2).Color := (primary => self.Color,  opacity => openGL.Opaque);
      end set_Sites;

      the_Indices := (1, 2);

      Self.State.Geometry.is_Transparent (False);

      Vertices_are (self.State.Geometry.all, self.State.Vertices);

      the_Primitive := openGL.Primitive.indexed.new_Primitive (openGL.primitive.Lines,  the_Indices);
      Self.State.Geometry.add (openGL.Primitive.view (the_Primitive));

      return (1 => Self.State.Geometry.all'Access); -- the_Face.all'access);
   end to_GL_Geometries;



   procedure set_Bounds (Self : in out Item)
   is
      use math.Geometry;
   begin
      Self.Bounds      := null_Bounds;

      Self.Bounds.Box  := Self.Bounds.Box or Vector_3' (Self.state.Vertices (1).Site);
      Self.Bounds.Box  := Self.Bounds.Box or Vector_3' (Self.state.Vertices (2).Site);

      Self.Bounds.Ball := Real'Max (abs (Self.state.Vertices (1).Site),
                                    abs (Self.state.Vertices (2).Site));
   end set_Bounds;



   function  Site    (Self : in     Item;   for_End : in Integer) return math.Vector_3
   is
      use openGL.Geometry.colored;
   begin
      return self.State.Vertices (openGL.Index_t (for_End)).Site;
   end Site;


   procedure Site_is (Self : in out Item;   Now     : in math.Vector_3;
                                            for_End : in Integer)
   is
      use openGL.Geometry.colored;
   begin
      Self.State.Vertices (openGL.Index_t (for_End)).Site := Now;
      Vertices_are (Self.State.Geometry.all, Self.State.Vertices);
      Self.set_Bounds;
   end Site_is;



   overriding
   function  Bounds (Self : in Item) return openGL.Bounds
   is
   begin
      return Self.Bounds;
   end Bounds;


end openGL.Model.line.colored;
