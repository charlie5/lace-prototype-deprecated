with
     openGL.Primitive.indexed;


package body openGL.Model.line.colored
is

   ---------
   --- Forge
   --

   function to_line_Model (Color : in openGL.Color;
                           End_1,
                           End_2 : in math.Vector_3 := Origin_3d) return Model.line.colored.item
   is
      Self : Model.line.colored.item;
   begin
      Self.Color                   := Color;
      Self.Vertices (1).Site := End_1;
      Self.Vertices (2).Site := End_2;

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
      if Self.Geometry = null
      then
         Self.Geometry := openGL.Geometry.colored.new_Geometry;
      end if;

      set_Sites :
      begin
         Self.Vertices (1).Color := (primary => Self.Color,  opacity => openGL.Opaque);
         self.Vertices (2).Color := (primary => self.Color,  opacity => openGL.Opaque);
      end set_Sites;

      the_Indices := (1, 2);

      Self.Geometry.is_Transparent (False);

      Vertices_are (Self.Geometry.all, Self.Vertices);

      the_Primitive := openGL.Primitive.indexed.new_Primitive (openGL.primitive.Lines,  the_Indices);
      Self.Geometry.add (openGL.Primitive.view (the_Primitive));

      return (1 => Self.Geometry.all'Access);
   end to_GL_Geometries;



   function  Site    (Self : in     Item;   for_End : in end_Id) return math.Vector_3
   is
   begin
      return Self.Vertices (for_End).Site;
   end Site;


   procedure Site_is (Self : in out Item;   Now     : in math.Vector_3;
                                            for_End : in end_Id)
   is
      use openGL.Geometry.colored;
   begin
      Self.Vertices (for_End).Site := Now;
      Vertices_are (Self.Geometry.all, Self.Vertices);

      Self.set_Bounds;
   end Site_is;


end openGL.Model.line.colored;
