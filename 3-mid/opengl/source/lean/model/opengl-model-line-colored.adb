with
     openGL.Primitive.indexed;


package body openGL.Model.line.colored
is
   ---------
   --- Forge
   --

   function to_line_Model (Color : in openGL.Color;
                           End_1,
                           End_2 : in Vector_3 := Origin_3D) return Item
   is
      Self : Item;
   begin
      Self.Color             := +Color;
      Self.Vertices (1).Site := End_1;
      Self.Vertices (2).Site := End_2;

      Self.set_Bounds;

      return Self;
   end to_line_Model;



   function new_line_Model (Color : in openGL.Color;
                            End_1,
                            End_2 : in Vector_3 := Origin_3D) return View
   is
   begin
      return new Item' (to_line_Model (Color, End_1, End_2));
   end new_line_Model;


   --------------
   --- Attributes
   --

   overriding
   function to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                    Fonts    : in     Font.font_id_Map_of_font) return Geometry.views
   is
      pragma unreferenced (Textures, Fonts);
      use Geometry.colored;

      indices_Count : constant long_Index_t := 2;
      the_Indices   : aliased  Indices      := (1 .. indices_Count => <>);
      the_Primitive :          Primitive.indexed.view;

   begin
      if Self.Geometry = null
      then
         Self.Geometry := Geometry.colored.new_Geometry;
      end if;

      set_Sites:
      begin
         Self.Vertices (1).Color := (Primary => Self.Color,  Alpha => opaque_Value);
         Self.Vertices (2).Color := (Primary => Self.Color,  Alpha => opaque_Value);
      end set_Sites;

      the_Indices := (1, 2);

      Self.Geometry.is_Transparent (False);
      Self.Geometry.Vertices_are   (Self.Vertices);

      the_Primitive := Primitive.indexed.new_Primitive (Primitive.Lines, the_Indices);
      Self.Geometry.add (Primitive.view (the_Primitive));

      return (1 => Self.Geometry);
   end to_GL_Geometries;



   function Site (Self : in Item;   for_End : in end_Id) return Vector_3
   is
   begin
      return Self.Vertices (for_End).Site;
   end Site;


   procedure Site_is (Self : in out Item;   Now     : in Vector_3;
                                            for_End : in end_Id)
   is
      use Geometry.colored;
   begin
      Self.Vertices (for_End).Site := Now;
      Self.Geometry.Vertices_are (Self.Vertices);

      Self.set_Bounds;
   end Site_is;


end openGL.Model.line.colored;
