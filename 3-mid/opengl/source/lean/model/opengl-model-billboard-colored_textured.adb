with
     openGL.Primitive.indexed,
     openGL.io;


package body openGL.Model.billboard.colored_textured
is

   use openGL;
   use type openGL.Real;


   type Geometry_view is access all openGL.Geometry.colored_textured.item'Class;



   ---------
   --- Forge
   --

   package body Forge
   is
      function new_Billboard (Scale   : in math.Vector_3;
                              Plane   : in billboard.Plane;
                              Color   : in openGL.lucid_Color;
                              Texture : in openGL.asset_Name) return View
      is
         Self : constant View := new Item;
      begin
         Self.Plane        := Plane;
         Self.Color        := Color;
         Self.Texture_Name := Texture;
         Self.define (Scale);

         return Self;
      end new_Billboard;
   end Forge;



   --------------
   --- Attributes
   --

   overriding
   function to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                    Fonts    : in     Font.font_id_Maps_of_font.Map) return openGL.Geometry.views
   is
      pragma Unreferenced (Textures, Fonts);

      use openGL.Geometry,
          openGL.Geometry.colored_textured,
          openGL.Texture;

      the_Indices  : aliased constant Indices         := (1, 2, 3, 4);
      the_Sites    :         constant billboard.Sites := vertex_Sites (Self.Plane,
                                                               Self.Scale (1),
                                                               Self.Scale (2));

      function new_Face (Vertices : access openGL.geometry.colored_textured.Vertex_array) return Geometry_view
      is
         use
             openGL.Primitive;

         the_Geometry  : constant Geometry_view  := openGL.Geometry.colored_textured.new_Geometry.all'Access;
         the_Primitive : constant Primitive.view := Primitive.indexed.new_Primitive (triangle_Fan,
                                                                                     the_Indices).all'Access;
      begin
         the_Geometry.Vertices_are (Vertices);
         the_Geometry.add          (the_Primitive);
         the_Geometry.Bounds_are   (Self.Bounds);
         the_Geometry.is_Transparent;

         return the_Geometry;
      end new_Face;


      the_Face : Geometry_view;

   begin
      declare
         the_Vertices : constant access openGL.Geometry.colored_textured.Vertex_array := Self.Vertices;
      begin
         the_Vertices.all := openGL.Geometry.colored_textured.Vertex_array'
                               (1 => (site => the_Sites (1),    color => Self.Color,  coords => (Self.texture_Coords (1))),
                                2 => (site => the_Sites (2),    color => Self.Color,  coords => (Self.texture_Coords (2))),
                                3 => (site => the_Sites (3),    color => Self.Color,  coords => (Self.texture_Coords (3))),
                                4 => (site => the_Sites (4),    color => Self.Color,  coords => (Self.texture_Coords (4))));

         the_Face         := new_Face (vertices => the_Vertices);

         if Self.texture_Name /= null_Asset
         then
            Self.Texture := openGL.io.to_Texture (to_String (Self.texture_Name));
         end if;

         if Self.Texture /= null_Object
         then
            the_Face.Texture_is (Self.Texture);
         end if;
      end;

      Self.Geometry := the_Face;

      return (1 => the_Face.all'Access);
   end to_GL_Geometries;



   procedure Color_is (Self : in out Item;   Now : in lucid_Color)
   is
   begin
      Self.Color := Now;

      for i in Self.Vertices'Range
      loop
         Self.Vertices (i).Color := Now;
      end loop;

      Self.is_Modified := True;
   end Color_is;



   procedure Texture_Coords_are (Self : in out Item;   Now : in Coordinates)
   is
   begin
      Self.texture_Coords := Now;
      Self.needs_Rebuild  := True;
   end Texture_Coords_are;



   procedure Scale_is (Self : in out Item;   Now : in Vector_3)
   is
   begin
      Self.Scale         := Now;
      Self.needs_Rebuild := True;
   end Scale_is;


   overriding
   procedure modify      (Self : in out Item)
   is
   begin
      Self.Geometry.Vertices_are (Self.Vertices);
      Self.is_Modified := False;
   end modify;



   overriding
   function  is_Modified (Self : in     Item) return Boolean
   is
   begin
      return Self.is_Modified;
   end is_Modified;


end openGL.Model.billboard.colored_textured;
