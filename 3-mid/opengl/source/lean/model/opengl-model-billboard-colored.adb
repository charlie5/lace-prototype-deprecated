with
     openGL.Primitive.indexed,
     openGL.IO;


package body openGL.Model.billboard.colored
is
   ---------
   --- Forge
   --

   function new_Billboard (Size    : in Size_t         := default_Size;
                           Plane   : in billboard.Plane;
                           Color   : in lucid_Color;
                           Texture : in asset_Name) return View
   is
      Self : constant View := new Item;
   begin
      Self.define (Size);

      Self.Plane        := Plane;
      Self.Color        := Color;
      Self.Texture_Name := Texture;

      return Self;
   end new_Billboard;


   --------------
   --- Attributes
   --

   overriding
   function to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                    Fonts    : in     Font.font_id_Map_of_font) return Geometry.views
   is
      pragma unreferenced (Textures, Fonts);

      use Geometry,
          Geometry.colored,
          Texture;

      the_Indices  : aliased constant Indices         := (1, 2, 3, 4);
      the_Sites    :         constant billboard.Sites := vertex_Sites (Self.Plane,
                                                                       Self.Width,
                                                                       Self.Height);

      function new_Face (Vertices : access Geometry.colored.Vertex_array) return Geometry.colored.view
      is
         use openGL.Primitive;

         the_Geometry  : constant Geometry.colored.view := Geometry.colored.new_Geometry;
         the_Primitive : constant Primitive.view        := Primitive.indexed.new_Primitive (triangle_Fan,
                                                                                            the_Indices).all'Access;
      begin
         the_Geometry.Vertices_are (Vertices.all);
         the_Geometry.add (the_Primitive);
         the_Geometry.is_Transparent;

         return the_Geometry;
      end new_Face;

      Color    : constant rgba_Color  := +Self.Color;
      the_Face : Geometry.colored.view;

   begin
      declare
         the_Vertices : constant access Geometry.colored.Vertex_array := Self.Vertices;
      begin
         the_Vertices.all := Geometry.colored.Vertex_array'
                               (1 => (site => the_Sites (1),  color => Color),
                                2 => (site => the_Sites (2),  color => Color),
                                3 => (site => the_Sites (3),  color => Color),
                                4 => (site => the_Sites (4),  color => Color));

         the_Face := new_Face (Vertices => the_Vertices);

         if Self.texture_Name /= null_Asset
         then
            Self.Texture := IO.to_Texture (Self.texture_Name);
         end if;

         if Self.Texture /= null_Object
         then
            the_Face.Texture_is (Self.Texture);
         end if;
      end;

      Self.Geometry := the_Face;

      return (1 => Geometry.view (the_Face));
   end to_GL_Geometries;



   procedure Color_is (Self : in out Item;   Now : in lucid_Color)
   is
   begin
      Self.Color := Now;

      for i in Self.Vertices'Range
      loop
         Self.Vertices (i).Color := +Now;
      end loop;

      Self.is_Modified := True;
   end Color_is;



   procedure Texture_Coords_are (Self : in out Item;   Now : in Coordinates)
   is
   begin
      Self.texture_Coords := Now;
      Self.needs_Rebuild  := True;
   end Texture_Coords_are;



   overriding
   procedure modify (Self : in out Item)
   is
   begin
      Self.Geometry.Vertices_are (Self.Vertices.all);
      Self.is_Modified := False;
   end modify;


   overriding
   function is_Modified (Self : in Item) return Boolean
   is
   begin
      return Self.is_Modified;
   end is_Modified;


end openGL.Model.billboard.colored;
