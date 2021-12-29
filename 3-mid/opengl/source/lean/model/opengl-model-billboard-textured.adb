with
     openGL.Primitive.indexed,
     openGL.Geometry.textured,
     openGL.io,

     ada.unchecked_Deallocation;


package body openGL.Model.billboard.textured
is
   ---------
   --- Forge
   --

   package body Forge
   is
      function new_Billboard (Size    : in Size_t         := default_Size;
                              Plane   : in billboard.Plane;
                              Texture : in asset_Name;
                              Lucid   : in Boolean        := False) return View
      is
         Self : constant View := new Item (Lucid);
      begin
         Self.Plane        := Plane;
         Self.Texture_Name := Texture;
         Self.define (Size);

         return Self;
      end new_Billboard;
   end Forge;


   --------------
   --- Attributes
   --

   overriding
   function to_GL_Geometries (Self : access Item;   Textures : access openGL.Texture.name_Map_of_texture'Class;
                                                    Fonts    : in     Font.font_id_Map_of_font) return Geometry.views
   is
      pragma unreferenced (Textures, Fonts);

      use Geometry,
          Geometry.textured,
          openGL.Texture;

      the_Indices  : aliased constant Indices         := (1, 2, 3, 4);
      the_Sites    :         constant billboard.Sites := vertex_Sites (Self.Plane,
                                                                       Self.Width,
                                                                       Self.Height);

      function new_Face (Vertices : in Geometry.textured.Vertex_array) return Geometry.textured.view
      is
         use Primitive;

         the_Geometry  : constant Geometry.textured.view := Geometry.textured.new_Geometry;
         the_Primitive : constant Primitive.view         := Primitive.indexed.new_Primitive (triangle_Fan,
                                                                                             the_Indices).all'Access;
      begin
         the_Geometry.Vertices_are (Vertices);
         the_Geometry.add (the_Primitive);
         the_Geometry.is_Transparent;

         return the_Geometry;
      end new_Face;

      the_Face : Geometry.textured.view;

   begin
      declare
         the_Vertices : constant Geometry.textured.Vertex_array
           := (1 => (site => the_Sites (1),  coords => Self.texture_Coords (1)),
               2 => (site => the_Sites (2),  coords => Self.texture_Coords (2)),
               3 => (site => the_Sites (3),  coords => Self.texture_Coords (3)),
               4 => (site => the_Sites (4),  coords => Self.texture_Coords (4)));
      begin
         the_Face := new_Face (Vertices => the_Vertices);

         if Self.texture_Name /= null_Asset
         then
            Self.Texture := IO.to_Texture (Self.texture_Name);
         end if;

         if Self.Lucid
         then
            if Self.lucid_Image /= null
            then
               if Self.Texture /= null_Object
               then
                  set_Image (Self.Texture, Self.lucid_Image.all);
               else
                  Self.Texture := openGL.Texture.Forge.to_Texture (Self.lucid_Image.all);
               end if;
            end if;
         else
            if Self.Image /= null
            then
               if Self.Texture /= null_Object
               then
                  Self.Texture.set_Image (Self.Image.all);
               else
                  Self.Texture := openGL.Texture.Forge.to_Texture (Self.Image.all);
               end if;
            end if;
         end if;

         if Self.Texture /= null_Object
         then
            the_Face.Texture_is (Self.Texture);
         end if;
      end;

      return (1 => the_Face.all'Access);
   end to_GL_Geometries;



   procedure Texture_is (Self : in out Item;   Now : in openGL.Texture.Object)
   is
   begin
      Self.Texture := Now;
   end Texture_is;


   function Texture (Self : in Item) return openGL.Texture.Object
   is
   begin
      return Self.Texture;
   end Texture;



   procedure Texture_Coords_are (Self : in out Item;   Now : in Coordinates)
   is
   begin
      Self.texture_Coords := Now;
      Self.needs_Rebuild  := True;
   end Texture_Coords_are;



   procedure Size_is (Self : in out Item;   Now : in Size_t)
   is
   begin
      Self.Size          := Now;
      Self.needs_Rebuild := True;
   end Size_is;



   procedure Image_is (Self : in out Item;   Now : in Image)
   is
      procedure deallocate is new ada.unchecked_Deallocation (Image,
                                                              Image_view);
   begin
      if Self.Image = null
      then
         Self.Image := new Image' (Now);

      elsif Self.Image'Length (1) = Now'Length (1)
        and Self.Image'Length (2) = Now'Length (2)
      then
         Self.Image.all := Now;

      else
         deallocate (Self.Image);
         Self.Image := new Image' (Now);
      end if;

      Self.needs_Rebuild := True;
   end Image_is;



   procedure Image_is (Self : in out Item;   Now : in lucid_Image)
   is
      procedure deallocate is new ada.unchecked_Deallocation (lucid_Image,
                                                              lucid_Image_view);
   begin
      if Self.lucid_Image = null
      then
         Self.lucid_Image := new lucid_Image' (Now);

      elsif Self.lucid_Image'Length (1) = Now'Length (1)
        and Self.lucid_Image'Length (2) = Now'Length (2)
      then
         Self.lucid_Image.all := Now;

      else
         deallocate (Self.lucid_Image);
         Self.lucid_Image := new lucid_Image' (Now);
      end if;

      Self.needs_Rebuild := True;
   end Image_is;


end openGL.Model.billboard.textured;
