with
     openGL.Primitive.indexed,
     openGL.Geometry.textured,
     openGL.io,

     ada.unchecked_Deallocation;


package body openGL.Model.billboard.textured
is

   type Geometry_view is access all openGL.Geometry.textured.item'Class;


   ---------
   --- Forge
   --

   package body Forge
   is
      function new_Billboard (Scale   : in math.Vector_3;
                              Plane   : in billboard.Plane;
                              Texture : in openGL.asset_Name;
                              Lucid   : in Boolean          := False) return View
      is
         Self : constant View := new Item (Lucid);
      begin
         Self.Plane        := Plane;
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
          openGL.Geometry.textured,
          openGL.Texture;

      the_Indices  : aliased constant Indices         := (1, 2, 3, 4);
      the_Sites    :         constant billboard.Sites := vertex_Sites (Self.Plane,
                                                                       Self.Scale (1),
                                                                       Self.Scale (2));

      function new_Face (Vertices : access openGL.geometry.textured.Vertex_array) return Geometry_view
      is
         use
             openGL.Primitive;

         the_Geometry  : constant Geometry_view  := openGL.Geometry.textured.new_Geometry.all'Access;
         the_Primitive : constant Primitive.view := Primitive.indexed.new_Primitive (triangle_Fan,
                                                                                     the_Indices).all'Access;
      begin
         the_Geometry.Vertices_are (Vertices.all);
         the_Geometry.add          (the_Primitive);
         the_Geometry.is_Transparent;

         return the_Geometry;
      end new_Face;


      the_Face : Geometry_view;

   begin
      declare
         the_Vertices : aliased openGL.Geometry.textured.Vertex_array
           := (1 => (site => the_Sites (1),   coords => Self.texture_Coords (1)),
               2 => (site => the_Sites (2),   coords => Self.texture_Coords (2)),
               3 => (site => the_Sites (3),   coords => Self.texture_Coords (3)),
               4 => (site => the_Sites (4),   coords => Self.texture_Coords (4)));
      begin
         the_Face := new_Face (vertices => the_Vertices'Access);

         if Self.texture_Name /= null_Asset
         then
            Self.Texture := openGL.io.to_Texture (to_String (Self.texture_Name));
         end if;

         if Self.Lucid
         then
            if Self.lucid_Image /= null
            then
               if Self.Texture /= null_Object
               then
                  openGL.Texture.set_Image (Self.Texture, Self.lucid_Image.all);
               else
                  Self.Texture := openGL.Texture.to_Texture (Self.lucid_Image.all);
               end if;
            end if;
         else
            if Self.Image /= null
            then
               if Self.Texture /= null_Object
               then
                  openGL.Texture.set_Image (Self.Texture, Self.Image.all);
               else
                  Self.Texture := openGL.Texture.to_Texture (Self.Image.all);
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



   procedure Texture_Coords_are (Self : in out Item;   Now : in Coordinates)
   is
   begin
      Self.texture_Coords := Now;
      Self.needs_Rebuild := True;
   end Texture_Coords_are;



   procedure Scale_is (Self : in out Item;   Now : in Vector_3)
   is
   begin
      Self.Scale := Now;
      Self.needs_Rebuild := True;
   end Scale_is;



   procedure Image_is (Self : in out Item;   Now : in openGL.Image)
   is
      procedure free is new ada.unchecked_Deallocation (Image,
                                                        Image_view);
   begin
      if Self.Image = null
      then
         Self.Image := new openGL.Image' (Now);

      elsif Self.Image'Length (1) = Now'Length (1)
        and Self.Image'Length (2) = Now'Length (2)
      then
         Self.Image.all := Now;

      else
         free (Self.Image);
         Self.Image := new openGL.Image' (Now);
      end if;

      Self.needs_Rebuild := True;
   end Image_is;



   procedure Image_is (Self : in out Item;   Now : in openGL.lucid_Image)
   is
      procedure free is new ada.unchecked_Deallocation (lucid_Image,
                                                        lucid_Image_view);
   begin
      if Self.lucid_Image = null
      then
         Self.lucid_Image := new openGL.lucid_Image' (Now);

      elsif Self.lucid_Image'Length (1) = Now'Length (1)
        and Self.lucid_Image'Length (2) = Now'Length (2)
      then
         Self.lucid_Image.all := Now;

      else
         free (Self.lucid_Image);
         Self.lucid_Image := new openGL.lucid_Image' (Now);
      end if;

      Self.needs_Rebuild := True;
   end Image_is;


end openGL.Model.billboard.textured;
