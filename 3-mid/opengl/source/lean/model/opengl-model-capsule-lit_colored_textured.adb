with
     openGL.Palette,
     openGL.Geometry.lit_colored_textured,
     openGL.Texture,
     openGL.IO,
     openGL.Primitive.indexed,

     float_math.Geometry.d3.Modeller.forge;


package body openGL.Model.capsule.lit_colored_textured
is

   ---------
   --- Forge
   --

   package body Forge
   is
      function new_Capsule (Radius : in math.Real;
                            Height : in math.Real;
                            Image  : in asset_Name := null_Asset) return View
      is
         Self : constant View := new Item;
      begin
         Self.Image := Image;
         Self.define (scale => (Radius * 2.0,
                                Radius * 2.0,
                                Height));
         return Self;
      end new_Capsule;
   end Forge;



   --------------
   --- Attributes
   --

   overriding
   function  Bounds (Self : in Item) return openGL.Bounds
   is
      Length       : constant Real := Self.Scale (3);
      Radius       : constant Real := Self.Scale (1) / 2.0;
      total_Length : constant Real := Length + 2.0 * Radius;
   begin
      return (ball => total_Length / 2.0,
              box  => (lower => (-Radius, -Radius, -total_Length / 2.0),
                       upper => ( Radius,  Radius,  total_Length / 2.0)));
   end Bounds;



   type Geometry_view is access all openGL.Geometry.lit_colored_textured.item'class;


   overriding
   function  to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                     Fonts    : in     Font.font_id_Maps_of_font.Map) return openGL.Geometry.views
   is
      pragma Unreferenced (Textures, Fonts);

      use      openGL.Geometry,
               openGL.Palette,
               openGL.Geometry.lit_colored_textured,
               float_math.Geometry.d3.Modeller.Forge;
      use type Real;

      Length        : constant Real                           := Self.Scale (3);
      Radius        : constant Real                           := Self.Scale (1) / 2.0;
      the_Mesh      : constant float_math.Geometry.d3.a_Model := to_capsule_Model (Length => Length,
                                                                                   Radius => Radius);

      vertex_Count  : constant Index_t                        :=      Index_t (the_Mesh.site_Count);
      indices_Count : constant long_Index_t                   := long_Index_t (the_Mesh.tri_Count * 3);

      the_Vertices  : aliased  openGL.geometry.lit_colored_textured.Vertex_array := (1 .. vertex_Count  => <>);
      the_Indices   : aliased  Indices                                           := (1 .. indices_Count => <>);

      the_Geometry  : constant Geometry_view
        := Geometry_view (openGL.Geometry.lit_colored_textured.new_Geometry (texture_is_Alpha => False));

   begin
      set_Sites :
      declare
         use      linear_Algebra;
         use type math.Real;

         ii : Integer;
      begin
         for i in the_Vertices'Range
         loop
            ii                      := Integer (i);

            the_Vertices (i).Site   := the_Mesh.Sites (ii);
            the_Vertices (i).Normal := Normalised (the_Mesh.Sites (ii));
            the_Vertices (i).Color  := (primary => White,
                                        opacity => openGL.Opaque);
            the_Vertices (i).Coords := (s => 0.0,
                                        t => 0.0);
         end loop;
      end set_Sites;


      set_Indices :
      declare
         j : long_Index_t := 1;
      begin
         for i in 1 .. the_Mesh.tri_Count
         loop
            the_Indices (j) := Index_t (the_Mesh.Triangles (i) (1));   j := j + 1;
            the_Indices (j) := Index_t (the_Mesh.Triangles (i) (2));   j := j + 1;
            the_Indices (j) := Index_t (the_Mesh.Triangles (i) (3));   j := j + 1;
         end loop;
      end set_Indices;


      if self.Image /= null_Asset
      then
         set_Texture :
         declare
            use openGL.Texture;
            the_Image   : constant openGL.Image          := openGL.io.to_Image (to_String (self.Image));
            the_Texture : constant openGL.Texture.object := to_Texture (the_Image);
         begin
            the_Geometry.Texture_is (the_Texture);
         end set_Texture;
      end if;

      the_Geometry.Bounds_are (Self.Bounds);

      the_Geometry.is_Transparent (False);

      Vertices_are (the_Geometry.all, the_Vertices);

      declare
         the_Primitive : constant openGL.Primitive.indexed.view
           := openGL.Primitive.indexed.new_Primitive (primitive.Triangles,
                                                      the_Indices);
      begin
         the_Geometry.add (openGL.Primitive.view (the_Primitive));
      end;

      return (1 => openGL.Geometry.view (the_Geometry));
   end to_GL_Geometries;


end openGL.Model.capsule.lit_colored_textured;
