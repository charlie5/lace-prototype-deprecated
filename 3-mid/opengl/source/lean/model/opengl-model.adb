with
     Ada.Unchecked_Deallocation;


package body openGL.Model
is

   procedure define (Self : in out Item;   Scale : in Vector_3)
   is
   begin
      Self.Scale := Scale;
   end define;



   procedure free (Self : in out View)
   is
      procedure deallocate is new ada.unchecked_Deallocation (Model.item'Class,
                                                              Model.view);
   begin
      Self.destroy;
      deallocate (Self);
   end free;



   procedure destroy (Self : in out Item)
   is
      type access_gl_Face_views is access all openGL.Geometry.views;

      procedure deallocate is new ada.Unchecked_Deallocation (openGL.Geometry.views,
                                                              access_gl_Face_views);

      the_opaque_Faces : access_gl_Face_views := access_gl_Face_views (Self.opaque_Geometries);
      the_lucid_Faces  : access_gl_Face_views := access_gl_Face_views (Self. lucid_Geometries);
   begin
      if the_opaque_Faces /= null
      then
         for Each in the_opaque_Faces'Range
         loop
            openGL.Geometry.free (the_opaque_Faces (Each));
         end loop;

         deallocate (the_opaque_Faces);
      end if;

      if the_lucid_Faces /= null
      then
         for Each in the_lucid_Faces'Range
         loop
            openGL.Geometry.free (the_lucid_Faces (Each));
         end loop;

         deallocate (the_lucid_Faces);
      end if;
   end destroy;



   function  Id (Self : in Item'Class) return openGL.Model_Id
   is
   begin
      return Self.Id;
   end Id;


   procedure Id_is (Self : in out Item'Class;   Now : in openGL.Model_Id)
   is
   begin
      Self.Id := Now;
   end Id_is;



   procedure set_Bounds (Self : in out Item)
   is
      use type openGL.Index_t;
   begin
      Self.Bounds := null_Bounds;

      if Self.opaque_Geometries /= null
      then
         for Each of Self.opaque_Geometries.all
         loop
            Self.Bounds.Box  :=    Self.Bounds.Box
                                or Each.Bounds.Box;

            Self.Bounds.Ball := math.Real'Max (Self.Bounds.Ball,
                                               Each.Bounds.Ball);
         end loop;
      end if;

      if Self.lucid_Geometries /= null
      then
         for Each of Self.lucid_Geometries.all
         loop
            Self.Bounds.Box  :=    Self.Bounds.Box
                                or Each.Bounds.Box;

            Self.Bounds.Ball := math.Real'Max (Self.Bounds.Ball,
                                               Each.Bounds.Ball);
         end loop;
      end if;
   end set_Bounds;



   procedure create_GL_Geometries (Self : in out Item'Class;   Textures : access Texture.name_Map_of_texture'Class;
                                                               Fonts    : in     Font.font_id_Maps_of_font.Map)
   is
      use type openGL.Index_t;

      all_Geometries : constant openGL.Geometry.views := Self.to_GL_Geometries (Textures, Fonts);

      opaque_Faces   :          openGL.Geometry.views (1 .. all_Geometries'Length);
      opaque_Count   :          openGL.Index_t := 0;

      lucid_Faces    :          openGL.Geometry.views (1 .. all_Geometries'Length);
      lucid_Count    :          openGL.Index_t := 0;

      procedure free is new ada.unchecked_Deallocation (openGL.Geometry.views,
                                                        access_Geometry_views);
   begin
      Self.Bounds := null_Bounds;

      -- Separate lucid and opaque geometries.
      --
      for Each in all_Geometries'Range
      loop
         if all_Geometries (Each).is_Transparent
         then
            lucid_Count               := lucid_Count + 1;
            lucid_Faces (lucid_Count) := all_Geometries (Each);
         else
            opaque_Count                := opaque_Count + 1;
            opaque_Faces (opaque_Count) := all_Geometries (Each);
         end if;

         Self.Bounds.Box :=    Self.Bounds.Box
                            or all_Geometries (Each).Bounds.Box;

         Self.Bounds.Ball:= math.Real'Max (Self.Bounds.Ball,
                                           all_Geometries (Each).Bounds.Ball);
      end loop;


      -- Free any existing geometries.
      --
      if Self.opaque_Geometries /= null
      then
         for Each in Self.opaque_Geometries'Range
         loop
            openGL.Geometry.free (Self.opaque_Geometries (Each));
         end loop;

         free (Self.opaque_Geometries);
      end if;

      if Self.lucid_Geometries /= null
      then
         for Each in Self.lucid_Geometries'Range
         loop
            openGL.Geometry.free (Self.lucid_Geometries (Each));
         end loop;

         free (Self.lucid_Geometries);
      end if;


      -- Create new gemometries.
      --
      Self.opaque_Geometries := new openGL.Geometry.views' (opaque_Faces (1 .. opaque_Count));
      Self. lucid_Geometries := new openGL.Geometry.views' ( lucid_Faces (1 ..  lucid_Count));
      Self.needs_Rebuild     := False;
   end create_GL_Geometries;



   function is_Modified (Self : in     Item) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end is_Modified;


   function Bounds (Self : in     Item) return openGL.Bounds
   is
   begin
      return Self.Bounds;
   end Bounds;


--     -----------
--     --- Streams
--     --
--
--     procedure Item_write (Stream : access Ada.Streams.Root_Stream_Type'Class;   Item : in  Model.item)
--     is
--     begin
--        null;
--     end Item_write;
--
--
--     procedure Item_read (Stream : access Ada.Streams.Root_Stream_Type'Class;    Item : out Model.item)
--     is
--        pragma Unreferenced (Stream);
--     begin
--        Item.opaque_Geometries := null;
--        Item.lucid_Geometries  := null;
--     end Item_read;


end openGL.Model;
