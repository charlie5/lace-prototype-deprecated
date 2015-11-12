with
     openGL.Primitive.indexed,
     openGL.Primitive.long_indexed,

     ada.unchecked_Deallocation,
     ada.unchecked_Conversion;



package body openGL.Geometry
is

   ---------
   --  Forge
   --

   procedure destroy (Self : in out Item)
   is
      use openGL.Buffer;
   begin
      free (Self.Vertices);
      Self.free_Primitives;
   end destroy;



   procedure free (Self : in out View)
   is
      procedure deallocate is new ada.unchecked_Deallocation (Geometry.item'Class, View);
   begin
      if Self = null then
         return;
      end if;

      Self.destroy;
      deallocate (Self);
   end free;



   procedure free_Primitives (Self : in out Item)
   is
   begin
      for Each in 1 .. Self.primitive_Count
      loop
         openGL.Primitive.free (Self.Primitives (Each));
      end loop;

      Self.primitive_Count := 0;
   end free_Primitives;



   --------------
   --  Attributes
   --

   function Label (Self : in Item'Class) return String
   is
   begin
      return to_String (Self.Label);
   end Label;


   procedure Label_is (Self : in out Item'Class;   Now : in String)
   is
   begin
      overwrite (Self.Label, 1, Now);
   end Label_is;



   procedure Indices_are  (Self : in out Item;   Now       : in Indices;
                                                 for_Facia : in Positive)
   is
      the_Primitive : constant openGL.Primitive.indexed.view
        := openGL.Primitive.indexed.view (Self.Primitives (Index_t (for_Facia)));
   begin
      the_Primitive.Indices_are (Now);
   end Indices_are;


   procedure Indices_are  (Self : in out Item;   Now       : in long_Indices;
                                                 for_Facia : in Positive)
   is
      the_Primitive : constant openGL.Primitive.long_indexed.view
        := openGL.Primitive.long_indexed.view (Self.Primitives (Index_t (for_Facia)));
   begin
      the_Primitive.Indices_are (Now);
   end Indices_are;



   function Primitives (Self : in Item'Class) return openGL.Primitive.views
   is
   begin
      return Self.Primitives (1 .. Self.primitive_Count);
   end Primitives;




   function Texture (Self : in Item'Class) return openGL.Texture.Object
   is
   begin
      return self.Texture;
   end Texture;


   procedure Texture_is (Self : in out Item'Class;   Now : in openGL.Texture.Object)
   is
   begin
      self.Texture := Now;
   end Texture_is;



   procedure Program_is  (Self : in out Item;   Now : in openGL.Program.view)
   is
   begin
      Self.Program := Now;
   end Program_is;


   function Program (Self : in Item) return openGL.Program.view
   is
   begin
      return Self.Program;
   end Program;



   function Bounds (self : in Item'Class) return openGL.Bounds
   is
   begin
      return Self.Bounds;
   end Bounds;


   procedure Bounds_are (Self : in out Item'Class;   Now : in openGL.Bounds)
   is
   begin
      Self.Bounds := Now;
   end Bounds_are;




   function is_Transparent (self : in Item) return Boolean
   is
   begin
      return Self.is_Transparent;
   end is_Transparent;


   procedure is_Transparent (Self : in out Item'Class;   Now : in Boolean := True)
   is
   begin
      Self.is_Transparent := Now;
   end is_Transparent;



   --------------
   --  Operations
   --

   procedure add (Self : in out Item'Class;   the_Primitive : in Primitive.view)
   is
   begin
      Self.primitive_Count                   := Self.primitive_Count + 1;
      Self.Primitives (self.primitive_Count) := the_Primitive;
   end add;



   procedure render (Self : in out Item'Class)
   is
      use GL,
          openGL.Texture;

      use type glSizei,
               GLuint;
   begin
      Self         .enable_Texture;
      Self.Program .set_Uniforms;
      Self.Vertices.enable;
      Self.Program .enable_Attributes;

      --  Draw each primitive.
      --

      if Self.primitive_Count = 0
      then
         raise program_Error;
      end if;

      for Each in 1 .. self.primitive_Count
      loop
         Self.Primitives (Each).render;
      end loop;
   end render;



   -----------
   --  Normals
   --

   function vertex_Id_in (face_Kind : in primitive.facet_Kind;
                          Indices   : in openGL.Indices;
                          for_Facet : in long_Index_t;
                          for_Point : in long_Index_t) return Index_t
   is
      use openGL.Primitive;
   begin
      case face_Kind
      is
         when Triangles =>
            return Indices (3 * (for_Facet - 1) + for_Point);

         when triangle_Strip =>
            return Indices (for_Facet - 1 + for_Point);

         when Triangle_Fan =>
            if for_Point = 1
            then   return 1;
            else   return Indices (for_Facet - 1 + for_Point);
            end if;

         when others =>
            raise Program_Error with "openGL primitive " & facet_Kind'Image (face_Kind) & " not yet supportted.";
            return Index_t'Last;
      end case;
   end vertex_Id_in;



   --  'facet_Count_in' return the maximum possible facet count, which includes redundant facets.
   --
   function facet_Count_in (face_Kind : in primitive.facet_Kind;
                            Indices   : in openGL.Indices) return long_Index_t
   is
      use openGL.Primitive;
   begin
      case face_Kind
      is
         when Triangles =>
            return  Indices'Length / 3;

         when   triangle_Strip
              | Triangle_Fan   =>
            return  Indices'Length - 2;

         when others =>
            raise Program_Error with "openGL primitive " & facet_Kind'Image (face_Kind) & " not yet supportted.";
            return 0;
      end case;
   end facet_Count_in;




   ----------
   --  Facets
   --
   type Facet  is array (     Index_t range 1 .. 3) of Index_t;     -- An 'indexed' triangle.
   type Facets is array (long_Index_t   range   <>  ) of Facet;

   type Facets_view is access all Facets;
   procedure free   is new ada.unchecked_Deallocation (Facets, Facets_view);


   --  'Facets_of' returns all non-redundant facets.
   --
   function Facets_of  (face_Kind : in primitive.facet_Kind;
                        Indices   : in openGL.Indices) return access Facets
   is
      use openGL.Primitive;

      the_Facets : Facets_view  := new Facets (1 .. facet_Count_in (face_Kind, Indices));
      Count      : long_Index_t := 0;
   begin
      for Each in the_Facets'Range
      loop
         declare
            P1 : constant Index_t := vertex_Id_in (face_Kind, Indices,  Each, 1);
            P2 : constant Index_t := vertex_Id_in (face_Kind, Indices,  Each, 2);
            P3 : constant Index_t := vertex_Id_in (face_Kind, Indices,  Each, 3);
         begin
            if not (P1 = P2 or P1 = P3 or P2 = P3)
            then
               Count := Count + 1;

               case face_Kind
               is
                  when   Triangles
                       | Triangle_Fan =>
                     the_Facets (Count) := (P1, P2, P3);

                  when   triangle_Strip =>
                     if Each mod 2 = 0
                     then   -- Is an even facet.
                        the_Facets (Count) := (P1, P3, P2);
                     else
                        the_Facets (Count) := (P1, P2, P3);
                     end if;

                  when others =>
                     raise Program_Error with "TBD";
               end case;
            end if;
         end;
      end loop;

      declare
         Result : constant Facets_view := new Facets'(the_Facets (1 .. Count));
      begin
         free (the_Facets);
         return Result;
      end;
   end Facets_of;



   -----------
   --  Normals
   --

   function Normals_of (face_Kind : in primitive.facet_Kind;
                        Indices   : in openGL.Indices;
                        Sites     : in openGL.Sites) return access Normals
   is
      use      linear_Algebra;
      use type Real;

      the_Normals : constant access Normals     := new Normals (Sites'Range);
      the_Facets  :                 Facets_view :=     Facets_of (face_Kind,
                                                                  Indices  ).all'Access;

      type facet_Normals      is array (long_Index_t range 1 .. the_Facets'Length) of Normal;
      type facet_Normals_view is access all facet_Normals;

      procedure free is new ada.unchecked_Deallocation (facet_Normals, facet_Normals_view);

      the_facet_Normals : facet_Normals_view := new facet_Normals;
      N                 : openGL.Vector_3;
      length_N          : Real;

   begin
      --  Calculate normal at each facet.
      --
      for Each in the_Facets'Range
      loop
         N        :=   Vector_3' (Sites (the_Facets (Each)(2))  -  Sites (the_Facets (Each)(1)))
                     * Vector_3' (Sites (the_Facets (Each)(3))  -  Sites (the_Facets (Each)(1)));

         length_N := abs (N);

         if Almost_zero (length_N)
         then   the_facet_Normals (Each) := N;                        -- 0 vector !
         else   the_facet_Normals (Each) := (1.0 / length_N) * N;
         end if;
      end loop;


      --  Calculate normal at each vertex.
      --
      declare
         Id     : Index_t;
         length : Real;
      begin
         for Each in the_Normals'Range
         loop
            the_Normals (Each) := (0.0, 0.0, 0.0);
         end loop;

         for f in the_Facets'Range
         loop
            for p in Index_t'(1) .. 3
            loop
               Id               := the_Facets (f)(p);
               the_Normals (Id) := the_Normals (Id) + the_facet_Normals (f);
            end loop;
         end loop;

         for p in Sites'Range
         loop
            length := abs (the_Normals (p));

            if not Almost_zero (length)
            then   the_Normals (p) := (1.0 / length) * the_Normals (p);
            else   the_Normals (p) := (0.0, -1.0, 0.0);
            end if;
         end loop;
      end;

      free (the_Facets);
      free (the_facet_Normals);

      return the_Normals.all'Unchecked_Access;
   end Normals_of;


end openGL.Geometry;
