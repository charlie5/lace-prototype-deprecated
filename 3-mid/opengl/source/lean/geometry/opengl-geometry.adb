with
     openGL.Primitive.indexed,
     openGL.Primitive.long_indexed,

     Ada.unchecked_Deallocation,
     Ada.unchecked_Conversion;


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
      Self.Texture        := Now;
      Self.is_Transparent := Self.is_Transparent or Now.is_Transparent;
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

   generic
      type any_Index_t is range <>;
      type any_Indices is array (long_Index_t range <>) of any_Index_t;

   function any_vertex_Id_in (face_Kind : in primitive.facet_Kind;
                              Indices   : in any_Indices;
                              for_Facet : in any_Index_t;
                              for_Point : in openGL.long_Index_t) return any_Index_t;

   function any_vertex_Id_in (face_Kind : in primitive.facet_Kind;
                              Indices   : in any_Indices;
                              for_Facet : in any_Index_t;
                              for_Point : in openGL.long_Index_t) return any_Index_t
   is
      use openGL.Primitive;
   begin
      case face_Kind
      is
         when Triangles =>
            return Indices (3 * (long_Index_t (for_Facet) - 1) + for_Point);

         when triangle_Strip =>
            return Indices (long_Index_t (for_Facet) - 1 + for_Point);

         when Triangle_Fan =>
            if for_Point = 1
            then   return 1;
            else   return Indices (long_Index_t (for_Facet) - 1 + for_Point);
            end if;

         when others =>
            raise Program_Error with "openGL primitive " & facet_Kind'Image (face_Kind) & " not yet supportted.";
            return any_Index_t'Last;
      end case;
   end any_vertex_Id_in;



   --  'facet_Count_in' return the maximum possible facet count, which includes redundant facets.
   --
   generic
      type any_Index_t is range <>;
      type any_Indices is array (long_Index_t range <>) of any_Index_t;

   function any_facet_Count_in (face_Kind : in primitive.facet_Kind;
                                Indices   : in any_Indices) return long_Index_t;

   function any_facet_Count_in (face_Kind : in primitive.facet_Kind;
                                Indices   : in any_Indices) return long_Index_t
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
   end any_facet_Count_in;

   function facet_Count_in is new any_facet_Count_in (any_Index_t => openGL.Index_t,
                                                      any_Indices => openGL.Indices);


   ----------
   --  Facets
   --
   type Facet  is array (     Index_t range 1 .. 3) of Index_t;     -- An 'indexed' triangle.
   type Facets is array (long_Index_t   range   <>  ) of Facet;

   type Facets_view is access all Facets;
   procedure free   is new ada.unchecked_Deallocation (Facets, Facets_view);


   --  'Facets_of' returns all non-redundant facets.
   --
   generic
      type any_Index_t is range <>;
      type any_Indices is array (long_Index_t range <>) of any_Index_t;

   function any_Facets_of  (face_Kind : in primitive.facet_Kind;
                            Indices   : in any_Indices) return access Facets;

   function any_Facets_of  (face_Kind : in primitive.facet_Kind;
                            Indices   : in any_Indices) return access Facets
   is
      use openGL.Primitive;

      function facet_Count_in is new any_facet_Count_in (any_Index_t => any_Index_t,
                                                         any_Indices => any_Indices);

      function vertex_Id_in is new any_vertex_Id_in (any_Index_t => any_Index_t,
                                                     any_Indices => any_Indices);

      the_Facets : Facets_view  := new Facets (1 .. facet_Count_in (face_Kind, Indices));
      Count      : long_Index_t := 0;

   begin
      for Each in the_Facets'Range
      loop
         declare
            P1 : constant Index_t := Index_t (vertex_Id_in (face_Kind, Indices,  any_Index_t (Each), 1));
            P2 : constant Index_t := Index_t (vertex_Id_in (face_Kind, Indices,  any_Index_t (Each), 2));
            P3 : constant Index_t := Index_t (vertex_Id_in (face_Kind, Indices,  any_Index_t (Each), 3));
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
   end any_Facets_of;


   function Facets_of is new any_Facets_of (openGL.Index_t,
                                            openGL.Indices);


   -----------
   --  Normals
   --

   generic
      type any_Index_t is range <>;
      type any_Indices is array (long_Index_t range <>) of any_Index_t;

   function any_Normals_of (face_Kind : in primitive.facet_Kind;
                            Indices   : in any_Indices;
                            Sites     : in openGL.Sites) return access Normals;

   function any_Normals_of (face_Kind : in primitive.facet_Kind;
                            Indices   : in any_Indices;
                            Sites     : in openGL.Sites) return access Normals
   is
      use      linear_Algebra;
      use type Real;

      function Facets_of is new any_Facets_of (any_Index_t,
                                               any_Indices);

      the_Normals : constant access Normals     := new Normals (Sites'Range);
      the_Facets  :                 Facets_view :=     Facets_of (face_Kind,
                                                                  Indices  ).all'unchecked_Access;

      type facet_Normals      is array (long_Index_t range 1 .. the_Facets'Length) of Normal;
      type facet_Normals_view is access all facet_Normals;

      procedure free is new ada.unchecked_Deallocation (facet_Normals, facet_Normals_view);

      the_facet_Normals : facet_Normals_view := new facet_Normals; -- (1 .. Index_t (the_Facets'Length));
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

         for p in the_Normals'Range
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
   end any_Normals_of;



   function Normals_of (face_Kind : in primitive.facet_Kind;
                        Indices   : in openGL.Indices;
                        Sites     : in openGL.Sites) return access Normals
   is
      function my_Normals_of is new any_Normals_of (any_Index_t => Index_t,
                                                    any_Indices => openGL.Indices);
   begin
      return my_Normals_of (face_Kind, Indices, Sites).all'unchecked_Access;
   end Normals_of;



   function Normals_of (face_Kind : in primitive.facet_Kind;
                        Indices   : in openGL.long_Indices;
                        Sites     : in openGL.Sites) return access Normals
   is
      function my_Normals_of is new any_Normals_of (any_Index_t => long_Index_t,
                                                    any_Indices => openGL.long_Indices);
   begin
      return my_Normals_of (face_Kind, Indices, Sites).all'unchecked_Access;
   end Normals_of;


   ---------
   -- Bounds
   --

   function get_Bounds (Count : in Natural) return openGL.Bounds
   is
      use Geometry_3d;
      the_Bounds : openGL.Bounds := null_Bounds;
   begin
      for i in 1 .. any_Index_t (Count)
      loop
         the_Bounds.Box  :=    the_Bounds.Box
                            or get_Site (i);

         the_Bounds.Ball := Real'Max (the_Bounds.Ball,
                                      abs (get_Site (i)));
      end loop;

      return the_Bounds;
   end get_Bounds;


end openGL.Geometry;
