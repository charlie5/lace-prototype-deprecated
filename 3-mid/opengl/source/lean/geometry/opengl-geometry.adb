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
      procedure deallocate is new ada.unchecked_Deallocation (Item'Class, View);
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
         Primitive.free (Self.Primitives (Each));
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



   procedure Indices_are (Self : in out Item;   Now       : in Indices;
                                                for_Facia : in Positive)
   is
      the_Primitive : constant Primitive.indexed.view
        := Primitive.indexed.view (Self.Primitives (Index_t (for_Facia)));
   begin
      the_Primitive.Indices_are (Now);
   end Indices_are;


   procedure Indices_are  (Self : in out Item;   Now       : in long_Indices;
                                                 for_Facia : in Positive)
   is
      the_Primitive : constant Primitive.long_indexed.view
        := Primitive.long_indexed.view (Self.Primitives (Index_t (for_Facia)));
   begin
      the_Primitive.Indices_are (Now);
   end Indices_are;



   function Primitives (Self : in Item'Class) return Primitive.views
   is
   begin
      return Self.Primitives (1 .. Self.primitive_Count);
   end Primitives;



   function Texture (Self : in Item'Class) return openGL.Texture.Object
   is
   begin
      return Self.Texture;
   end Texture;


   procedure Texture_is (Self : in out Item'Class;   Now : in openGL.Texture.Object)
   is
   begin
      Self.Texture        := Now;
      Self.is_Transparent :=    Self.is_Transparent
                             or Now .is_Transparent;
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



   function is_Transparent (Self : in Item) return Boolean
   is
   begin
      return    Self.is_Transparent
             or Self.Texture.is_Transparent;
   end is_Transparent;


   procedure is_Transparent (Self : in out Item;   Now : in Boolean := True)
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
   begin
      if Self.primitive_Count = 0
      then
         raise Error with "Unable to render geometry with no primitives.";
      end if;

      Self         .enable_Texture;
      Self.Program .set_Uniforms;
      Self.Vertices.enable;
      Self.Program .enable_Attributes;

      for Each in 1 .. self.primitive_Count     -- Render each primitive.
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
                              for_Facet : in long_Index_t;
                              for_Point : in long_Index_t) return any_Index_t;

   function any_vertex_Id_in (face_Kind : in Primitive.facet_Kind;
                              Indices   : in any_Indices;
                              for_Facet : in long_Index_t;
                              for_Point : in long_Index_t) return any_Index_t
   is
      use openGL.Primitive;
   begin
      case face_Kind
      is
         when Triangles =>
            return Indices (3 * (for_Facet - 1) + for_Point);

         when triangle_Strip =>
            return Indices (for_Facet - 1 + for_Point);

         when triangle_Fan =>
            if for_Point = 1
            then   return 1;
            else   return Indices (for_Facet - 1 + for_Point);
            end if;

         when others =>
            raise Error with "openGL primitive " & face_Kind'Image & " not yet supported.";
      end case;
   end any_vertex_Id_in;



   generic
      type any_Index_t is range <>;
      type any_Indices is array (long_Index_t range <>) of any_Index_t;

   function any_facet_Count_in (face_Kind : in primitive.facet_Kind;
                                Indices   : in any_Indices) return long_Index_t;
   --
   --  Returns the maximum possible facet count, which includes redundant facets.


   function any_facet_Count_in (face_Kind : in primitive.facet_Kind;
                                Indices   : in any_Indices) return long_Index_t
   is
      use Primitive;
   begin
      case face_Kind
      is
         when Triangles =>
            return  Indices'Length / 3;

         when   triangle_Strip
              | triangle_Fan =>
            return  Indices'Length - 2;

         when others =>
            raise Error with "openGL primitive " & face_Kind'Image & " not yet supported.";
      end case;
   end any_facet_Count_in;


   function facet_Count_in is new any_facet_Count_in (any_Index_t => Index_t,
                                                      any_Indices => Indices);
   pragma Unreferenced (facet_Count_in);


   ----------
   --  Facets
   --
   type Facet  is array (     Index_t range 1 .. 3) of Index_t;     -- An 'indexed' triangle.
   type Facets is array (long_Index_t range   <>  ) of Facet;

   type Facets_view is access all Facets;
   procedure free   is new ada.unchecked_Deallocation (Facets, Facets_view);


   generic
      type any_Index_t is range <>;
      type any_Indices is array (long_Index_t range <>) of any_Index_t;

   function any_Facets_of (face_Kind : in primitive.facet_Kind;
                           Indices   : in any_Indices) return access Facets;
   --
   --  'Facets_of' returns all non-redundant facets.


   function any_Facets_of (face_Kind : in primitive.facet_Kind;
                           Indices   : in any_Indices) return access Facets
   is
      use openGL.Primitive;

      function facet_Count_in is new any_facet_Count_in (any_Index_t => any_Index_t,
                                                         any_Indices => any_Indices);

      function vertex_Id_in   is new any_vertex_Id_in   (any_Index_t => any_Index_t,
                                                         any_Indices => any_Indices);

      the_Facets : Facets_view  := new Facets (1 .. facet_Count_in (face_Kind, Indices));
      Count      : long_Index_t := 0;

   begin
      for Each in the_Facets'Range
      loop
         declare
            P1 : constant Index_t := Index_t (vertex_Id_in (face_Kind, Indices,  Each, 1));
            P2 : constant Index_t := Index_t (vertex_Id_in (face_Kind, Indices,  Each, 2));
            P3 : constant Index_t := Index_t (vertex_Id_in (face_Kind, Indices,  Each, 3));
         begin
            if not (   P1 = P2
                    or P1 = P3
                    or P2 = P3)
            then
               Count := Count + 1;

               case face_Kind
               is
                  when Triangles
                     | triangle_Fan =>
                     the_Facets (Count) := (P1, P2, P3);

                  when triangle_Strip =>
                     if Each mod 2 = 0
                     then   -- Is an even facet.
                        the_Facets (Count) := (P1, P3, P2);
                     else
                        the_Facets (Count) := (P1, P2, P3);
                     end if;

                  when others =>
                     raise Error with "openGL primitive " & face_Kind'Image & " not yet supported.";
               end case;

            end if;
         end;
      end loop;

      declare
         Result : constant Facets_view := new Facets' (the_Facets (1 .. Count));
      begin
         free (the_Facets);
         return Result;
      end;
   end any_Facets_of;


   function Facets_of is new any_Facets_of (Index_t,
                                            Indices);
   pragma Unreferenced (Facets_of);


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
      function Facets_of is new any_Facets_of (any_Index_t,
                                               any_Indices);

      the_Normals : constant access Normals     := new Normals (Sites'Range);
      the_Facets  :                 Facets_view :=     Facets_of (face_Kind,
                                                                  Indices).all'unchecked_Access;

      type facet_Normals      is array (long_Index_t range 1 .. the_Facets'Length) of Normal;
      type facet_Normals_view is access all facet_Normals;

      procedure free is new ada.unchecked_Deallocation (facet_Normals, facet_Normals_view);     -- TODO: Should not be needed since freeing will occur when 'facet_Normals_view' goes out of scope ?

      the_facet_Normals : facet_Normals_view := new facet_Normals;
      N                 : Vector_3;
      length_N          : Real;

   begin
      --  Calculate normal at each facet.
      --
      for Each in the_Facets'Range
      loop
         N :=   (Sites (the_Facets (Each)(2)) - Sites (the_Facets (Each)(1)))
              * (Sites (the_Facets (Each)(3)) - Sites (the_Facets (Each)(1)));

         length_N := abs (N);

         if almost_Zero (length_N)
         then   the_facet_Normals (Each) := N;                        -- 0 vector !
         else   the_facet_Normals (Each) := (1.0 / length_N) * N;
         end if;
      end loop;

      --  Calculate normal at each vertex.
      --
      declare
         Id     : Index_t;
         Length : Real;
      begin
         for Each in the_Normals'Range
         loop
            the_Normals (Each) := Origin_3D;
         end loop;

         for f in the_Facets'Range
         loop
            for p in Index_t' (1) .. 3
            loop
               Id               := the_Facets (f) (p);
               the_Normals (Id) := the_Normals (Id) + the_facet_Normals (f);
            end loop;
         end loop;

         for p in the_Normals'Range
         loop
            Length := abs (the_Normals (p));

            if almost_Zero (Length)
            then   the_Normals (p) := (0.0, -1.0, 0.0);
            else   the_Normals (p) := (1.0 / Length) * the_Normals (p);
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
      return my_Normals_of (face_Kind,
                            Indices,
                            Sites).all'unchecked_Access;
   end Normals_of;



   function Normals_of (face_Kind : in primitive.facet_Kind;
                        Indices   : in openGL.long_Indices;
                        Sites     : in openGL.Sites) return access Normals
   is
      function my_Normals_of is new any_Normals_of (any_Index_t => long_Index_t,
                                                    any_Indices => openGL.long_Indices);
   begin
      return my_Normals_of (face_Kind,
                            Indices,
                            Sites).all'unchecked_Access;
   end Normals_of;


   ---------
   -- Bounds
   --

   function get_Bounds (Count : in Natural) return openGL.Bounds
   is
      use Geometry_3D;
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


   ---------------
   -- Transparency
   --

   function get_Transparency (Count : in Natural) return Boolean
   is
      use type color_Value;
   begin
      for i in 1 .. any_Index_t (Count)
      loop
         if get_Color (i).Alpha /= opaque_Value
         then
            return True;
         end if;
      end loop;

      return False;
   end get_Transparency;


end openGL.Geometry;
