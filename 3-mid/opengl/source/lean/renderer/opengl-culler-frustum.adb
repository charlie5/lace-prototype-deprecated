with
     openGL.Frustum;

package body openGL.Culler.frustum
is
   ---------
   --- Forge
   --

   procedure define (Self : in out Item)
   is
   begin
      Self.vanish_point_size_Min.Value_is (0.00_12);
   end define;



   --------------
   --- Attributes
   --

   overriding
   procedure add (Self : in out Item;   the_Visual : in Visual.view)
   is
   begin
      null;
   end add;


   overriding
   procedure rid (Self : in out Item;   the_Visual : in Visual.view)
   is
   begin
      null;
   end rid;



   overriding
   function object_Count (Self : in Item) return Natural
   is
      pragma unreferenced (Self);
   begin
      return 0;
   end object_Count;



   function vanish_point_size_Min (Self : in Item'Class) return Real
   is
   begin
      return Self.vanish_point_size_Min.Value;
   end vanish_point_size_Min;


   procedure vanish_point_size_Min_is (Self : in out Item'Class;   Now : in Real)
   is
   begin
      Self.vanish_point_size_Min.Value_is (Now);
   end vanish_point_size_Min_is;



   overriding
   function cull (Self : in Item;   the_Visuals    : in Visual.views;
                                    camera_Frustum : in openGL.frustum.Plane_array;
                                    camera_Site    : in Vector_3) return Visual.views
   is
      visible_Objects : Visual.views (the_Visuals'Range);
      Last            : Natural := 0;

      the_Object      : Visual.view;

      the_vanish_point_size_Min : constant Real := Self.vanish_point_size_Min.Value;

   begin
      -- Apply 'frustum' and 'apparent size' culling.
      --
      for i in the_Visuals'Range
      loop
         the_Object := the_Visuals (i);

         declare
            use openGL.Frustum,
                Visual;

            the_Size      : constant Real := the_Object.Model.Bounds.Ball;
            the_Distance  : constant Real := abs (camera_Site - Site_of (the_Object.all));
            apparent_Size :          Real;


            function is_visible_for_Plane (Which : in openGL.frustum.plane_Id) return Boolean
            is
               the_Site       : Vector_3 renames Site_of (the_Object.all);
               plane_Distance : constant Real :=   camera_Frustum (Which) (1) * the_Site (1)
                                                 + camera_Frustum (Which) (2) * the_Site (2)
                                                 + camera_Frustum (Which) (3) * the_Site (3)
                                                 + camera_Frustum (Which) (4);
            begin
               return plane_Distance + the_Size > 0.0;
            end is_visible_for_plane;

         begin
            if    the_Distance /= 0.0        -- The visual is on same site as camera.
              and the_Size     /= 0.0        -- The visual bounds are known.
            then
               apparent_Size := the_Size / the_Distance;
            else
               apparent_Size := Real'Last;
            end if;

            if    apparent_Size > the_vanish_point_size_Min
              and is_visible_for_Plane (Left)
              and is_visible_for_Plane (Right)
              and is_visible_for_Plane (High)
              and is_visible_for_Plane (Low)
            then
               Last                   := Last + 1;
               visible_Objects (Last) := the_Object;
               the_Object.apparent_Size_is (apparent_Size);
            end if;
         end;
      end loop;

      return visible_Objects (1 .. Last);
   end cull;


end openGL.Culler.frustum;
