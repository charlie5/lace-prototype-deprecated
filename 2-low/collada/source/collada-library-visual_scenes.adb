with
     float_Math.Algebra.linear.D3,
     ada.unchecked_Deallocation;

package body collada.Library.visual_scenes
is
   -------------
   --- Transform
   --

   function to_Matrix (Self : in Transform) return collada.Matrix_4x4
   is
      use Math,
          math.Algebra.linear,
          math.Algebra.linear.D3;
   begin
      case Self.Kind
      is
         when Translate =>
            return Transpose (to_translate_Matrix (Self.Vector));     -- Transpose converts from math Row vectors to collada Col vectors.

         when Rotate =>
            declare
               the_Rotation : constant Matrix_3x3 := Transpose (to_Rotation (Self.Axis (1),     -- Transpose converts from math Row vectors to collada Col vectors.
                                                                             Self.Axis (2),
                                                                             Self.Axis (3),
                                                                             Self.Angle));
            begin
               return to_rotate_Matrix (the_Rotation);
            end;

         when Scale =>
            return to_scale_Matrix (Self.Scale);

         when full_Transform =>
            return Self.Matrix;
      end case;
   end to_Matrix;


   --------
   --- Node
   --

   function Sid (Self : in Node) return Text
   is
   begin
      return Self.Sid;
   end Sid;


   function Id (Self : in Node) return Text
   is
   begin
      return Self.Id;
   end Id;


   function Name (Self : in Node) return Text
   is
   begin
      return Self.Name;
   end Name;


   --------------
   --- Transforms
   --

   function Transforms (Self : in Node) return Transform_array
   is
   begin
      return Self.Transforms.all;
   end Transforms;



   function fetch_Transform (Self : access Node;   transform_Sid : in String) return access Transform
   is
      use type ada.Strings.unbounded.unbounded_String;
   begin
      for i in Self.Transforms'Range
      loop
         if Self.Transforms (i).Sid = transform_Sid
         then
            return Self.Transforms (i)'Access;
         end if;
      end loop;

      return null;
   end fetch_Transform;



   procedure add (Self : in out Node;   the_Transform : in Transform)
   is
      Old : Transform_array_view := Self.Transforms;

      procedure deallocate is new ada.unchecked_Deallocation (Transform_array, Transform_array_view);

   begin
      if Old = null
      then   Self.Transforms := new Transform_array' (1 =>      the_Transform);
      else   Self.Transforms := new Transform_array' (Old.all & the_Transform);
             deallocate (Old);
      end if;
   end add;



   function local_Transform (Self : in Node) return Matrix_4x4
   is
   begin
      if Self.Transforms = null
      then
         return Identity_4x4;
      end if;

      declare
         use Math;

         all_Transforms : Transform_array renames Self.Transforms.all;
         the_Result     : Matrix_4x4      :=      math.Identity_4x4;

      begin
         for i in all_Transforms'Range
         loop
            the_Result := the_Result * to_Matrix (all_Transforms (i));
         end loop;

         return the_Result;
      end;
   end local_Transform;



   function global_Transform (Self : in Node) return Matrix_4x4
   is
      use Math;
   begin
      if Self.Parent = null
      then
         return Self.local_Transform;
      else
         return Self.Parent.global_Transform * Self.local_Transform;     -- Recurse.
      end if;
   end global_Transform;



   function find_Transform (Self : in Node;   of_Kind : in transform_Kind;
                                              Sid     : in String) return Positive
   is
      use type Text;
   begin
      for i in Self.Transforms'Range
      loop
         if         Self.Transforms (i).Kind = of_Kind
           and then Self.Transforms (i).Sid  = Sid
         then
            return i;
         end if;
      end loop;

      raise Transform_not_found with "No " & transform_Kind'Image (of_Kind) & " transform found with sid: " & Sid & ".";
   end find_Transform;



   function fetch_Transform (Self : in Node;   of_Kind : in transform_Kind;
                                               Sid     : in String) return Transform
   is
   begin
      return Self.Transforms (find_Transform (Self, of_Kind, Sid));
   end fetch_Transform;



   function find_Transform (Self : in Node;   of_Kind : in transform_Kind) return Positive
   is
   begin
      for i in Self.Transforms'Range
      loop
         if Self.Transforms (i).Kind = of_Kind
         then
            return i;
         end if;
      end loop;

      raise Transform_not_found with "No " & of_Kind'Image & " transform found";
   end find_Transform;



   function fetch_Transform (Self : in Node;   of_Kind : in transform_Kind) return Transform
   is
   begin
      return Self.Transforms (find_Transform (Self, of_Kind));
   end fetch_Transform;



   function full_Transform (Self : in Node) return Matrix_4x4
   is
      the_Transform : constant Transform := fetch_Transform (Self, full_Transform);
   begin
      return the_Transform.Matrix;
   end full_Transform;



   function Translation (Self : in Node) return Vector_3
   is
      the_Translation : constant Transform := fetch_Transform (Self, Translate);
   begin
      return the_Translation.Vector;
   end Translation;



   function Rotate_Z (Self : in Node) return Vector_4
   is
      use Math;
      the_Rotation : Transform;
   begin
      the_Rotation := fetch_Transform (Self, Rotate, "rotationZ");

      return Vector_4 (the_Rotation.Axis & the_Rotation.Angle);

   exception
      when Transform_not_found =>
         the_Rotation := fetch_Transform (Self, Rotate, "rotateZ");

         return Vector_4 (the_Rotation.Axis & the_Rotation.Angle);
   end Rotate_Z;



   procedure set_Location (Self : in out Node;   To : in math.Vector_3)
   is
      Id : constant Positive := find_Transform (Self, Translate, "location");
   begin
      Self.Transforms (Id).Vector := To;
   end set_Location;



   procedure set_Location_x (Self : in out Node;   To : in math.Real)
   is
      Id : constant Positive := find_Transform (Self, Translate, "location");
   begin
      Self.Transforms (Id).Vector (1) := To;
   end set_Location_x;



   procedure set_Location_y (Self : in out Node;   To : in math.Real)
   is
      Id : constant Positive := find_Transform (Self, Translate, "location");
   begin
      Self.Transforms (Id).Vector (2) := To;
   end set_Location_y;



   procedure set_Location_z (Self : in out Node;   To : in math.Real)
   is
      Id : constant Positive := find_Transform (Self, Translate, "location");
   begin
      Self.Transforms (Id).Vector (3) := To;
   end set_Location_z;



   procedure set_Transform (Self : in out Node;   To : in math.Matrix_4x4)
   is
      Id : constant Positive := find_Transform (Self, full_Transform, "transform");
   begin
      Self.Transforms (Id).Matrix := To;
   end set_Transform;



   procedure set_x_rotation_Angle (Self : in out Node;   To : in math.Real)
   is
      Id : Positive;
   begin
      Id := find_Transform (Self, Rotate, "rotationX");
      Self.Transforms (Id).Angle := To;

   exception
      when Transform_not_found =>
         Id := find_Transform (Self, Rotate, "rotateX");
         Self.Transforms (Id).Angle := To;
   end set_x_rotation_Angle;



   procedure set_y_rotation_Angle (Self : in out Node;   To : in math.Real)
   is
      Id : Positive;
   begin
      Id := find_Transform (Self, Rotate, "rotationY");
      Self.Transforms (Id).Angle := To;

   exception
      when Transform_not_found =>
         Id := find_Transform (Self, Rotate, "rotateY");
         Self.Transforms (Id).Angle := To;
   end set_y_rotation_Angle;



   procedure set_z_rotation_Angle (Self : in out Node;   To : in math.Real)
   is
      Id : Positive;
   begin
      Id := find_Transform (Self, Rotate, "rotationZ");
      Self.Transforms (Id).Angle := To;

   exception
      when Transform_not_found =>
         Id := find_Transform (Self, Rotate, "rotateZ");
         Self.Transforms (Id).Angle := To;
   end set_z_rotation_Angle;



   function Rotate_Y (Self : in Node) return Vector_4
   is
      use Math;
      the_Rotation : Transform;
   begin
      the_Rotation := fetch_Transform (Self, Rotate, "rotationY");

      return Vector_4 (the_Rotation.Axis & the_Rotation.Angle);

   exception
      when Transform_not_found =>
         the_Rotation := fetch_Transform (Self, Rotate, "rotateY");

         return Vector_4 (the_Rotation.Axis & the_Rotation.Angle);
   end Rotate_Y;



   function Rotate_X (Self : in Node) return Vector_4
   is
      use Math;
      the_Rotation : Transform;
   begin
      the_Rotation := fetch_Transform (Self, Rotate, "rotationX");

      return Vector_4 (the_Rotation.Axis & the_Rotation.Angle);

   exception
      when Transform_not_found =>
         the_Rotation := fetch_Transform (Self, Rotate, "rotateX");

         return Vector_4 (the_Rotation.Axis & the_Rotation.Angle);
   end Rotate_X;



   function Scale (Self : in Node) return Vector_3
   is
      the_Translation : constant Transform := fetch_Transform (Self, Scale, "scale");
   begin
      return the_Translation.Scale;
   end Scale;



   procedure Sid_is (Self : in out Node;   Now : in Text)
   is
   begin
      Self.Sid := Now;
   end Sid_is;



   procedure Id_is (Self : in out Node;   Now : in Text)
   is
   begin
      Self.Id := Now;
   end Id_is;



   procedure Name_is (Self : in out Node;   Now : in Text)
   is
   begin
      Self.Name := Now;
   end Name_is;


   ------------
   --- Hierachy
   --

   function Parent (Self : in Node) return Node_view
   is
   begin
      return Self.Parent;
   end Parent;



   procedure Parent_is (Self : in out Node;   Now : Node_view)
   is
   begin
      Self.Parent := Now;
   end Parent_is;



   function Children (Self : in Node) return Nodes
   is
   begin
      if Self.Children = null
      then
         return Nodes' (1 .. 0 => <>);  -- No Nodes.
      end if;

      return Self.Children.all;
   end Children;



   function Child (Self : in Node;   Which : in Positive) return Node_view
   is
   begin
      if Self.Children = null
      then
         raise constraint_Error with "No children found.";
      end if;

      return Self.Children (Which);
   end Child;



   function Child (Self : in Node;   Named : in String) return Node_view
   is
      use ada.Strings.unbounded;
   begin
      if Self.Children = null
      then
         raise constraint_Error with "Child not found.";
      end if;

      declare
         the_Children : constant Nodes_view := Self.Children;
      begin
         for i in the_Children'Range
         loop
            if the_Children (i).Name = Named
            then
               return the_Children (i);

            else
               begin
                  return the_Children (i).Child (named => Named);
               exception
                  when constraint_Error => null;
               end;
            end if;
         end loop;
      end;

      raise constraint_Error with "Child not found.";
   end Child;



   procedure add (Self : in out Node;   the_Child : in Node_view)
   is
   begin
      if Self.Children = null
      then
         Self.Children := new Nodes' (1 => the_Child);
      else
         declare
            old_Children : Nodes_view := Self.Children;
            procedure deallocate is new ada.Unchecked_Deallocation (Nodes, Nodes_view);
         begin
            Self.Children := new Nodes' (old_Children.all & the_Child);
            deallocate (old_Children);
         end;
      end if;
   end add;


end collada.Library.visual_scenes;
