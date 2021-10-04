package collada.Library.visual_scenes
--
-- Models a collada 'visual_scenes' library, which contains node/joint hierachy info.
--
is
   ------------
   -- Transform
   --

   type transform_Kind is (Translate, Rotate, Scale, full_Transform);

   type Transform (Kind : transform_Kind := transform_Kind'First) is
      record
         Sid : Text;

         case Kind is
            when Translate =>
               Vector : Vector_3;

            when Rotate =>
               Axis   : Vector_3;
               Angle  : math.Real;

            when Scale =>
               Scale  : Vector_3;

            when full_Transform =>
               Matrix : Matrix_4x4;
         end case;
      end record;

   type Transform_array is array (Positive range <>) of aliased Transform;

   function to_Matrix (Self : in Transform) return collada.Matrix_4x4;


   --------
   --- Node
   --

   type Node      is tagged private;
   type Node_view is access all Node;
   type Nodes     is array (Positive range <>) of Node_view;

   function  Sid     (Self : in     Node) return Text;
   function  Id      (Self : in     Node) return Text;
   function  Name    (Self : in     Node) return Text;

   procedure Sid_is  (Self : in out Node;   Now : in Text);
   procedure Id_is   (Self : in out Node;   Now : in Text);
   procedure Name_is (Self : in out Node;   Now : in Text);

   procedure add             (Self : in out Node;   the_Transform : in Transform);
   function  Transforms      (Self : in     Node) return Transform_array;
   function  fetch_Transform (Self : access Node;   transform_Sid : in String) return access Transform;

   function  local_Transform (Self : in     Node) return Matrix_4x4;
   --
   -- Returns the result of combining all 'Transforms'.

   function  global_Transform (Self : in    Node) return Matrix_4x4;
   --
   -- Returns the result of combining 'local_Transform' with each ancestors 'local_Transform'.

   function  full_Transform (Self : in     Node) return Matrix_4x4;
   function  Translation    (Self : in     Node) return Vector_3;
   function  Rotate_Z       (Self : in     Node) return Vector_4;
   function  Rotate_Y       (Self : in     Node) return Vector_4;
   function  Rotate_X       (Self : in     Node) return Vector_4;
   function  Scale          (Self : in     Node) return Vector_3;

   procedure set_x_rotation_Angle (Self : in out Node;   To : in math.Real);
   procedure set_y_rotation_Angle (Self : in out Node;   To : in math.Real);
   procedure set_z_rotation_Angle (Self : in out Node;   To : in math.Real);

   procedure set_Location   (Self : in out Node;   To : in math.Vector_3);
   procedure set_Location_x (Self : in out Node;   To : in math.Real);
   procedure set_Location_y (Self : in out Node;   To : in math.Real);
   procedure set_Location_z (Self : in out Node;   To : in math.Real);

   procedure set_Transform  (Self : in out Node;   To : in math.Matrix_4x4);

   function  Parent    (Self : in     Node)  return Node_view;
   procedure Parent_is (Self : in out Node;   Now : Node_view);

   function  Children  (Self : in     Node)  return Nodes;
   function  Child     (Self : in     Node;   Which : in Positive) return Node_view;
   function  Child     (Self : in     Node;   Named : in String  ) return Node_view;

   procedure add       (Self : in out Node;   the_Child : in Node_view);

   Transform_not_found : exception;


   ----------------
   --- visual_Scene
   --

   type visual_Scene is
      record
         Id   : Text;
         Name : Text;

         root_Node : Node_view;
      end record;

   type visual_Scene_array is array (Positive range <>) of visual_Scene;


   ----------------
   --- Library Item
   --

   type Item is
      record
         Contents      : access visual_Scene_array;
         skeletal_Root :        Text;
      end record;



private

   type Transform_array_view is access all Transform_array;
   type Nodes_view           is access all Nodes;

   type Node is tagged
      record
         Sid  : Text;
         Id   : Text;
         Name : Text;

         Transforms : Transform_array_view;

         Parent   : Node_view;
         Children : Nodes_view;
      end record;


end collada.Library.visual_scenes;
