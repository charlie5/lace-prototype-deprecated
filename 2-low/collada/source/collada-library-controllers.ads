package collada.Library.controllers
--
-- Models a collada 'controllers' library, which is a collection of controllers.
--
is

   type Inputs_view    is access all Library.Inputs;
   type int_Array_view is access all int_Array;


   ----------
   --- Joints
   --

   type Joints is
      record
         Inputs : Inputs_view;
      end record;


   ------------------
   --- vertex_Weights
   --

   type vertex_Weights is
      record
         Count   : Natural;
         Inputs  : Inputs_view;

         v_Count : int_Array_view;
         v       : int_Array_view;
      end record;

   function  joint_Offset_of (Self : in vertex_Weights) return math.Index;
   function weight_Offset_of (Self : in vertex_Weights) return math.Index;


   --------
   --- Skin
   --

   type Skin is
      record
         main_Source       :        Text;
         bind_shape_Matrix :        float_Array (1 .. 16);
         Sources           : access library.Sources;
         Joints            :        controllers.Joints;
         vertex_weights    :        controllers.vertex_Weights;
      end record;

   function Weights_of           (Self : in Skin) return access float_Array;
   function bind_shape_Matrix_of (Self : in Skin) return Matrix_4x4;
   function bind_Poses_of        (Self : in Skin) return Matrix_4x4_array;
   function joint_Names_of       (Self : in Skin) return Text_array;


   --------------
   --- Controller
   --

   type Controller is
      record
         Name : Text;
         Id   : Text;
         Skin : controllers.Skin;
      end record;

   type Controller_array is array (Positive range <>) of Controller;


   ----------------
   --- Library Item
   --

   type Item is
      record
         Contents : access Controller_array;
      end record;


end collada.Library.controllers;
