with
     openGL.Program,
     openGL.Model;

package openGL.Visual
is
   type Item  is tagged private;

   type View  is access all Item'Class;
   type Views is array (Positive range <>) of View;

   type Grid      is array (Integer range <>, Integer range <>) of Visual.view;
   type Grid_view is access all Grid;


   procedure free (Self : in out View);


   ---------
   --- Forge
   --
   package Forge
   is
      function new_Visual (Model      : in openGL.Model.view;
                           Scale      : in Vector_3 := (1.0, 1.0, 1.0);
                           is_Terrain : in Boolean  := False) return Visual.view;
   end Forge;


   --------------
   --  Attributes
   --

   procedure Model_is         (Self : in out Item;   Now : in Model.view);
   function  Model            (Self : in     Item)     return Model.view;

   procedure Scale_is         (Self : in out Item;   Now : in Vector_3);
   function  Scale            (Self : in     Item)     return Vector_3;

   procedure is_Terrain       (Self : in out Item;   Now : in Boolean := True);
   function  is_Terrain       (Self : in     Item)     return Boolean;

   procedure face_Count_is    (Self : in out Item;   Now : in Natural);
   function  face_Count       (Self : in     Item)     return Natural;

   procedure apparent_Size_is (Self : in out Item;   Now : in Real);
   function  apparent_Size    (Self : in     Item)     return Real;

   procedure mvp_Transform_is (Self : in out Item;   Now : in Matrix_4x4);
   function  mvp_Transform    (Self : in     Item)     return Matrix_4x4;

   procedure model_Transform_is (Self : in out Item;   Now : in Matrix_4x4);
   function  model_Transform    (Self : in     Item)     return Matrix_4x4;

   procedure camera_Transform_is (Self : in out Item;   Now : in Matrix_4x4);
   function  camera_Transform    (Self : in     Item)     return Matrix_4x4;

   procedure Transform_is     (Self : in out Item;   Now : in Matrix_4x4);
   function  Transform        (Self : in     Item)     return Matrix_4x4;

   procedure Site_is          (Self : in out Item;   Now : in Vector_3);
   function  Site_of          (Self : in     Item)     return Vector_3;

   procedure Spin_is          (Self : in out Item;   Now : in Matrix_3x3);
   function  Spin_of          (Self : in     Item)     return Matrix_3x3;

   procedure program_Parameters_are (Self : in out Item;   Now : in program.Parameters_view);
   function  program_Parameters     (Self : in     Item)     return program.Parameters_view;



private

   type Item is tagged
      record
         Model              : openGL.Model.view;
         Scale              : Vector_3 := (1.0, 1.0, 1.0);

         model_Transform    : Matrix_4x4;
         camera_Transform   : Matrix_4x4;
         Transform          : Matrix_4x4;
         mvp_Transform      : Matrix_4x4;

         program_Parameters : program.Parameters_view;

         is_Terrain : Boolean  := False;
         face_Count : Positive := 1;

         apparent_Size : Real;     -- A measure of how large the visual is in screen size.
      end record;

end openGL.Visual;
