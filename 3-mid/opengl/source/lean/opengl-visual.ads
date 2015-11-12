with
     openGL.Program,
     openGL.Model;


package openGL.Visual
is

   type Item  is tagged private;

   type View  is access all Item'Class;
   type Views is array (Positive range <>) of View;

   procedure free (Self : in out View);



   ---------
   --- Forge
   --
   package Forge
   is
      function new_Visual (Model      : in openGL.Model.view;
                           Scale      : in Vector_3         := (1.0, 1.0, 1.0);
                           is_Terrain : in Boolean          := False          ) return openGL.Visual.view;
   end Forge;



   --------------
   --- Core types
   --

   type Grid      is array (Integer range <>, Integer range <>) of Visual.view;
   type Grid_view is access all Grid;



   --------------
   --  Attributes
   --

   function  Model                  (Self : in     Item)     return openGL.Model.view;
   procedure Model_is               (Self : in out Item;   Now : in openGL.Model.view);

   function  Scale                  (Self : in     Item)     return Vector_3;
   procedure Scale_is               (Self : in out Item;   Now : in Vector_3);

   function  is_Terrain             (Self : in     Item)     return Boolean;
   procedure is_Terrain             (Self : in out Item;   Now : in Boolean := True);

   function  face_Count             (Self : in     Item)     return Natural;
   procedure face_Count_is          (Self : in out Item;   Now : in Natural);

   function  apparent_Size          (Self : in     Item)     return Real;
   procedure apparent_Size_is       (Self : in out Item;   Now : in Real);

   function  mvp_Transform          (Self : in     Item)     return Matrix_4x4;
   procedure mvp_Transform_is       (Self : in out Item;   Now : in Matrix_4x4);

   function  inverse_modelview_Matrix
                                    (Self : in     Item)     return Matrix_3x3;
   procedure inverse_modelview_Matrix_is
                                    (Self : in out Item;   Now : in Matrix_3x3);

   function  Transform              (Self : in     Item)     return Matrix_4x4;
   procedure Transform_is           (Self : in out Item;   Now : in Matrix_4x4);


   procedure Site_is                (Self : in out Item;   Now : in Vector_3);
   function  Site_of                (Self : in     Item)     return Vector_3;

   procedure Spin_is                (Self : in out Item;   Now : in Matrix_3x3);
   function  Spin_of                (Self : in     Item)     return Matrix_3x3;

   function  program_Parameters     (Self : in     Item)     return program.Parameters_view;
   procedure program_Parameters_are (Self : in out Item;   Now : in program.Parameters_view);



   --------------
   --  Operations
   --

   -- Nil.




private

   type Item is tagged
      record
         Model                    : openGL .Model.view;
         Scale                    :         Vector_3  := (1.0, 1.0, 1.0);
         Transform                :         Matrix_4x4;
         mvp_Transform            :         Matrix_4x4;
         inverse_modelview_Matrix :         Matrix_3x3;
         program_Parameters       : program.Parameters_view;

         is_Terrain               :         Boolean   := False;
         face_Count               :         Positive  := 1;

         apparent_Size            :         math.Real;     -- A measure of how large the visual is in screen size.
      end record;

end openGL.Visual;
