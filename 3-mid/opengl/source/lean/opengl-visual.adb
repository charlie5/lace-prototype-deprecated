with
     ada.unchecked_Deallocation;

package body openGL.Visual
is

   package body Forge
   is
      function new_Visual (Model      : in openGL.Model.view;
                           Scale      : in Vector_3 := (1.0, 1.0, 1.0);
                           is_Terrain : in Boolean  := False) return openGL.Visual.view
      is
      begin
         return new Visual.item' (Model              => Model,
                                  model_Transform    => Identity_4x4,
                                  camera_Transform   => Identity_4x4,
                                  Transform          => Identity_4x4,
                                  mvp_Transform      => Identity_4x4,
                                  Scale              => Scale,
                                  program_Parameters => null,
                                  is_Terrain         => is_Terrain,
                                  face_Count         => 1,
                                  apparent_Size      => <>);
      end new_Visual;
   end Forge;



   procedure free (Self : in out View)
   is
      procedure deallocate is new ada.unchecked_Deallocation (Item'Class, View);
   begin
      deallocate (Self);
   end free;



   function Model (Self : in Item) return openGL.Model.view
   is
   begin
      return Self.Model;
   end Model;


   procedure Model_is (Self : in out Item;   Now : in openGL.Model.view)
   is
   begin
      Self.Model := Now;
   end Model_is;



   function Scale (Self : in Item) return Vector_3
   is
   begin
      return Self.Scale;
   end Scale;


   procedure Scale_is (Self : in out Item;   Now : in Vector_3)
   is
   begin
      Self.Scale := Now;
   end Scale_is;



   function  is_Terrain (Self : in Item) return Boolean
   is
   begin
      return Self.is_Terrain;
   end is_Terrain;


   procedure is_Terrain (Self : in out Item;   Now : in Boolean := True)
   is
   begin
      Self.is_Terrain := Now;
   end is_Terrain;



   function face_Count (Self : in Item) return Natural
   is
   begin
      return Self.face_Count;
   end face_Count;


   procedure face_Count_is (Self : in out Item;   Now : in Natural)
   is
   begin
      Self.face_Count := Now;
   end face_Count_is;



   function apparent_Size (Self : in Item) return Real
   is
   begin
      return Self.apparent_Size;
   end apparent_Size;


   procedure apparent_Size_is (Self : in out Item;   Now : in Real)
   is
   begin
      Self.apparent_Size := Now;
   end apparent_Size_is;



   function Transform (Self : in Item) return Matrix_4x4
   is
   begin
      return Self.Transform;
   end Transform;


   procedure Transform_is (Self : in out Item;   Now : in Matrix_4x4)
   is
   begin
      Self.Transform := Now;
   end Transform_is;


   function mvp_Transform (Self : in Item) return Matrix_4x4
   is
   begin
      return Self.mvp_Transform;
   end mvp_Transform;


   procedure mvp_Transform_is (Self : in out Item;   Now : in Matrix_4x4)
   is
   begin
      Self.mvp_Transform := Now;
   end mvp_Transform_is;


   function model_Transform (Self : in Item) return Matrix_4x4
   is
   begin
      return Self.model_Transform;
   end model_Transform;


   procedure model_Transform_is (Self : in out Item;   Now : in Matrix_4x4)
   is
   begin
      Self.model_Transform := Now;
   end model_Transform_is;



   function camera_Transform (Self : in Item) return Matrix_4x4
   is
   begin
      return Self.camera_Transform;
   end camera_Transform;


   procedure camera_Transform_is (Self : in out Item;   Now : in Matrix_4x4)
   is
   begin
      Self.camera_Transform := Now;
   end camera_Transform_is;



   procedure Spin_is (Self : in out Item;   Now : in Matrix_3x3)
   is
      use linear_Algebra_3d;
   begin
      set_Rotation (Self.Transform, Now);
      --  set_Rotation (Self.model_Transform, Now);
   end Spin_is;


   function Spin_of (Self : in Item) return Matrix_3x3
   is
      use linear_Algebra_3d;
   begin
      return get_Rotation (Self.Transform);
      --  return get_Rotation (Self.model_Transform);
   end Spin_of;



   procedure Site_is (Self : in out Item;   Now : in Vector_3)
   is
      use linear_Algebra_3d;
   begin
      set_Translation (Self.Transform, Now);
      --  set_Translation (Self.model_Transform, Now);
   end Site_is;


   function Site_of (Self : in Item) return Vector_3
   is
      use linear_Algebra_3d;
   begin
      return get_Translation (Self.Transform);
      --  return get_Translation (Self.model_Transform);
   end Site_of;



   function program_Parameters (Self : in Item) return program.Parameters_view
   is
   begin
      return Self.program_Parameters;
   end program_Parameters;


   procedure program_Parameters_are (Self : in out Item;   Now : in program.Parameters_view)
   is
   begin
      Self.program_Parameters := Now;
   end program_Parameters_are;


end openGL.Visual;
