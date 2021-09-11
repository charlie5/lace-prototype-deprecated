limited
with
     openGL.Program;


package openGL.Variable.uniform
--
--  Models a uniform variable for shaders.
--
is
   type Item  is abstract new Variable.item with private;


   ---------
   --  Forge
   --

   procedure define  (Self : in out Item;   Program : access openGL.Program.item'class;
                                            Name    : in     String);
   overriding
   procedure destroy (Self : in out Item);


   -----------
   --  Actuals
   --

   type bool  is new Variable.uniform.item with private;
   type int   is new Variable.uniform.item with private;
   type float is new Variable.uniform.item with private;
   type vec3  is new Variable.uniform.item with private;
   type vec4  is new Variable.uniform.item with private;
   type mat3  is new Variable.uniform.item with private;
   type mat4  is new Variable.uniform.item with private;

   procedure Value_is (Self : in bool;    Now : in Boolean);
   procedure Value_is (Self : in int;     Now : in Integer);
   procedure Value_is (Self : in float;   Now : in Real);
   procedure Value_is (Self : in vec3;    Now : in Vector_3);
   procedure Value_is (Self : in vec4;    Now : in Vector_4);
   procedure Value_is (Self : in mat3;    Now : in Matrix_3x3);
   procedure Value_is (Self : in mat4;    Now : in Matrix_4x4);



private

   type Item  is abstract new openGL.Variable.item with null record;

   type bool  is new Variable.uniform.item with null record;
   type int   is new Variable.uniform.item with null record;
   type float is new Variable.uniform.item with null record;

   type vec3  is new Variable.uniform.item with null record;
   type vec4  is new Variable.uniform.item with null record;

   type mat3  is new Variable.uniform.item with null record;
   type mat4  is new Variable.uniform.item with null record;

end openGL.Variable.uniform;
