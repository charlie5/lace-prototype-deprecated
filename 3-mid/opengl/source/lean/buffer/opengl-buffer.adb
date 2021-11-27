with
     openGL.Errors,
     openGL.Tasks,
     ada.unchecked_Deallocation;

package body openGL.Buffer
is
   use type a_Name;


   ---------------
   --  Buffer Name
   --

   function new_vbo_Name return a_Name
   is
      Name : aliased a_Name;
   begin
      Tasks.check;
      glGenBuffers (1, Name'unchecked_Access);
      return Name;
   end new_vbo_Name;



   procedure free (vbo_Name : in a_Name)
   is
      Name : aliased a_Name := vbo_Name;
   begin
      Tasks.check;
      glDeleteBuffers (1, Name'unchecked_Access);
   end free;
   pragma Unreferenced (free);


   ----------
   --  Object
   --

   procedure verify_Name (Self : in out Object'Class)
   is
   begin
      if Self.Name = 0 then
         Self.Name := new_vbo_Name;
      end if;
   end verify_Name;



   function Name (Self : in Object) return Buffer.a_Name
   is
   begin
      return Self.Name;
   end Name;



   procedure enable (Self : in Object'Class)
   is
      pragma assert (Self.Name > 0);
   begin
      Tasks.check;
      glBindBuffer (to_GL_Enum (Self.Kind),
                    Self.Name);
      openGL.Errors.log;
   end enable;



   procedure destroy (Self : in out Object'Class)
   is
   begin
      Tasks.check;

      glBindBuffer (to_GL_Enum (Self.Kind), 0);
      openGL.Errors.log;

      glDeleteBuffers (1,  Self.Name'Access);
      openGL.Errors.log;

      Self.Name := 0;
   end destroy;



   procedure free (Self : in out View)
   is
      procedure deallocate is new ada.unchecked_Deallocation (Buffer.Object'Class,
                                                              Buffer.view);
   begin
      if Self /= null
      then
         Self.destroy;
         deallocate (Self);
      end if;
   end free;



   function Length (Self : in Object) return Positive
   is
   begin
      return Self.Length;
   end Length;


   -------------------------
   --  'array' Buffer Object
   --

   overriding
   function Kind (Self : in array_Object) return Buffer.a_Kind
   is
      pragma Unreferenced (Self);
   begin
      return array_Buffer;
   end Kind;



   ---------------------------------
   --  'element array' Buffer object
   --

   overriding
   function Kind (Self : in element_array_Object) return Buffer.a_Kind
   is
      pragma Unreferenced (Self);
   begin
      return element_array_Buffer;
   end Kind;


end openGL.Buffer;
