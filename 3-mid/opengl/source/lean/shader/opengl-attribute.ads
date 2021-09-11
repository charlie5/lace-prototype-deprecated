with
     GL,
     system.storage_Elements;

package openGL.Attribute
--
--  Models an openGL shader attribute.
--
is
   type Item  is tagged private;
   type View  is access all Item'Class;
   type Views is array (Positive range <>) of View;

   type data_Kind is (GL_BYTE,  GL_UNSIGNED_BYTE,
                      GL_SHORT, GL_UNSIGNED_SHORT,
                      GL_INT,   GL_UNSIGNED_INT,
                      GL_FLOAT, GL_FIXED);
   ---------
   --- Forge
   --

   procedure define  (Self : in out Item);
   procedure destroy (Self : in out Item);

   package Forge
   is
      use system.storage_Elements;

      function  to_Attribute (Name        : in String;
                              gl_Location : in gl.GLuint;
                              Size        : in gl.GLint;
                              data_Kind   : in Attribute.data_Kind;
                              Stride      : in Natural;
                              Offset      : in storage_Offset;
                              Normalized  : in Boolean) return Item;

      function new_Attribute (Name        : in String;
                              gl_Location : in gl.GLuint;
                              Size        : in gl.GLint;
                              data_Kind   : in Attribute.data_Kind;
                              Stride      : in Natural;
                              Offset      : in storage_Offset;
                              Normalized  : in Boolean) return View;
   end Forge;


   --------------
   --- Attributes
   --

   function Name        (Self : in Item'Class) return String;
   function gl_Location (Self : in Item'Class) return gl.GLuint;


   --------------
   --- Operations
   --

   procedure enable (Self : in Item);



private

   type Item is tagged
      record
         Name          : access String;
         Location      :        gl.GLuint;
         Size          :        gl.GLint;
         data_Kind     :        Attribute.data_Kind;
         vertex_Stride :        gl.GLint;
         Offset        :        system.storage_Elements.storage_Offset;
         Normalized    :        gl.GLboolean;
      end record;

   for data_Kind use (GL_BYTE           => 16#1400#,
                      GL_UNSIGNED_BYTE  => 16#1401#,
                      GL_SHORT          => 16#1402#,
                      GL_UNSIGNED_SHORT => 16#1403#,
                      GL_INT            => 16#1404#,
                      GL_UNSIGNED_INT   => 16#1405#,
                      GL_FLOAT          => 16#1406#,
                      GL_FIXED          => 16#140c#);

end openGL.Attribute;
