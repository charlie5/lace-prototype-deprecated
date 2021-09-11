with
     openGL.Tasks,
     GL.lean,
     System,
     ada.unchecked_Conversion;

package body openGL.Attribute
is
   use GL.lean;

   ---------
   --  Forge
   --

   procedure define  (Self : in out Item)
   is
   begin
      null;
   end define;


   procedure destroy (Self : in out Item)
   is
   begin
      null;
   end destroy;


   package body Forge
   is
      function to_Attribute (Name        : in String;
                             gl_Location : in gl.GLuint;
                             Size        : in gl.GLint;
                             data_Kind   : in Attribute.data_Kind;
                             Stride      : in Natural;
                             Offset      : in storage_Offset;
                             Normalized  : in Boolean) return Item
      is
      begin
         return (name          => new String'(Name),
                 location      => gl_Location,
                 size          => Size,
                 data_kind     => data_Kind,
                 vertex_stride => gl.GLint (Stride),
                 offset        => Offset,
                 normalized    => Boolean'Pos (Normalized));
      end to_Attribute;


      function new_Attribute (Name        : in String;
                              gl_Location : in gl.GLuint;
                              Size        : in gl.GLint;
                              data_Kind   : in Attribute.data_Kind;
                              Stride      : in Natural;
                              Offset      : in Storage_Offset;
                              Normalized  : in Boolean) return View
      is
      begin
         return new Item' (to_Attribute (Name,
                                         gl_Location,
                                         Size,
                                         data_Kind,
                                         Stride,
                                         Offset,
                                         Normalized));
      end new_Attribute;

   end Forge;


   --------------
   --  Attributes
   --

   function Name (Self : in Item'Class) return String
   is
   begin
      return Self.Name.all;
   end Name;


   function gl_Location (Self : in Item'Class) return gl.GLuint
   is
   begin
      return Self.Location;
   end gl_Location;


   --------------
   --  Operations
   --

   procedure enable (Self : in Item)
   is
      use GL,
          system.Storage_Elements;

      type GLvoid_access is access all GLvoid;

      function to_GL is new ada.unchecked_Conversion (attribute.data_Kind, gl.GLenum);          -- TODO: Address different sizes warning.
      function to_GL is new ada.unchecked_Conversion (storage_Offset,      GLvoid_access);
   begin
      Tasks.check;

      glEnableVertexAttribArray (Index      => Self.gl_Location);
      glVertexAttribPointer     (Index      => Self.gl_Location,
                                 Size       => Self.Size,
                                 the_Type   => to_GL (Self.data_Kind),
                                 Normalized => Self.Normalized,
                                 Stride     => Self.vertex_Stride,
                                 Ptr        => to_GL (Self.Offset));
   end enable;


end openGL.Attribute;
