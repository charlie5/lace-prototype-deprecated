-- This file is generated by SWIG. Please do *not* modify by hand.
--
with Interfaces.C.Pointers;
with Interfaces.C.Strings;
with System;
package box2d_c.joint_Cursor is

   -- Item
   --

   type Item is record
      Joint : access box2d_c.b2Joint;
   end record;

   -- Items
   --
   type Items is
     array (Interfaces.C.size_t range <>) of aliased box2d_c.joint_Cursor.Item;

   -- Pointer
   --
   type Pointer is access all box2d_c.joint_Cursor.Item;

   -- Pointers
   --
   type Pointers is
     array
       (Interfaces.C.size_t range <>) of aliased box2d_c.joint_Cursor.Pointer;

   -- Pointer_Pointer
   --
   type Pointer_Pointer is access all box2d_c.joint_Cursor.Pointer;

   function construct return box2d_c.joint_Cursor.Item;

private

   pragma Import (C, construct, "Ada_new_joint_Cursor");

end box2d_c.joint_Cursor;
