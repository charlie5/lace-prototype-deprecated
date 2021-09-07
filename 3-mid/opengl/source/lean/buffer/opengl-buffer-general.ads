generic
   type base_Object   is new openGL.Buffer.Object with private;

   type Index         is range <>;
   type Element       is private;
   type Element_Array is array (Index range <>) of Element;

package openGL.Buffer.general
--
--  A generic for producing various types of openGL vertex buffer objects.
--
is
   type Object is new base_Object with private;
   type View   is access all Object'Class;


   ---------
   --  Forge
   --

   package Forge
   is
      function to_Buffer (From  : access constant Element_Array;
                          Usage : in              Buffer.Usage) return Object;

      function to_Buffer (From  : in              Element_Array;
                          Usage : in              Buffer.Usage) return  Object;
   end Forge;


   --------------
   --  Operations
   --

   procedure set (Self : in out Object;   Position : in              Positive     := 1;
                                          To       : in              Element_Array);

   procedure set (Self : in out Object;   Position : in              Positive     := 1;
                                          To       : access constant Element_Array);



private

   type Object is new base_Object with
      record
         Usage : Buffer.Usage;
      end record;

   default_Terminator : Element;       -- No 'Interfaces.C.Pointers' subprogram is called which uses the default terminator, so
                                       -- a default 'Element' should suffice.

end openGL.Buffer.general;
