private
with
     openGL.Buffer.short_indices;

package openGL.Primitive.short_indexed
--
--  Provides a class for short indexed openGL primitives.
--
is
   type    Item  is limited new Primitive.item with private;
   subtype Class is Item'Class;

   type    View  is access all Item'Class;
   type    Views is array (Index_t range <>) of View;


   ---------
   --  Forge
   --

   function  new_Primitive (Kind    : in facet_Kind;
                            Indices : in openGL.short_Indices) return Primitive.short_indexed.view;

   function  new_Primitive (Kind    : in facet_Kind;
                            Indices : in openGL.Indices)       return Primitive.short_indexed.view;

   function  new_Primitive (Kind    : in facet_Kind;
                            Indices : in openGL.long_Indices)  return Primitive.short_indexed.view;

   procedure define  (Self : in out Item;   Kind    : in facet_Kind;
                                            Indices : in openGL.short_Indices);
   procedure define  (Self : in out Item;   Kind    : in facet_Kind;
                                            Indices : in openGL.Indices);
   procedure define  (Self : in out Item;   Kind    : in facet_Kind;
                                            Indices : in openGL.long_Indices);
   overriding
   procedure destroy (Self : in out Item);


   --------------
   --  Attributes
   --

   procedure Indices_are (Self : in out Item;   Now : in short_Indices);
   procedure Indices_are (Self : in out Item;   Now : in       Indices);
   procedure Indices_are (Self : in out Item;   Now : in  long_Indices);


   --------------
   --  Operations
   --

   overriding
   procedure render (Self : in out Item);



private

   type Item  is limited new Primitive.item with
      record
         Indices : Buffer.short_indices.view;
      end record;

end openGL.Primitive.short_indexed;
