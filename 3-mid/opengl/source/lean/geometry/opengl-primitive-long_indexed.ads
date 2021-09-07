private
with
     openGL.Buffer.long_indices;

package openGL.Primitive.long_indexed
--
--  Provides a class for long indexed openGL primitives.
--
is
   type    Item  is limited new Primitive.item with private;
   subtype Class is Item'Class;

   type    View  is access all Item'class;
   type    Views is array (Index_t range <>) of View;


   ---------
   --  Forge
   --

   function  new_Primitive (Kind    : in facet_Kind;
                            Indices : in long_Indices) return Primitive.long_indexed.view;

   procedure define  (Self : in out Item;   Kind    : in facet_Kind;
                                            Indices : in long_Indices);
   overriding
   procedure destroy (Self : in out Item);


   --------------
   --  Attributes
   --

   procedure Indices_are (Self : in out Item;   Now : in long_Indices);


   --------------
   --  Operations
   --

   overriding
   procedure render (Self : in out Item);



private

   type Item  is limited new Primitive.item with
      record
         Indices : Buffer.long_indices.view;
      end record;

end openGL.Primitive.long_indexed;
