package openGL.Primitive.non_indexed
--
--  Provides a class for non-indexed openGL primitives.
--
is
   type    Item  is limited new Primitive.item with private;
   subtype Class is Item'Class;

   type    View  is access all Item'Class;
   type    Views is array (Index_t range <>) of View;


   ---------
   --  Forge
   --

   overriding
   procedure define  (Self : in out Item;   Kind : in facet_Kind);

   overriding
   procedure destroy (Self : in out Item);

   function  new_Primitive (Kind         : in facet_Kind;
                            vertex_Count : in Natural) return Primitive.non_indexed.view;

   --------------
   --  Operations
   --

   overriding
   procedure render (Self : in out Item);



private

   type Item  is limited new Primitive.item with
      record
         vertex_Count : Natural := 0;
      end record;

end openGL.Primitive.non_indexed;
