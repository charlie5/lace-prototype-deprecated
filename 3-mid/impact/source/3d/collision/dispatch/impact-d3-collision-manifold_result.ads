with impact.d3.manifold_Point,
     impact.d3.Manifold,
     impact.d3.Object,
     impact.d3.collision.Detector.discrete;



package impact.d3.collision.manifold_Result
--
--  impact.d3.collision.manifold_Result is a helper class to manage contact results.
--
is
   use Math;


   type ContactAddedCallback is access
     function (cp      : access manifold_Point.item'Class;
               colObj0 : in     Object.item'Class;
               partId0 : in     Integer;
               index0  : in     Integer;
               colObj1 : in     Object.item'Class;
               partId1 : in     Integer;
               index1  : in     Integer) return Boolean;

   gContactAddedCallback : ContactAddedCallback;   -- This is to allow MaterialCombiner/Custom Friction/Restitution values





   type Item is new impact.d3.collision.Detector.discrete.Result with private;


   package Forge
   is

      function  to_manifold_Result (body0, body1 : access Object.item'Class) return Item;

   end Forge;



   procedure setPersistentManifold (Self : in out Item;   manifoldPtr : access impact.d3.Manifold.item'Class);
   function  getPersistentManifold (Self : in     Item)          return access impact.d3.Manifold.item'Class;




   overriding procedure setShapeIdentifiersA (Self : in out Item;   partId0 : in Integer;
                                                                     index0  : in Integer);

   overriding procedure setShapeIdentifiersB (Self : in out Item;   partId1 : in Integer;
                                                                     index1  : in Integer);



   overriding procedure addContactPoint      (Self : in out Item;   normalOnBInWorld : in math.Vector_3;
                                                                     pointInWorld     : in math.Vector_3;
                                                                     depth            : in math.Real  );



   procedure refreshContactPoints (Self : in out Item);








private



   type Item is new impact.d3.collision.Detector.discrete.Result with
      record
         m_manifoldPtr : access impact.d3.Manifold.item'Class;

         --  we need this for compounds
         m_rootTransA  : Transform_3d;
         m_rootTransB  : Transform_3d;

         m_body0,
         m_body1       : access Object.item'Class;

         m_partId0,
         m_partId1,
         m_index0,
         m_index1      : Integer := -1;
      end record;




   function getBody0Internal (Self : in Item) return access Object.item'Class;
   function getBody1Internal (Self : in Item) return access Object.item'Class;




end impact.d3.collision.manifold_Result;
