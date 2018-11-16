with impact.d3.collision.Detector.discrete,
     impact.d3.Shape.convex.internal.polyhedral.box;



package impact.d3.collision.Detector.discrete.box_box
--
--
--  impact.d3.collision.Detector.discrete.box_box wraps the ODE box-box collision detector.
--
is
   use impact.d3.collision.Detector.discrete;



   type Item is new impact.d3.collision.Detector.discrete.item with private;



   function to_box_box_Detector (box1, box2 : in impact.d3.Shape.convex.internal.polyhedral.box.view) return Item;





   overriding procedure getClosestPoints (Self : in out Item;   input       : in     ClosestPointInput;
                                                     output      : in out Result'Class;
                                                     swapResults : in     Boolean          := False);



private

   type Item is new impact.d3.collision.Detector.discrete.item with
      record
         m_box1,
         m_box2 : impact.d3.Shape.convex.internal.polyhedral.box.view;
      end record;

end impact.d3.collision.Detector.discrete.box_box;
