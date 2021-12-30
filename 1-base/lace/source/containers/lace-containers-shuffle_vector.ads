with
     ada.Containers.Vectors;


generic
   with package Vectors is new ada.Containers.Vectors (<>);

procedure lace.Containers.shuffle_Vector (the_Vector : in out vectors.Vector);
