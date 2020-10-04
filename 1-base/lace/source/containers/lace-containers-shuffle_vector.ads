with
     ada.containers.Vectors;

generic
   with package Vectors is new ada.containers.Vectors (<>);

procedure lace.containers.shuffle_Vector (the_Vector : in out vectors.Vector);
