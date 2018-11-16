

package body impact.d3.union_Find
is





   --- Forge
   --

   procedure destruct (Self : in out Item)
   is
   begin
      Self.Free;
   end destruct;





   --- Attributes
   --


   function getNumElements (Self : in Item) return Natural
   is
   begin
      return Natural (Self.m_elements.Length);
   end getNumElements;






   function isRoot         (Self : in Item;   x : in Integer) return Boolean
   is
   begin
      return  x = Self.m_elements.Element (x).m_id;
   end isRoot;




   function getElement (Self : in Item;   index : in Integer) return access btElement
   is
   begin
      return Self.m_elements.Element (index);
   end getElement;








   function find (Self : in Item;   p, q : in Integer) return Boolean
   is
   begin
      return Self.find (p) = Self.find (q);
   end find;







   function find (Self : in Item;   index : in Integer) return Integer  -- nb: index is 0 based island index (atm)
   is
      --  btAssert(x < m_N);
      --  btAssert(x >= 0);
      x          : Integer       := index;
      elementPtr : btElement_view;

--        kkk : Integer := Self.m_elements.Element (x+1).m_id;

   begin
      while x /= Self.m_elements.Element (x).m_id
      loop
         --  Not really a reason not to use path compression, and it flattens the trees/improves find performance dramatically.

         if USE_PATH_COMPRESSION then
--              put_Line ("KKK: " & integer'Image (Self.m_elements.Element (x).m_id));

            elementPtr                       := Self.m_elements.Element (Self.m_elements.Element (x).m_id);
            Self.m_elements.Element (x).m_id := elementPtr.m_id;
            x                                := elementPtr.m_id;
         else
            x := Self.m_elements.Element (x).m_id;
         end if;

         --  btAssert(x < m_N);
         --  btAssert(x >= 0);
      end loop;


      return x;
   end find;








   --- Operations
   --



   --  This is a special operation, destroying the content of impact.d3.union_Find.
   --
   --  It sorts the elements, based on island id, in order to make it easy to iterate over islands.
   --
   procedure sortIslands (Self : in out Item)
   is
      numElements : constant Integer := Integer (Self.m_elements.Length);   -- First store the original body index, and islandId.


      function "<" (L, R : in btElement_view) return Boolean
      is
      begin
         return l.m_id < r.m_id;
      end;

      package Sorter is new btElement_Vectors.Generic_Sorting ("<");

   begin
--        for i in 1 .. numElements
      for i in 0 .. numElements - 1
      loop
         Self.m_elements.Element (i).m_id := Self.find (i);

         if not STATIC_SIMULATION_ISLAND_OPTIMIZATION then
            Self.m_elements.Element (i).m_sz := i;
         end if;
      end loop;

      --  Sort the vector using predicate and std::sort
      --
      Sorter.sort (Self.m_elements); -- .quickSort (impact.d3.union_FindElementSortPredicate);
   end sortIslands;







   procedure reset (Self : in out Item;   N : in Integer)
   is
   begin
      Self.allocate (N + 0);

--        for i in 1 .. N
      for i in 0 .. N - 1
      loop
         Self.m_elements.Element (i).m_id := i;
         Self.m_elements.Element (i).m_sz := 1;
      end loop;
   end reset;









   procedure allocate (Self : in out Item;   N : in Integer)
   is
      use ada.Containers;
   begin
      Self.Free;
--        Self.m_elements.set_Length (Count_type (N));

      for i in 1 .. N
      loop
         Self.m_elements.append (new btElement);
      end loop;
   end allocate;





   procedure Free     (Self : in out Item)
   is
      the_Element : btElement_view;
   begin
      for Each in 0 .. Integer (Self.m_elements.Length) - 1
      loop
         the_Element := Self.m_elements.Element (Each);
         free (the_Element);
      end loop;

      Self.m_elements.clear;
   end Free;






   procedure unite (Self : in out Item;   p, q : in Integer)
   is
      i : constant Integer := Self.find (p);
      j : constant Integer := Self.find (q);

   begin
      if i = j then
         return;
      end if;


      if not USE_PATH_COMPRESSION then
         --  weighted quick union, this keeps the 'trees' balanced, and keeps performance of unite O( log(n) )
         if Self.m_elements.Element (i).m_sz < Self.m_elements.Element (j).m_sz then
            Self.m_elements.Element (i).m_id := j;
            Self.m_elements.Element (j).m_sz := Self.m_elements.Element (j).m_sz + Self.m_elements.Element (i).m_sz - 0;
         else
            Self.m_elements.Element (j).m_id := i;
            Self.m_elements.Element (i).m_sz := Self.m_elements.Element (i).m_sz + Self.m_elements.Element (j).m_sz - 0;
         end if;

      else
         Self.m_elements.Element (i).m_id := j;
         Self.m_elements.Element (j).m_sz := Self.m_elements.Element (j).m_sz + Self.m_elements.Element (i).m_sz - 0;
      end if;

   end unite;



end impact.d3.union_Find;
