package body impact.d2.world_Callbacks
is


   procedure dummy is begin null; end dummy;




   --  Return true if contact calculations should be performed between these two shapes.
   --  If you implement your own collision filter you may want to build from this implementation.
   --
   function  ShouldCollide (Self : access b2ContactFilter;  fixtureA, fixtureB : access Fixture.b2Fixture'Class) return Boolean
   is
      pragma Unreferenced (Self);
      use type int16, uint16;

      filterA : Fixture.b2Filter renames fixtureA.GetFilterData;
      filterB : Fixture.b2Filter renames fixtureB.GetFilterData;

      collide : Boolean;
   begin

      if         filterA.groupIndex  = filterB.groupIndex
        and then filterA.groupIndex /= 0
      then
         return filterA.groupIndex > 0;
      end if;

      collide :=          (filterA.maskBits     and filterB.categoryBits) /= 0
                 and then (filterA.categoryBits and filterB.maskBits  ) /= 0;

      return collide;
   end ShouldCollide;







--  b2DebugDraw::b2DebugDraw()
--  {
--          m_drawFlags = 0;
--  }
--
--  void b2DebugDraw::SetFlags(uint32 flags)
--  {
--          m_drawFlags = flags;
--  }
--
--  uint32 b2DebugDraw::GetFlags() const
--  {
--          return m_drawFlags;
--  }
--
--  void b2DebugDraw::AppendFlags(uint32 flags)
--  {
--          m_drawFlags |= flags;
--  }
--
--  void b2DebugDraw::ClearFlags(uint32 flags)
--  {
--          m_drawFlags &= ~flags;
--  }


end impact.d2.world_Callbacks;
