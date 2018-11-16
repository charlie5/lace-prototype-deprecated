with impact.d3.Shape.compound;



package body impact.d3.collision.Algorithm.activating.compound
is



   function to_compound_Algorithm (ci           : in     AlgorithmConstructionInfo;
                                   body0, body1 : access impact.d3.Object.item'Class;
                                   isSwapped    : in     Boolean                                                ) return Item'Class
   is
      use impact.d3.collision.Algorithm.activating;

      Self : Item := (Algorithm.activating.item with   -- to_activating_Algorithm (ci, body0, body1) with
                      m_isSwapped      => isSwapped,
                      m_sharedManifold => ci.m_manifold,
                      m_ownsManifold   => False,
                      others => <>);

      function get_colObj return access impact.d3.Object.item'Class
      is
      begin
         if isSwapped then return body1;
         else return body0;
         end if;
      end get_colObj;


      type compound_Shape_view is access all impact.d3.Shape.compound.item'Class;

      colObj        : constant access impact.d3.Object.item'Class := get_colObj;                 pragma Assert (colObj.getCollisionShape.isCompound);
      compoundShape :        compound_Shape_view                := compound_Shape_view (colObj.getCollisionShape);
   begin
      Self.define (ci, body0, body1);
      return Self;
   end to_compound_Algorithm;


   --  impact.d3.collision.Algorithm.activating.compound::
   --  impact.d3.collision.Algorithm.activating.compound (const impact.d3.collision.AlgorithmConstructionInfo& ci,impact.d3.Object* body0,impact.d3.Object* body1,bool isSwapped)
   --  :impact.d3.collision.Algorithm.activating (ci,body0,body1),
   --  m_isSwapped (isSwapped),
   --  m_sharedManifold (ci.m_manifold)
   --  {
   --          m_ownsManifold = false;
   --
   --          impact.d3.Object* colObj = m_isSwapped? body1 : body0;
   --          btAssert (colObj->getCollisionShape()->isCompound());
   --
   --          impact.d3.Shape.compound* compoundShape = static_cast<impact.d3.Shape.compound*>(colObj->getCollisionShape());
   --          m_compoundShapeRevision = compoundShape->getUpdateRevision();
   --
   --          preallocateChildAlgorithms(body0,body1);
   --  }



   overriding procedure destruct (Self : in out Item)
   is
      pragma Unreferenced (Self);
   begin
      return;
   end destruct;




   overriding procedure processCollision (Self : in out Item;   body0, body1 : access impact.d3.Object.item'Class;
                                                     dispatchInfo : in     impact.d3.Dispatcher.DispatcherInfo;
                                                     resultOut    :    out impact.d3.collision.manifold_Result.item)
   is
   begin
      raise Program_Error with "TBD33";
      return;
   end processCollision;



   overriding function calculateTimeOfImpact (Self : in Item;   body0, body1 : access impact.d3.Object.item'Class;
                                                     dispatchInfo : in     impact.d3.Dispatcher.DispatcherInfo;
                                                     resultOut    : access impact.d3.collision.manifold_Result.item) return math.Real
   is
   begin
      raise Program_Error with "TBD34";
      return 0.0;
   end calculateTimeOfImpact;







   overriding procedure getAllContactManifolds (Self : in out Item;   manifoldArray : out impact.d3.collision.Algorithm.btManifoldArray)
   is
   begin
      raise Program_Error with "TBD";
      return;
   end getAllContactManifolds;


--          virtual        void        getAllContactManifolds(btManifoldArray&        manifoldArray)
--          {
--                  int i;
--                  for (i=0;i<m_childCollisionAlgorithms.size();i++)
--                  {
--                          if (m_childCollisionAlgorithms[i])
--                                  m_childCollisionAlgorithms[i]->getAllContactManifolds(manifoldArray);
--                  }
--          }











   --- Create Functions
   --




--  class impact.d3.collision.Algorithm.activating.compound  : public impact.d3.collision.Algorithm.activating
--  {
--  public:



--          struct CreateFunc :public         impact.d3.collision.AlgorithmCreateFunc
--          {
--                  virtual        impact.d3.collision.Algorithm* CreateCollisionAlgorithm(impact.d3.collision.AlgorithmConstructionInfo& ci, impact.d3.Object* body0,impact.d3.Object* body1)
--                  {
--                          void* mem = ci.m_dispatcher1->allocateCollisionAlgorithm(sizeof(impact.d3.collision.Algorithm.activating.compound));
--                          return new(mem) impact.d3.collision.Algorithm.activating.compound(ci,body0,body1,false);
--                  }
--          };



--          struct SwappedCreateFunc :public         impact.d3.collision.AlgorithmCreateFunc
--          {
--                  virtual        impact.d3.collision.Algorithm* CreateCollisionAlgorithm(impact.d3.collision.AlgorithmConstructionInfo& ci, impact.d3.Object* body0,impact.d3.Object* body1)
--                  {
--                          void* mem = ci.m_dispatcher1->allocateCollisionAlgorithm(sizeof(impact.d3.collision.Algorithm.activating.compound));
--                          return new(mem) impact.d3.collision.Algorithm.activating.compound(ci,body0,body1,true);
--                  }
--          };
--
--  };


   overriding function CreateCollisionAlgorithm (Self : in CreateFunc;   ci           : in     AlgorithmConstructionInfo;
                                      body0, body1 : access impact.d3.Object.item'Class) return impact.d3.Dispatcher.Algorithm_view
   is
   begin
      raise Program_Error with "TBD35";
      return null;
   end CreateCollisionAlgorithm;




   overriding function CreateCollisionAlgorithm (Self : in SwappedCreateFunc;   ci           : in     AlgorithmConstructionInfo;
                                                                     body0, body1 : access impact.d3.Object.item'Class) return impact.d3.Dispatcher.Algorithm_view
   is
   begin
      raise Program_Error with "TBD36";
      return null;
   end CreateCollisionAlgorithm;










   procedure removeChildAlgorithms      (Self : in out Item)
   is
   begin
      raise Program_Error with "TBD37";
      return;
   end removeChildAlgorithms;




   procedure preallocateChildAlgorithms (Self : in out Item;   body0, body1 : access  impact.d3.Object.item'Class)
   is
   begin
      raise Program_Error with "TBD38";
      return;
   end preallocateChildAlgorithms;



end impact.d3.collision.Algorithm.activating.compound;



--  #include "BulletCollision/CollisionDispatch/impact.d3.collision.Algorithm.activating.compound.h"
--  #include "BulletCollision/CollisionDispatch/impact.d3.Object.h"
--  #include "BulletCollision/CollisionShapes/impact.d3.Shape.compound.h"
--  #include "BulletCollision/BroadphaseCollision/impact.d3.collision.bounding_volume_Tree.h"
--  #include "LinearMath/btIDebugDraw.h"
--  #include "LinearMath/btAabbUtil2.h"
--  #include "impact.d3.collision.manifold_Result.h"



--  impact.d3.collision.Algorithm.activating.compound::impact.d3.collision.Algorithm.activating.compound( const impact.d3.collision.AlgorithmConstructionInfo& ci,impact.d3.Object* body0,impact.d3.Object* body1,bool isSwapped)
--  :impact.d3.collision.Algorithm.activating(ci,body0,body1),
--  m_isSwapped(isSwapped),
--  m_sharedManifold(ci.m_manifold)
--  {
--          m_ownsManifold = false;
--
--          impact.d3.Object* colObj = m_isSwapped? body1 : body0;
--          btAssert (colObj->getCollisionShape()->isCompound());
--
--          impact.d3.Shape.compound* compoundShape = static_cast<impact.d3.Shape.compound*>(colObj->getCollisionShape());
--          m_compoundShapeRevision = compoundShape->getUpdateRevision();
--
--          preallocateChildAlgorithms(body0,body1);
--  }



--  void        impact.d3.collision.Algorithm.activating.compound::preallocateChildAlgorithms(impact.d3.Object* body0,impact.d3.Object* body1)
--  {
--          impact.d3.Object* colObj = m_isSwapped? body1 : body0;
--          impact.d3.Object* otherObj = m_isSwapped? body0 : body1;
--          btAssert (colObj->getCollisionShape()->isCompound());
--
--          impact.d3.Shape.compound* compoundShape = static_cast<impact.d3.Shape.compound*>(colObj->getCollisionShape());
--
--          int numChildren = compoundShape->getNumChildShapes();
--          int i;
--
--          m_childCollisionAlgorithms.resize(numChildren);
--          for (i=0;i<numChildren;i++)
--          {
--                  if (compoundShape->getDynamicAabbTree())
--                  {
--                          m_childCollisionAlgorithms[i] = 0;
--                  } else
--                  {
--                          impact.d3.Shape* tmpShape = colObj->getCollisionShape();
--                          impact.d3.Shape* childShape = compoundShape->getChildShape(i);
--                          colObj->internalSetTemporaryCollisionShape( childShape );
--                          m_childCollisionAlgorithms[i] = m_dispatcher->findAlgorithm(colObj,otherObj,m_sharedManifold);
--                          colObj->internalSetTemporaryCollisionShape( tmpShape );
--                  }
--          }
--  }



--  void        impact.d3.collision.Algorithm.activating.compound::removeChildAlgorithms()
--  {
--          int numChildren = m_childCollisionAlgorithms.size();
--          int i;
--          for (i=0;i<numChildren;i++)
--          {
--                  if (m_childCollisionAlgorithms[i])
--                  {
--                          m_childCollisionAlgorithms[i]->~impact.d3.collision.Algorithm();
--                          m_dispatcher->freeCollisionAlgorithm(m_childCollisionAlgorithms[i]);
--                  }
--          }
--  }



--  impact.d3.collision.Algorithm.activating.compound::~impact.d3.collision.Algorithm.activating.compound()
--  {
--          removeChildAlgorithms();
--  }
--
--
--








--  struct        btCompoundLeafCallback : impact.d3.collision.bounding_volume_Tree::ICollide
--  {
--
--  public:
--
--          impact.d3.Object* m_compoundColObj;
--          impact.d3.Object* m_otherObj;
--          impact.d3.Dispatcher* m_dispatcher;
--          const impact.d3.DispatcherInfo& m_dispatchInfo;
--          impact.d3.collision.manifold_Result*        m_resultOut;
--          impact.d3.collision.Algorithm**        m_childCollisionAlgorithms;
--          impact.d3.Manifold*        m_sharedManifold;




--          btCompoundLeafCallback (impact.d3.Object* compoundObj,impact.d3.Object* otherObj,impact.d3.Dispatcher* dispatcher,const impact.d3.DispatcherInfo& dispatchInfo,impact.d3.collision.manifold_Result*        resultOut,impact.d3.collision.Algorithm**        childCollisionAlgorithms,impact.d3.Manifold*        sharedManifold)
--                  :m_compoundColObj(compoundObj),m_otherObj(otherObj),m_dispatcher(dispatcher),m_dispatchInfo(dispatchInfo),m_resultOut(resultOut),
--                  m_childCollisionAlgorithms(childCollisionAlgorithms),
--                  m_sharedManifold(sharedManifold)
--          {
--
--          }



--          void        ProcessChildShape(impact.d3.Shape* childShape,int index)
--          {
--                  btAssert(index>=0);
--                  impact.d3.Shape.compound* compoundShape = static_cast<impact.d3.Shape.compound*>(m_compoundColObj->getCollisionShape());
--                  btAssert(index<compoundShape->getNumChildShapes());
--
--
--                  //backup
--                  impact.d3.Transform        orgTrans = m_compoundColObj->getWorldTransform();
--                  impact.d3.Transform        orgInterpolationTrans = m_compoundColObj->getInterpolationWorldTransform();
--                  const impact.d3.Transform& childTrans = compoundShape->getChildTransform(index);
--                  impact.d3.Transform        newChildWorldTrans = orgTrans*childTrans ;
--
--                  //perform an AABB check first
--                  impact.d3.Vector aabbMin0,aabbMax0,aabbMin1,aabbMax1;
--                  childShape->getAabb(newChildWorldTrans,aabbMin0,aabbMax0);
--                  m_otherObj->getCollisionShape()->getAabb(m_otherObj->getWorldTransform(),aabbMin1,aabbMax1);
--
--                  if (TestAabbAgainstAabb2(aabbMin0,aabbMax0,aabbMin1,aabbMax1))
--                  {
--
--                          m_compoundColObj->setWorldTransform( newChildWorldTrans);
--                          m_compoundColObj->setInterpolationWorldTransform(newChildWorldTrans);
--
--                          //the contactpoint is still projected back using the original inverted worldtrans
--                          impact.d3.Shape* tmpShape = m_compoundColObj->getCollisionShape();
--                          m_compoundColObj->internalSetTemporaryCollisionShape( childShape );
--
--                          if (!m_childCollisionAlgorithms[index])
--                                  m_childCollisionAlgorithms[index] = m_dispatcher->findAlgorithm(m_compoundColObj,m_otherObj,m_sharedManifold);
--
--                          ///detect swapping case
--                          if (m_resultOut->getBody0Internal() == m_compoundColObj)
--                          {
--                                  m_resultOut->setShapeIdentifiersA(-1,index);
--                          } else
--                          {
--                                  m_resultOut->setShapeIdentifiersB(-1,index);
--                          }
--
--                          m_childCollisionAlgorithms[index]->processCollision(m_compoundColObj,m_otherObj,m_dispatchInfo,m_resultOut);
--                          if (m_dispatchInfo.m_debugDraw && (m_dispatchInfo.m_debugDraw->getDebugMode() & btIDebugDraw::DBG_DrawAabb))
--                          {
--                                  impact.d3.Vector worldAabbMin,worldAabbMax;
--                                  m_dispatchInfo.m_debugDraw->drawAabb(aabbMin0,aabbMax0,impact.d3.Vector(1,1,1));
--                                  m_dispatchInfo.m_debugDraw->drawAabb(aabbMin1,aabbMax1,impact.d3.Vector(1,1,1));
--                          }
--
--                          //revert back transform
--                          m_compoundColObj->internalSetTemporaryCollisionShape( tmpShape);
--                          m_compoundColObj->setWorldTransform(  orgTrans );
--                          m_compoundColObj->setInterpolationWorldTransform(orgInterpolationTrans);
--                  }
--          }


--          void                Process(const impact.d3.collision.bounding_volume_TreeNode* leaf)
--          {
--                  int index = leaf->dataAsInt;
--
--                  impact.d3.Shape.compound* compoundShape = static_cast<impact.d3.Shape.compound*>(m_compoundColObj->getCollisionShape());
--                  impact.d3.Shape* childShape = compoundShape->getChildShape(index);
--                  if (m_dispatchInfo.m_debugDraw && (m_dispatchInfo.m_debugDraw->getDebugMode() & btIDebugDraw::DBG_DrawAabb))
--                  {
--                          impact.d3.Vector worldAabbMin,worldAabbMax;
--                          impact.d3.Transform        orgTrans = m_compoundColObj->getWorldTransform();
--                          impact.d3.TransformAabb(leaf->volume.Mins(),leaf->volume.Maxs(),0.,orgTrans,worldAabbMin,worldAabbMax);
--                          m_dispatchInfo.m_debugDraw->drawAabb(worldAabbMin,worldAabbMax,impact.d3.Vector(1,0,0));
--                  }
--                  ProcessChildShape(childShape,index);
--
--          }
--  };








--  void impact.d3.collision.Algorithm.activating.compound::processCollision (impact.d3.Object* body0,impact.d3.Object* body1,const impact.d3.DispatcherInfo& dispatchInfo,impact.d3.collision.manifold_Result* resultOut)
--  {
--          impact.d3.Object* colObj = m_isSwapped? body1 : body0;
--          impact.d3.Object* otherObj = m_isSwapped? body0 : body1;
--
--
--
--          btAssert (colObj->getCollisionShape()->isCompound());
--          impact.d3.Shape.compound* compoundShape = static_cast<impact.d3.Shape.compound*>(colObj->getCollisionShape());
--
--          ///impact.d3.Shape.compound might have changed:
--          ////make sure the internal child collision algorithm caches are still valid
--          if (compoundShape->getUpdateRevision() != m_compoundShapeRevision)
--          {
--                  ///clear and update all
--                  removeChildAlgorithms();
--
--                  preallocateChildAlgorithms(body0,body1);
--          }
--
--
--          impact.d3.collision.bounding_volume_Tree* tree = compoundShape->getDynamicAabbTree();
--          //use a dynamic aabb tree to cull potential child-overlaps
--          btCompoundLeafCallback  callback(colObj,otherObj,m_dispatcher,dispatchInfo,resultOut,&m_childCollisionAlgorithms[0],m_sharedManifold);
--
--          ///we need to refresh all contact manifolds
--          ///note that we should actually recursively traverse all children, impact.d3.Shape.compound can nested more then 1 level deep
--          ///so we should add a 'refreshManifolds' in the impact.d3.collision.Algorithm
--          {
--                  int i;
--                  btManifoldArray manifoldArray;
--                  for (i=0;i<m_childCollisionAlgorithms.size();i++)
--                  {
--                          if (m_childCollisionAlgorithms[i])
--                          {
--                                  m_childCollisionAlgorithms[i]->getAllContactManifolds(manifoldArray);
--                                  for (int m=0;m<manifoldArray.size();m++)
--                                  {
--                                          if (manifoldArray[m]->getNumContacts())
--                                          {
--                                                  resultOut->setPersistentManifold(manifoldArray[m]);
--                                                  resultOut->refreshContactPoints();
--                                                  resultOut->setPersistentManifold(0);//??necessary?
--                                          }
--                                  }
--                                  manifoldArray.resize(0);
--                          }
--                  }
--          }
--
--          if (tree)
--          {
--
--                  impact.d3.Vector localAabbMin,localAabbMax;
--                  impact.d3.Transform otherInCompoundSpace;
--                  otherInCompoundSpace = colObj->getWorldTransform().inverse() * otherObj->getWorldTransform();
--                  otherObj->getCollisionShape()->getAabb(otherInCompoundSpace,localAabbMin,localAabbMax);
--
--                  const ATTRIBUTE_ALIGNED16(impact.d3.collision.bounding_volume_TreeVolume)        bounds=impact.d3.collision.bounding_volume_TreeVolume::FromMM(localAabbMin,localAabbMax);
--                  //process all children, that overlap with  the given AABB bounds
--                  tree->collideTV(tree->m_root,bounds,callback);
--
--          } else
--          {
--                  //iterate over all children, perform an AABB check inside ProcessChildShape
--                  int numChildren = m_childCollisionAlgorithms.size();
--                  int i;
--                  for (i=0;i<numChildren;i++)
--                  {
--                          callback.ProcessChildShape(compoundShape->getChildShape(i),i);
--                  }
--          }
--
--          {
--                                  //iterate over all children, perform an AABB check inside ProcessChildShape
--                  int numChildren = m_childCollisionAlgorithms.size();
--                  int i;
--                  btManifoldArray        manifoldArray;
--          impact.d3.Shape* childShape = 0;
--          impact.d3.Transform        orgTrans;
--          impact.d3.Transform        orgInterpolationTrans;
--          impact.d3.Transform        newChildWorldTrans;
--          impact.d3.Vector aabbMin0,aabbMax0,aabbMin1,aabbMax1;
--
--                  for (i=0;i<numChildren;i++)
--                  {
--                          if (m_childCollisionAlgorithms[i])
--                          {
--                                  childShape = compoundShape->getChildShape(i);
--                          //if not longer overlapping, remove the algorithm
--                  orgTrans = colObj->getWorldTransform();
--                  orgInterpolationTrans = colObj->getInterpolationWorldTransform();
--                                  const impact.d3.Transform& childTrans = compoundShape->getChildTransform(i);
--                  newChildWorldTrans = orgTrans*childTrans ;
--
--                                  //perform an AABB check first
--                                  childShape->getAabb(newChildWorldTrans,aabbMin0,aabbMax0);
--                                  otherObj->getCollisionShape()->getAabb(otherObj->getWorldTransform(),aabbMin1,aabbMax1);
--
--                                  if (!TestAabbAgainstAabb2(aabbMin0,aabbMax0,aabbMin1,aabbMax1))
--                                  {
--                                          m_childCollisionAlgorithms[i]->~impact.d3.collision.Algorithm();
--                                          m_dispatcher->freeCollisionAlgorithm(m_childCollisionAlgorithms[i]);
--                                          m_childCollisionAlgorithms[i] = 0;
--                                  }
--                          }
--                  }
--          }
--  }







--  impact.d3.Scalar        impact.d3.collision.Algorithm.activating.compound::calculateTimeOfImpact(impact.d3.Object* body0,impact.d3.Object* body1,const impact.d3.DispatcherInfo& dispatchInfo,impact.d3.collision.manifold_Result* resultOut)
--  {
--
--          impact.d3.Object* colObj = m_isSwapped? body1 : body0;
--          impact.d3.Object* otherObj = m_isSwapped? body0 : body1;
--
--          btAssert (colObj->getCollisionShape()->isCompound());
--
--          impact.d3.Shape.compound* compoundShape = static_cast<impact.d3.Shape.compound*>(colObj->getCollisionShape());
--
--          //We will use the OptimizedBVH, AABB tree to cull potential child-overlaps
--          //If both proxies are Compound, we will deal with that directly, by performing sequential/parallel tree traversals
--          //given Proxy0 and Proxy1, if both have a tree, Tree0 and Tree1, this means:
--          //determine overlapping nodes of Proxy1 using Proxy0 AABB against Tree1
--          //then use each overlapping node AABB against Tree0
--          //and vise versa.
--
--          impact.d3.Scalar hitFraction = impact.d3.Scalar(1.);
--
--          int numChildren = m_childCollisionAlgorithms.size();
--          int i;
--      impact.d3.Transform        orgTrans;
--      impact.d3.Scalar frac;
--          for (i=0;i<numChildren;i++)
--          {
--                  //temporarily exchange parent impact.d3.Shape with childShape, and recurse
--                  impact.d3.Shape* childShape = compoundShape->getChildShape(i);
--
--                  //backup
--          orgTrans = colObj->getWorldTransform();
--
--                  const impact.d3.Transform& childTrans = compoundShape->getChildTransform(i);
--                  //impact.d3.Transform        newChildWorldTrans = orgTrans*childTrans ;
--                  colObj->setWorldTransform( orgTrans*childTrans );
--
--                  impact.d3.Shape* tmpShape = colObj->getCollisionShape();
--                  colObj->internalSetTemporaryCollisionShape( childShape );
--          frac = m_childCollisionAlgorithms[i]->calculateTimeOfImpact(colObj,otherObj,dispatchInfo,resultOut);
--                  if (frac<hitFraction)
--                  {
--                          hitFraction = frac;
--                  }
--                  //revert back
--                  colObj->internalSetTemporaryCollisionShape( tmpShape);
--                  colObj->setWorldTransform( orgTrans);
--          }
--          return hitFraction;
--
--  }



