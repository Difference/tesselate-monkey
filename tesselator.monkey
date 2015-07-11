Strict
 

' tesselate.monkey
' Poly earclipper that deals with holes
' Draw lines and  polylines with linewidth and miter/bevel joints
' () 2014.02.24 - Peter Scheutz aka Difference

' v10 - 2015.06.19 
' Renamed MakeTriangles to MakeIndexedTriangles, made MakeTriangles return Unpacked triangles

' v09 - 2014.03.29 
' Added Class IndexedTriangles, removed Tesselator Class
' Made most of the module private, leaving only the Make Triangles functions and IndexedTriangles Class public.
' Dropped calling CleanUpPoly so If you get strange results, try using CleanUpPoly() on your polygons before
' sending them to MakeTriangles()

' v08 - 2014.03.12 new inner protrusion check
' added angle limit befor making a joint
' changed a lot of signs and reversed IsLeft() - wish maybe wrong but works here!

' v07b - 2014.03.12 added a few safty checks
' v0.7 - 2014.03.11
' major rework of the polyline function
' miter-limit and bevel joints are in 
' Square captype is now working


' v0.6b - 2014.03.07
' Thick line added, (miter joins only for now)
' added params to TriangulateAndDrawPolyline()
 
' v0.5 - 2014.03.01
' New method for merging holes, see comments in HolMerge for strategy
' Moved the functions to Tesselator Class to wrap the calls 

' v0.3 - 2014.02.26 
'	 - changed Tessellate() To a While loop, keeping problem points For examination 
'		- better detection of identical points in incomming poly
' 		- check for intersection with all polys in HoleMerge
'		- check that two adjacent lines connects from outer to inner poly (only use one)
'
' 2014.02.24 
' v0.2 - first release version

#rem
The MIT License (MIT)

Copyright (c)  2014   Peter Scheutz , portions by Darel Rex Finley, Nathan Mercer, Paul Bourke

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
#end

'Import mojo2
 
Public 


' This is the class the indexed triangles are stored in
' Use an instance of this class in your own code
' 
Class IndexedTriangles
	Field points:Float[]
	Field indexes:Int[]
	Field texcoords:Float[]
	
	'Private
	Method FixIndexes:Void()
		For Local n:Int = 0 Until indexes.Length()  
 			indexes[n] = indexes[n]/2
		Next
	End Method
	
	' for use with Mojo 2 DrawPrimitives
	
	'Public
	Method UnPack:Float[]()
	
		Local triangles:Float[indexes.Length()*2]
	
		Local i:Int = 0
	
		For Local n:Int = 0 Until indexes.Length() Step 3
			triangles[i] = points[indexes[n]]
			triangles[i+1] = points[indexes[n]+1]
	
			triangles[i+2] = points[indexes[n+1]]
			triangles[i+3] = points[indexes[n+1]+1]			
	
			triangles[i+4] = points[indexes[n+2]]
			triangles[i+5] = points[indexes[n+2]+1]		
			i +=6
			
		Next
	
		Return triangles
	
	End Method
	
	
	
	
	Method Clone:IndexedTriangles()
	
		Local it:= New IndexedTriangles
		
		it.points = New Float[points.Length()]
		it.indexes = New Int[indexes.Length()]

	
		For Local i:Int = 0 Until points.Length() 
			it.points[i]  =  points[i] 
		Next
	
		For Local i:Int = 0 Until indexes.Length() 
			it.indexes[i]  =  indexes[i] 
		Next	
		
		Return it
		
	End Method	
	
 	
	Method Scale:Void(s:Float)
		For Local i:Int = 0 Until points.Length() 
			points[i] *= s			
		Next
	End Method
	
	
	Method MakeTextureCords:Void(offx:Float=0,offy:Float=0,scalex:Float=0,scaley:Float=0)
	
	
	
		Local minx:Float = 1000000.0
		Local maxx:Float =  -1000000.0
		Local miny:Float =  1000000.0
		Local maxy:Float =  -1000000.0
	
	
		For Local i:Int = 0 Until points.Length() Step 2
			If points[i] < minx Then minx = points[i]
			If points[i+1] < miny Then miny = points[i+1]
			If points[i] > maxx Then maxx = points[i]
			If points[i+1] > maxy Then maxy = points[i+1]
		Next
		
		Local magx:Float = (maxx-minx)
		Local magy:Float = (maxy-miny)
		
		texcoords = New Float[points.Length]
		
		For Local i:Int = 0 Until points.Length() Step 2
		
			Local u:Float =   scalex*(points[i] - minx) / magx  + offx
			Local v:Float =  scaley*(points[i+1] - miny) / magy  + offy
		
			texcoords[i] = u 
			texcoords[i+1] = v
		Next
		 
		
	End Method

	Method Offset:Void(x:Float,y:Float)
		For Local i:Int = 0 Until points.Length() Step 2
			points[i] += x
			points[i+1] += y
		Next
	End Method

	Method Draw:Void()		
		DrawTriangles(points,indexes)
	End Method	

End Class


Function MakeTriangles:Float[](polys:Float[][])
	Local indexedTris:IndexedTriangles = MakeIndexedTriangles(polys)
	Return indexedTris.UnPack()
End Function

' Expects an array of an array of floats.
' each array is a polygon, the outermost first.
' if Count> 1 then the following polys are expected to be holes
' The holes are expectid to be winded opposite the first.
Function MakeIndexedTriangles:IndexedTriangles(polys:Float[][])

 	Local compoundpoly:= New CompoundPolygon(polys)
 	
 	compoundpoly.itri.FixIndexes()
 	
 	Return compoundpoly.itri 		
End Function	


' Expects a single polygon.
' Multiple tight inner cornes will result in small overlaps in triangles
' This will only show if the line is rendered with alpha<1.0
Function MakeLineTriangles:IndexedTriangles(points:Float[],linewidth:Float,closed:Bool,jointype:Int=0,captype:Int=0,miterlimit:Float=4.0)

		'Print "MakeLineTriangles"


		If miterlimit<1 Then miterlimit = 1
		Local fatline:= New  ThickPolyline(points,linewidth,closed,jointype,captype,miterlimit)
		Return fatline.itri

End Function
 

' Helper function to remove identical points. Call before sending your poly to MakeTriangles() if you get triangulation errors
Function CleanUpPolygon:float[](poly:float[])

	Local skiplist:= New IntList	
			
	Repeat
		skiplist.Clear()
	
		' Create a list af adjacent vertices that are identical to the previus vertex
		' we need to remove them or they will confuse the earclipper
		If poly.Length()>4
	
			'Local p3:Int = poly.Length() - 4
			Local p2:Int = poly.Length() - 2
			
			For Local p1:Int = 0 Until poly.Length() Step 2
			
 				If PointsAreIdentical(poly[p1],poly[p1+1],poly[p2],poly[p2+1])
					skiplist.AddLast p2					
				Endif

				p2 = p1
				
			Next	
		
			Local skip:Int
			
			For Local n:Int = 0 Until poly.Length() Step 2				
				If skiplist.Contains(n)
					skip +=2
				Endif
			
				If n+skip<poly.Length()
					poly[n] = poly[n+skip] 
					poly[n+1] = poly[n+skip+1] 
				Endif
			
			Next
			
			 If skip Then 				 
			 	' truncate array to new length()
				poly = poly[..(poly.Length()-skip)]
			Endif
		
		Endif
	Until  skiplist.IsEmpty()  	

	Return poly

End Function 
 
 
' ************* Everything below this line is internal to the tesselator
' Don't use it or rely on the functions in it
 
Private 

Global gTess_IntersecsX:Float
Global gTess_IntersecsY:Float
Const EPS:Float = 0.05
 

Function PointsAreIdentical:Bool(x1:Float,y1:Float,x2:Float,y2:float)
 
	' Manhatten distance should do
	If Abs(x1-x2) < EPS
		If Abs(y1-y2) < EPS	 		
			Return True
		Endif
	Endif

	Return False
End Function

 

  
'Polygon with holes 
Class CompoundPolygon 
 	Field polyindexes:= New Deque<IndexedPolygon> 	
 	Field itri:= New IndexedTriangles  
   
 
	Method New(newpoints:Float[][])
		Build newpoints
	End Method

	Method Build:Void(newpoints:Float[][])
		
		
		itri.points =  FloatArraysMerge(newpoints)	
		
 	
		'make polygons that references the points in the pool
		' each value is an offset to the x coordinate in the pool
		Local index:Int
		For Local i:Int = 0 Until newpoints.Length()
		
			Local ipoly:= New IndexedPolygon(itri.points)  
		 
			polyindexes.PushLast ipoly
			
			For Local n:Int = 0 Until newpoints[i].Length()-1 Step 2			
				ipoly.Push(index)
				index += 2
			Next
		Next
		
 		Local mergedpoly:IndexedPolygon		
 	 
		' merge holes and polygon
		' OBS for now assume all polys with index> 0 are holes 
		' start by adding first poly, then merge holes	
		mergedpoly  = polyindexes.PopFirst()
		
		While polyindexes.Length()
			Local hole:= polyindexes.PopFirst()
			mergedpoly = HoleMerge(itri.points,mergedpoly,hole,polyindexes)
		wend
			
		If mergedpoly.Length()<2 Then Return
 
 		' triangulate it
		Tessellate(itri.points,mergedpoly)
 
	End Method
 
	
	Method Tessellate:Void(points:Float[],indexedpoly:IndexedPolygon)
 	
		' a polygon can be triangulated with vertexcount - 2 triangles
		Local trianglecount:int = indexedpoly.Length() - 2
		
		' add room for bridges
		If indexedpoly.bridges
			trianglecount +=  indexedpoly.bridges.Length()/3
		endif
		
		If trianglecount<=0 Then Return 
 	
		' an arrry for triangleindexes to be passed to DrawTriangles()
		itri.indexes = New Int[trianglecount*3]	
 
		Local i:int
 
		While indexedpoly.ClipEar()
 			itri.indexes[i] = indexedpoly.ear[0]
			itri.indexes[i+1] = indexedpoly.ear[1]
			itri.indexes[i+2] = indexedpoly.ear[2]	
			i +=3
		Wend
	
		If indexedpoly.bridges   
			While indexedpoly.bridges.Length() 
				itri.indexes[i+2] = indexedpoly.bridges.Pop()
				itri.indexes[i+1] = indexedpoly.bridges.Pop()		
				itri.indexes[i] = indexedpoly.bridges.Pop()	
				i +=3	
			Wend
		endif	
		
		' did we loose some in the fire?
		If i < itri.indexes.Length()
			itri.indexes = itri.indexes[..i]					
			'trianglecount = i/3
		Endif
 
	End Method
 	
End Class

	

Class IndexedPolygon Extends IntStack

	Field bridges:IntStack

	Field pool:Float[]
	Field area:float
	Field winding:Int
	Field prepared:Bool

	Field ear:Int[3]

	Method New(points:Float[])		
		pool = points		
	End Method 
 	
	 ' determine winding
	Method Prepare:Void()
	
		If   pool.Length()<3 Then
			Error "IndexedPolygon must have a pool"
		Endif		
		area = PolygonArea(pool,Self)
		winding = Sgn(area)
		
		prepared = true
	End Method
 
		
	' Return True if ear was clipped
	' clipped ear indexes is in ear array	
	Method ClipEar:Bool()
		If Self.Length() <3 Then Return False
 	
		If Not prepared Then Prepare()
 
		Local attempts:Int = Self.Length()
  
		For Local r:Int = 0 To attempts
			Local f:Int = Self.Pop() 

				ear[0] = Self.Top()
				ear[1] = f
				ear[2] = Self.Get(0)
				
				Local a:Float = PolygonArea(pool,ear)
				
				If (Sgn(a) = winding) 'Or Sgn(a) = 0 ' check triangle winding to ignore concave triangles (they are not ears)					
					If NoPointsInTriangle(pool,Self,ear) ' check that no point fron the main poly is in the ear (then it does not intersect)
						Return True
					Endif
	 			
				Endif
				Self.Insert(0,f)  ' roll polygon
				
			Next
		
		Return false
		
	End Method	

End Class



Function PointInPolygon:Int(points:Float[],indexes:Int[],x:Float,y:Float) 

	Local  j:Int = indexes[indexes.Length()-1]
  	Local oddNodes:Int 

	For Local i:= Eachin indexes
		If (points[i+1]< y And points[j+1]>=y ) Or (points[j+1]< y And points[i+1]>=y) 
			If  (points[i]<=x Or points[j]<=x) 
				If (points[i]+(y-points[i+1])/(points[j+1]-points[i+1])*(points[j]-points[i])<x) 
					oddNodes = 1 ~ oddNodes 
				Endif
			Endif
		Endif
		
		j=i 
	Next

  Return oddNodes

End Function



Function PolygonArea:Float(pool:Float[],poly:Int[])

	Local accum:Float
	Local  j:Int = poly[poly.Length()-1]

	For Local i:= Eachin poly
		accum += pool[j] * pool[i+1] - pool[i] * pool[j+1]
    	j=i 
	Next

	Return accum / 2.0

End Function



Function PolygonArea:Float(pool:Float[],poly:IndexedPolygon)

	Local accum:Float
	Local  j:Int = poly.Top()

	For Local i:=Eachin poly
		accum += pool[j] * pool[i+1] - pool[i] * pool[j+1]
    	j=i 
	Next

	Return accum / 2.0

End Function


Function PolygonArea:Float(polyPoints:Float[])

	Local accum:Float
	Local  j:Int = polyPoints.Length()-2

	For Local i:Int =0 Until polyPoints.Length() Step 2
		accum += polyPoints[j] * polyPoints[i+1] - polyPoints[i] * polyPoints[j+1]
    	j=i 
	Next

	Return accum / 2.0

End Function



'Function params: pointpool , intarray/stack with indexes and the 3 triangle indexes to ignore
Function NoPointsInTriangle:Bool(points:Float[],poly:IntStack,tri:Int[])

	For Local i:= Eachin poly
		If (i <> tri[0]) 
			If (i <> tri[1]) 
				If (i <> tri[2])
				
					Local doit:Bool = true
		
					If points[i]< points[tri[0]]
						If points[i]< points[tri[1]]
							If points[i]< points[tri[2]]
								doit =  false	
							Endif
						Endif
					Endif
 					If doit
						If points[i]> points[tri[0]]
							If points[i]> points[tri[1]]
								If points[i]> points[tri[2]]
										doit =  false
								Endif
							Endif
						Endif
					
						If doit
	
							If points[i+1]< points[tri[0]+1]
								If points[i+1]< points[tri[1]+1]
									If points[i+1]< points[tri[2]+1]
										doit =  false
									Endif
								Endif
							Endif	
						If doit
								If points[i+1]> points[tri[0]+1]
									If points[i+1]> points[tri[1]+1]
										If points[i+1]> points[tri[2]+1]
											doit =  false	
										Endif
									Endif
								Endif		
							endif
						endif	
							
 					endif
				If doit
			 	'	If pointintrianglewarpy(points[i],points[i+1],  points[tri[0]],points[tri[0]+1],  points[tri[1]],points[tri[1]+1],  points[tri[2]],points[tri[2]+1])
				
					' Return False
					
						If  PointInPolygon(points,tri,points[i],points[i+1])
					'		''Print "Point in " + i
				 			Return False
				 		Endif
			 		endif
			
				Endif
			Endif
		Endif
	Next 
	Return True
End Function 



'returns True if p1 and p2 are on the same side of the line a->b
Function sameside:bool(p1x#,p1y#,p2x#,p2y#,ax#,ay#,bx#,by#)
	Local cp1# = (bx-ax)*(p1y-ay)-(p1x-ax)*(by-ay)
	Local cp2# = (bx-ax)*(p2y-ay)-(p2x-ax)*(by-ay)
	If cp1*cp2 >= 0 Then Return True
	Return false
End Function	
	
'Clever little trick for telling if a point is inside a given triangle
'If for each pair of points AB in the triangle, P is on the same side of AB as 
'the other point in the triangle, then P is in the triangle. 
Function pointintrianglewarpy:Bool(px#,py#,ax#,ay#,bx#,by#,cx#,cy#)
	If sameside(px,py,ax,ay,bx,by,cx,cy) And sameside(px,py,bx,by,ax,ay,cx,cy) And sameside(px,py,cx,cy,ax,ay,bx,by)
		Return True
	Else
		Return False
	EndIf
End Function



#rem
Function DotProduct#(x0#,y0#,x1#,y1#,x2#,y2#)
	Return (x1-x0)*(y2-y1)-(x2-x1)*(y1-y0)
End Function

Function InsideTriangle:Bool(px#,py#,x0#,y0#,x1#,y1#,x2#,y2#)
	If DotProduct(x0,y0,x1,y1,px,py)<0
		If DotProduct(x1,y1,x2,y2,px,py)<0
			If DotProduct(x2,y2,x0,y0,px,py)<0
				Return True
			Endif
		Endif
	Endif
	
	Return False
End
#end

'Function inside_trigon(points:Float[],poly:IntStack,tri:Int[])
 
'    Local as_x:Float = s.x-a.x 
 '   Int as_y:Float = s.y-a.y 
'
 '   Bool s_ab:Bool = (b.x-a.x)*as_y-(b.y-a.y)*as_x > 0 
'
 '   If((c.x-a.x)*as_y-(c.y-a.y)*as_x > 0 = s_ab) Return False 
'
 '   If((c.x-b.x)*(s.y-b.y)-(c.y-b.y)*(s.x-b.x) > 0 <> s_ab) Return False 
'
 '   return true;
'End function

'Function inside_trigon(  s:FloatPair,   a:FloatPair,   :FloatPairb,   c:FloatPair)
' 
'    Local as_x:Float = s.x-a.x;
'    int as_y:Float = s.y-a.y;'

 '   Bool s_ab:Bool = (b.x-a.x)*as_y-(b.y-a.y)*as_x > 0 

 '   If((c.x-a.x)*as_y-(c.y-a.y)*as_x > 0 == s_ab) Return False 

 '   If((c.x-b.x)*(s.y-b.y)-(c.y-b.y)*(s.x-b.x) > 0 != s_ab) Return False 

 '   return true;
'End function

' find 

Function FloatArraysMerge:Float[](arr:Float[][] )

	Local marr:Float[]
	Local offset:int

	For Local n:Int = 0 Until arr.Length()
		
		Local taillen:Int = arr[n].Length()
		
		marr = marr.Resize(offset+taillen)
		
		For Local i:Int = 0 Until taillen
			marr[offset+i] = arr[n][i]
		Next
		
		offset = marr.Length()
		
	Next	
	
	Return marr	

End Function

' find two lines that goes from outer to inner poly without crossing either of them or  any other holes
' cut away this piece of the polys and merge them
' save the bridge triangeles in a special pool and add them to the triangulation in the end of tesselate
Function HoleMerge:IndexedPolygon(points:Float[],poly:IndexedPolygon,hole:IndexedPolygon,allpolys:Deque<IndexedPolygon>)

 
	Local ip2:Int	= poly.Length() -1 	
	
	For Local ip1:=0 Until poly.Length()
		
		Local ih2:Int = hole.Length() - 1
		
		For Local ih1:=0 Until hole.Length()
		
		
			Local p1:= poly.Get(ip1)
			Local p2:= poly.Get(ip2)
			Local h1:= hole.Get(ih1)
			Local h2:= hole.Get(ih2)								
 
 	 		' look for two lines
	 		Local bridgefound:Bool = True
	 		
 
			If SegmentTess_IntersectsPoly(p1,h2,poly,points) 
				bridgefound = False
			Elseif SegmentTess_IntersectsPoly(h2,p1,hole,points) 
				bridgefound = False
			Elseif SegmentTess_IntersectsPoly(p2,h1,poly,points) 
				bridgefound = False
			Elseif SegmentTess_IntersectsPoly(h1,p2,hole,points) 
				bridgefound = False
				
			else	
				
				' check the other hole polys too, merge lines can not cross them either
				For Local apoly:= Eachin allpolys
				
					If (apoly <> poly) And (apoly <> hole)
					
						If SegmentTess_IntersectsPoly(p1,h2,apoly,points)
							bridgefound = False
						Elseif SegmentTess_IntersectsPoly(h1,p2,apoly,points) 
							bridgefound = False
						Endif
					endif
				Next
 				
			Endif

 			If bridgefound

				'	'Print "Success - Hole connectors found"
					Local newpoly:= New IndexedPolygon(points) 
					
 					' insert outer poly first:	
					For Local np:Int = ip1 Until poly.Length()
						newpoly.Push poly.Get(np)	
					Next 
					
					For Local np:Int = 0 Until ip1 
						newpoly.Push poly.Get(np)	
					Next 	
 					
					' insert inner poly (assume it's reversed )
					For Local np:Int = ih1  Until hole.Length()
						newpoly.Push hole.Get(np)	
					Next 
					
					For Local np:Int = 0 Until ih1 
						newpoly.Push hole.Get(np)	
					Next 	
					
					'insert "break" point from outer poly again 
					'newpoly.Push hole.Get(ih1)
					If  poly.bridges
						newpoly.bridges = poly.bridges
					Else
						newpoly.bridges = New IntStack
					Endif
 


 					' try this first, if good add the other triangle
 					' otherwise  add the two triangles with the opposite diagonal
					If Not PointInPolygon(points,[p2,p1,h2],points[h1],points[h1+1]) 

 						newpoly.bridges.Push p1
 						newpoly.bridges.Push h2
 						newpoly.bridges.Push p2

 						newpoly.bridges.Push h1 						
						newpoly.bridges.Push p2
 						newpoly.bridges.Push h2
 						
 					Else
 						newpoly.bridges.Push h1 					
			 			newpoly.bridges.Push p2
 						newpoly.bridges.Push p1

			 			newpoly.bridges.Push p1
 						newpoly.bridges.Push h2
 						newpoly.bridges.Push h1

 					Endif
 

					Return newpoly				
				
		 	Endif
			
			ih2 =ih1
			
		Next
		
 		ip2 =ip1
	Next

 	Return  poly

End Function

 
  

'/* from http://paulbourke.net/geometry/lineline2d/
'   Determine the intersection point of two line segments

	'Function returns 
	' 0 both lines are parallel and will never intersect.
	'1 lines are colinear (i.e. the same).
	'2 Tess_Intersects, but outside segnments
	
	 
	'4 both segments intersect 
'*/
Function Intersects:Int(x1:Float, y1:Float, x2:Float, y2:Float, x3:Float, y3:Float, x4:Float, y4:Float )


	Local denom:Float  = (y4-y3) * (x2-x1) - (x4-x3) * (y2-y1)
	Local numera:Float = (x4-x3) * (y1-y3) - (y4-y3) * (x1-x3)
	Local numerb:Float = (x2-x1) * (y1-y3) - (y2-y1) * (x1-x3)

	'/* Are the line coincident? */
	If (Abs(numera) < EPS And Abs(numerb) < EPS And Abs(denom) < EPS) Then
		gTess_IntersecsX = (x1 + x2) / 2
		gTess_IntersecsY = (y1 + y2) / 2
		Return 1
	Endif

	'/* Are the line parallel */
	If (Abs(denom) < EPS) Then
		gTess_IntersecsX = (x1 + x2) / 2
		gTess_IntersecsY = (y1 + y2) / 2
		Return 0
	Endif

	'/* Is the intersection along the the segments */
	Local mua:Float = numera / denom
	Local mub:Float = numerb / denom
	gTess_IntersecsX = x1 + mua * (x2 - x1)
  	gTess_IntersecsY = y1 + mua * (y2 - y1)
 
	If  (mua >= 0.0) And (mua <= 1.0)
		If  (mub >= 0.0) And (mub <= 1.0)
          	Return 4
 
		Endif
	Endif

    Return 2		

End Function


' Function to check if a line segment Tess_Intersects a single polygon
' ignoring the starting point of the line segmet
Function SegmentTess_IntersectsPoly:Bool(s1:Int,s2:Int,poly:IntStack,points:Float[])

		If Not poly.Length() Return False
		
 		Local i2:Int = poly.Top()
		
		For Local i1:= Eachin poly
 		
			If (s1 <> i1) And (s1 <> i2) 
				If Intersects(points[s1],points[s1+1],points[s2],points[s2+1],points[i1],points[i1+1],points[i2],points[i2+1])>2
 			  		 Return True
 				Endif
			Endif	
						
			i2 = i1
		Next
		
		Return False
End Function


' wrapper to check multiple polygons 
Function SegmentTess_IntersectsPolys:Bool(p:Int,h:Int,polys:IndexedPolygon[],points:Float[])
	For Local poly:= Eachin polys
		If SegmentTess_IntersectsPoly(p,h,poly,points)	
			Return True
		Endif
	Next 
	Return False
End Function
 
 

Function CrossProduct:Float(v1x:Float,v1y:Float,v2x:Float,v2y:Float)
    Return (v1x*v2y) - (v1x*v2y)
End Function
 
' determine of point is right or left of directional vector 
Function  IsLeft:Bool(ax:Float,ay:Float,bx:Float,by:Float,cx:Float,cy:Float)
	Return ((bx - ax)*(cy - ay) - (by - ay)*(cx - ax)) < 0 ' beware this might be IsRight to you...
End Function 
 
 
Class ThickPolyline   

	Field polyindexes:= New Deque<IndexedPolygon> 	
 	Field itri:= New IndexedTriangles  
 
	' jointype: miter = 0 , round = 1, bevel = 2	  	 
	' captypes: butt = 0 , round = 1, square = 2		 
	Method New(orgpoints:Float[],linewidth:Float,closed:Bool,jointype:Int,captype:Int,miterlimit:Float)
		If orgpoints.Length() < 4 Then Return  
		Build orgpoints,linewidth,closed,jointype,captype,miterlimit
	End Method


	Method Build:Void(orgpoints:Float[],linewidth:Float,closed:Bool,jointype:Int,captype:Int,miterlimit:Float )
	
		' no round joint yet, so use bevel
		If jointype = 1 Then jointype = 2

		' no round captype yet, so use square
		If captype = 1 Then captype = 2

  	
 		Local tris:= New IntStack
  	
		Local lps:= New FloatStack
		Local rps:= New FloatStack

 	
		Local halfwidth:Float = linewidth / 2.0
  
		Local segmentcount:Int  = orgpoints.Length()/2
		If closed segmentcount +=1

		Local p1:Int = orgpoints.Length() - 2
		Local p2:Int = 0
		Local p3:Int = 2

		
		For Local seg:Int = 1 To segmentcount
	 			
			' two line segments, p2 is the corner that we're making point for  
			Local ax:Float = orgpoints[p1]
			Local ay:Float	= orgpoints[p1+1]		
			Local bx:Float = orgpoints[p2]
			Local by:Float	= orgpoints[p2+1]
			Local cx:Float = orgpoints[p3]
			Local cy:Float	= orgpoints[p3+1]		
			

			Local segmentjointype:Int = jointype ' this is overridden on a per joint basis
 
			' create a vector perpendicular to line for offsettting   line a-b
			Local v1x:Float =  (by-ay)
			Local v1y:Float = -(bx-ax)		
	
			' normalise it
			Local magnitude1:Float = Sqrt(v1x*v1x+v1y*v1y)
			v1x /= magnitude1
			v1y /= magnitude1
	
			'give it thickness
			v1x *= halfwidth
			v1y *= halfwidth
				
			' create a vector perpendicular to line for offsettting   line b-c
			Local v2x:Float =   (cy-by)
			Local v2y:Float =  -(cx-bx)			
	
			' normalise it
			Local magnitude2:Float    = Sqrt(v2x*v2x+v2y*v2y)
 
 			v2x /= magnitude2
			v2y /= magnitude2
	
			'give it thickness
			v2x *= halfwidth
			v2y *= halfwidth	
	
 
 			' get angle before considering bevel or round, of skipping the joint
			Local angle:Float = ATan2(v2y,v2x) - ATan2(v1y,v1x)
			

			Local dothisjoint:Bool = False
			' minimum angle to previous point before inserting a joint
			If Abs(angle) > 1 Then  dothisjoint = True	 
  
 
 			If p2 = 0  Then dothisjoint = True	 ' first point then    
			If seg = segmentcount  dothisjoint = True	 ' last point
 
			If dothisjoint
 
	 			' check if segment b-c is turning left or right
	 			Local isleft:Bool = IsLeft(ax,ay,bx,by,cx,cy)
				
				If isleft 'look at left side intersection
					If Not Intersects(ax+v1x,ay+v1y,bx+v1x,by+v1y,bx+v2x,by+v2y,cx+v2x,cy+v2y)>=2  
						' its a staight line or a point
				 		segmentjointype = 3 ' straight line 
					Endif
				Else	' look at right side intersection
					If Not Intersects(ax-v1x,ay-v1y,bx-v1x,by-v1y,bx-v2x,by-v2y,cx-v2x,cy-v2y)>=2  
						' its a staight line or a point
				 		segmentjointype = 3 ' straight line 
				 	endif
				endif
				
	 			
	 			' base for indexes to the points
	 			' using two pointstacks makes for easier visual debugging
	 			' but results in this slightly convoluted offset trick
	 			' negative indeses are for the right line
	  			Local lindex:Int = lps.Length() - 2 			
	  			Local rindex:Int = rps.Length() - 2 + 1000000
	  			
	  			Local beveltri:Int = 0

				' check miterlimit and mark it as a bevel join if limit is crossed
				If segmentjointype = 0
				
					Local mx:Float =  bx - gTess_IntersecsX	
					Local my:Float =  by - gTess_IntersecsY	
								
					Local dist:Float = Sqrt(mx*mx+my*my)
		
					If dist>miterlimit *linewidth 
						segmentjointype  = 2
					endif
				
				Endif
					
				' cap end point if not closed			
				If Not closed			
					If p2 = 0 ' first point
						segmentjointype = 4	 ' first point	  			
					Endif
				
	
					If seg = segmentcount   
					 	segmentjointype = 5	 ' last point
					Endif
		
				endif		
				
				Select segmentjointype
		
					Case 0	' miter join
						
						If isleft
			
								' mirror the point around point b
								Local meltvx:Float = bx - gTess_IntersecsX	
								Local meltvy:Float = by - gTess_IntersecsY	 
							
								rps.Push  bx + meltvx	 	
								rps.Push  by + meltvy 							
							
	 
								Local meltmagnitude:Float = Sqrt(meltvx*meltvx+meltvy*meltvy) 
	 				 			
	 				 			'fix inner protrutions
	 				 			If meltmagnitude > 2*linewidth  ' this might need ajusting
	 				 			
	 				 		 		meltvx /=meltmagnitude
	 				 				meltvy /=meltmagnitude	
	 				 			
	 				 			
	 				 				meltmagnitude = Min(meltmagnitude,magnitude1)
	 				 				meltmagnitude = Min(meltmagnitude,magnitude2) 				 			
	 				 			
	 				 			
	 				 		 		lps.Push bx - meltvx   * meltmagnitude
							 		lps.Push by - meltvy  	* meltmagnitude 
								Else
									lps.Push gTess_IntersecsX
									lps.Push gTess_IntersecsY
								
								endif
			  
						Else
						
				 				Local meltvx:Float = bx - gTess_IntersecsX	
								Local meltvy:Float = by - gTess_IntersecsY	 
							
							
								lps.Push  bx + meltvx	 	
								lps.Push  by + meltvy 	
	 	 				 			
								Local meltmagnitude:Float = Sqrt(meltvx*meltvx+meltvy*meltvy) 
	 	 			 			
	 	 			 			'fix inner protrutions	
	 	 			 			If meltmagnitude > 2*linewidth  ' this might need ajusting
	 		 						meltvx /=meltmagnitude
		 				 			meltvy /=meltmagnitude
		 				 			
		 				 			meltmagnitude = Min(meltmagnitude,magnitude1)
		 				 			meltmagnitude = Min(meltmagnitude,magnitude2) 				 			
		 				 			
		 				 		 	rps.Push bx - meltvx   * meltmagnitude
								 	rps.Push by - meltvy  	* meltmagnitude
								 	
								 Else
									 rps.Push gTess_IntersecsX
									 rps.Push gTess_IntersecsY
								 endif
	 											
						Endif		
	 			
					Case 2
		 	 
						If isleft
							beveltri = 1	' flag indicates we should add a triangle later
 	
							rps.Push bx - v1x  
							rps.Push by - v1y  
	
		
							rps.Push bx - v2x 	 
							rps.Push by - v2y 	 
							
	  
							Local meltvx:Float = bx - gTess_IntersecsX	
							Local meltvy:Float = by - gTess_IntersecsY	 
							
 							Local meltmagnitude:Float = Sqrt(meltvx*meltvx+meltvy*meltvy) 
	 	 			 			

	 				 		'fix inner protrutions
	 				 		If meltmagnitude >  2*linewidth  ' this might need ajusting

	 				 			meltvx /=meltmagnitude
	 				 			meltvy /=meltmagnitude
	 				 			
	 				 			meltmagnitude = Min(meltmagnitude,magnitude1)
	 				 			meltmagnitude = Min(meltmagnitude,magnitude2) 				 			
	 				 			
 	 				 		 	lps.Push bx - meltvx   * meltmagnitude
							 	lps.Push by - meltvy  	* meltmagnitude 
							Else
								lps.Push gTess_IntersecsX
								lps.Push gTess_IntersecsY
								
							Endif 
	 							
						Else
							beveltri = 2 ' flag indicates we sould add a triangle later
							
							
					 			Local meltvx:Float = bx - gTess_IntersecsX	
								Local meltvy:Float = by - gTess_IntersecsY	 
							
		 	 				 			
								Local meltmagnitude:float = Sqrt(meltvx*meltvx+meltvy*meltvy) 
	 	 			 			
	 	 			 			'fix inner protrutions
	 	 			 			If meltmagnitude > 2*linewidth  ' this might need ajusting
	 	 			 			
	 	 				 			meltvx /=meltmagnitude
		 				 			meltvy /=meltmagnitude
		 				 			
		 				 			meltmagnitude = Min(meltmagnitude,magnitude1)
		 				 			meltmagnitude = Min(meltmagnitude,magnitude2) 				 			
		 				 			
		 				 		 	rps.Push bx - meltvx   * meltmagnitude
								 	rps.Push by - meltvy  	* meltmagnitude
								 	
								 Else
									 rps.Push gTess_IntersecsX
									 rps.Push gTess_IntersecsY
								 endif 
	 
	 
							lps.Push bx + v1x  
							lps.Push by + v1y   
							
							lps.Push bx + v2x 	 
							lps.Push by + v2y 						
		
		 				Endif	
	
	
					Case 3 ' straight line joint
					
						rps.Push bx - v2x 	 	
						rps.Push by - v2y 	 
						lps.Push bx +  v2x
						lps.Push by +  v2y	 
		 					
			
					Case 4 ' first point when not closed  
 					
						If captype = 2 ' squre end point, extend it by halfline width
							rps.Push bx - v2x + v2y ' add square off sets ' (swap and switch signs to get unit vector)
							rps.Push by - v2y - v2x 
							lps.Push bx + v2x + v2y  
							lps.Push by + v2y - v2x 		
					
						Else
							rps.Push bx - v2x 	 	
							rps.Push by - v2y 	 
							lps.Push bx + v2x
							lps.Push by + v2y	 
	 					Endif
	 
	
					Case 5   ' last point when not closed  
					
			 			If captype = 2 ' squre end point, extend it by halfline width
		
							rps.Push bx - v1x - v1y  	
							rps.Push by - v1y + v1x 
							lps.Push bx + v1x - v1y  	
							lps.Push by + v1y + v1x 	
 			 			Else
 							rps.Push bx - v1x 	 	
							rps.Push by - v1y 	 
							lps.Push bx +  v1x
							lps.Push by +  v1y	 
						endif
	
				End select
				 
				' Add the triangles 
				' the line body
				If lindex>=0
						
					tris.Push lindex
					tris.Push lindex + 2 
					tris.Push - rindex   
		 		 '			
					tris.Push lindex  + 2
					tris.Push -rindex - 2  
					tris.Push -rindex 
	 		 
	 		 		' add bevel fillers
				 	If beveltri = 2
				
						tris.Push lindex + 2
						tris.Push lindex + 4 
						tris.Push -rindex -2  
						
				 	Elseif  beveltri = 1
				
				 		tris.Push lindex  + 2
				 		tris.Push -rindex -4 
				 		tris.Push -rindex -2
					 	
				 	Endif
						
				Endif	
				
				p1 = p2
			 
			Endif

			
			p2 = p3
			p3 += 2
	
 			If p3>=	orgpoints.Length()	p3 -= orgpoints.Length()
 				
		Next	
	
  		
		Local offset:Int = lps.Length()  
 
 		' append right side to left
		For Local d:= Eachin rps
			lps.Push d
		Next
 
		itri.points = lps.ToArray()
		
 		itri.indexes = tris.ToArray()	
 		
		For Local i:Int = 0 Until itri.indexes.Length()
 			If itri.indexes[i] < -900000 Then
				itri.indexes[i] += 1000000  ' correct the negative indexes
				itri.indexes[i] = offset - itri.indexes[i]   ' add offset and correct negative index
			Endif
	 	Next	
	 	
	 	lps = Null
	 	tris = null
 
	End Method
 

End Class



