Strict

'minimal mojo2 app!

Import mojo2
Import tesselator

Class MyApp Extends App

	Field canvas:Canvas
	
	Field vectordrawing:IndexedTriangles
	
	Field morpher:IndexedTriangles
	
	Field mfactor:Float
 
	Method OnCreate:Int()
		canvas=New Canvas
		vectordrawing = MakeVectorDrawing()
		morpher = vectordrawing.Clone()
		
		
		Return 0
	End
	
	Method OnUpdate:Int()
	
		If TouchHit(0)
			For Local n:Int = 0 Until vectordrawing.points.Length 
		 		morpher.points[n] =  vectordrawing.points[n]  + Rnd( -100,100)
		 	Next
		 	mfactor = 1
		Endif
	
		Return 0
	End Method
	
	
	Method OnRender:Int()
	
		canvas.Clear 0,0,1
		 	
		canvas.SetBlendMode 1
	 	 
	 	For Local n:Int = 0 Until vectordrawing.points.Length '-1'-1Step 2
	 		morpher.points[n] =morpher.points[n]*mfactor +  vectordrawing.points[n]*(1.0-mfactor)  
	 	Next
	 
	 	canvas.SetColor 1,1,0,1
	 	canvas.DrawPrimitives(3,morpher.UnPack())
		
		canvas.SetColor 1,0,0,1
		canvas.DrawText "Triangles: " + vectordrawing.indexes.Length/3,DeviceWidth()/2,DeviceHeight()/2,.5,.5
		

		canvas.Flush
		
		mfactor *=0.999
		
		Return 0
	End
End


Function MakeVectorDrawing:IndexedTriangles()
	Local edge:Float[360*2]
		
		
		
		
		Local radius:Float = 110
		
		For Local n:Int= 0 Until 360*2 Step 2
	 		edge[n] = Cos(Float(n)*0.50)*radius  
	 		edge[n+1] = Sin(Float(n)*0.5)*radius  *2
		Next
		
		Local hole:Float[360*2]
		
		radius = 50
		For Local n:Int= 0 Until 360*2 Step 2
	 		hole[n] = Cos(-Float(n)*0.5)*radius    
	 		hole[n+1] = Sin(-Float(n)*0.5)*radius   - radius *2.0
		Next	
		
		Local hole2:Float[360*2]
		
		For Local n:Int= 0 Until 360*2 Step 2
	 		hole2[n] = Cos(-Float(n)*0.5)*radius    
	 		hole2[n+1] = Sin(-Float(n)*0.5)*radius   + radius*2.0
		Next			
		
		
		Local itris:IndexedTriangles  = MakeIndexedTriangles([edge,hole,hole2])
		
		itris.Offset(DeviceWidth()/2,DeviceHeight()/2)
		
		Return itris
		
	'	Local vectordrawing:DrawList = New DrawList
		
	'	vectordrawing.SetColor 1,1,0,1
	' 	vectordrawing.DrawPrimitives (3,itris.Unpack())
	 	
	 	 
		
		 
End Function


Function Main:Int()
	New MyApp
	Return 0
End
