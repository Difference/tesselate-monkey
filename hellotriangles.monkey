Strict

'minimal mojo2 app!

Import mojo2
Import tesselator

Class MyApp Extends App

	Field canvas:Canvas
	
	Field vectordrawing:DrawList
	
 
	Method OnCreate:Int()
		canvas=New Canvas
		vectordrawing = MakeVectorDrawing()
		Return 0
	End
	
	Method OnRender:Int()
	
		canvas.Clear 0,0,1
		 	
		canvas.SetBlendMode 1
		canvas.SetColor 1,1,0,1
		canvas.DrawText "Hello Vector World",DeviceWidth()/2,DeviceHeight()/2,.5,.5
		
		
	 
	'  itris.Offset(DeviceWidth()/2,DeviceHeight()/2)
	 
	 
	 	 
		
		canvas.RenderDrawList(vectordrawing)
		
	 
		
		 canvas.Rotate 0.1
		canvas.Flush
		
		Return 0
	End
End


Function MakeVectorDrawing:DrawList()
	Local edge:Float[36*2]
		
		
		
		
		Local radius:Float = 110
		
		For Local n:Int= 0 Until 36*2 Step 2
	 		edge[n] = Cos(-Float(n)*5.0)*radius  
	 		edge[n+1] = Sin(-Float(n)*5.0)*radius  *2
		Next
		
		Local hole:Float[36*2]
		
		radius = 50
		For Local n:Int= 0 Until 36*2 Step 2
	 		hole[n] = Cos(Float(n)*5.0)*radius    
	 		hole[n+1] = Sin(Float(n)*5.0)*radius   - radius *2.0
		Next	
		
		Local hole2:Float[36*2]
		
		For Local n:Int= 0 Until 36*2 Step 2
	 		hole2[n] = Cos(Float(n)*5.0)*radius    
	 		hole2[n+1] = Sin(Float(n)*5.0)*radius   + radius*2.0
		Next			
		
		
		Local itris:IndexedTriangles  = MakeTriangles([edge,hole,hole2])
		
		itris.Offset(DeviceWidth()/2,DeviceHeight()/2)
		
		Local vectordrawing:DrawList = New DrawList
		
		vectordrawing.SetColor 1,1,0,1
	 	vectordrawing.DrawPrimitives (3,itris.AsPrimitives())
	 	
	 	 
		
		Return vectordrawing
End Function


Function Main:Int()
	New MyApp
	Return 0
End
