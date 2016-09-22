Strict

Import mojo2
Import tesselator

Class MyApp Extends App

	Field canvas:Canvas	
	 
	 
	Field editmode:Int	
	Field drawpoints:= New List<Float> 
	Field trisoup:IndexedTriangles
	Field material:Material
	 
	Method OnUpdate:Int()
	
		If MouseHit(0)
			If editmode = 0 
			
				drawpoints.AddLast(MouseX())
				drawpoints.AddLast(MouseY())
				
 
				 trisoup = MakeIndexedTriangles([drawpoints.ToArray()])
			
			Endif
			
		
		Endif
	
		 return 0
	End
	
	Method OnCreate:Int()
		canvas=New Canvas
		
		material = MakeTexture()
		
		Return 0
	End
	
	Method OnRender:Int()
	
		canvas.Clear
		
		canvas.SetColor(1,1,1)

		If trisoup<>Null And  trisoup.TriangleCount()>=1
			Local scale:Float = 0.75+Cos(Millisecs()*0.1)*0.25
			
			Local offx:Float = 0.5+Sin(Millisecs()*0.05)*0.25
			Local offy:Float = 0.5+Sin(Millisecs()*0.11)*0.15
			
			'scale = 1
			'offx = 0
			'offy = 0
		
			trisoup.MakeTextureCords(offx,offy,scale,scale)
			canvas.DrawIndexedPrimitives(3,trisoup.TriangleCount,trisoup.Points(),trisoup.texcoords,trisoup.Indexes,material)
		Endif
		
		canvas.SetColor 1,0,0,1
		canvas.DrawText "Click screen 3 or times or more to create points for the polygon",DeviceWidth()/2,DeviceHeight()/2,.5,.5
 
		 
		
		
		
	 	canvas.Flush
		Return 0
	End
End

Function Main:Int()
	New MyApp
	Return 0
End

Function MakeTexture:Material()

	Local img:=New Image( 256,256 )
		Local icanvas:=New Canvas( img )

 
		For Local x:=0 Until 16
			For Local y:=0 Until 16
				If (x~y)&1
					icanvas.SetColor 0,0.5+Float(x)/32.0,Float(y)/20.0
				Else
					icanvas.SetColor Float(y)/20.0,0.5+Float(x)/32.0,1-Float(y)/16.0
				Endif
				icanvas.DrawRect x*16,y*16,16,16
			Next
		Next
		
		icanvas.SetColor 1,1,1
		icanvas.DrawText("Hello texturecords",128,128,0.5,0.5)
		
		icanvas.Flush
		
		Local mat:= New Material()

	 
		
		mat.SetTexture "ColorTexture",img.Material.ColorTexture		
		
		
		Return mat

End Function

		
		
	
	
	