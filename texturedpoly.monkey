Strict

Import mojo2
Import tesselator

Class MyApp Extends App

	Field canvas:Canvas	
	Field image:Image
	 
	Field editmode:Int	
	Field drawpoints:= New List<Float> 
	
	Field material:Material
	 
	Method OnUpdate:Int()
	
		If MouseHit(0)
			If editmode = 0 
			
				drawpoints.AddLast(MouseX())
				drawpoints.AddLast(MouseY())
			
			Endif
			
		
		Endif
	
		 return 0
	End
	
	Method OnCreate:Int()
		canvas=New Canvas
		
		material = New Material()

		image = MakeImage()
		
		material.SetTexture "ColorTexture",image.Material.ColorTexture
		
		Return 0
	End
	
	Method OnRender:Int()
	
		
		
		'render to main canvas...
		canvas.Clear
		'canvas.DrawImage image,MouseX(),MouseY()
		
		
		Local drawpoints:Float[] = drawpoints.ToArray()
		Local trisoup:= MakeIndexedTriangles([drawpoints])
		trisoup.MakeTextureCords()
		
		canvas.SetColor(1,1,1)
		'canvas.DrawPrimitives(3,drawpoints.Length()/6,drawpoints)
		
		
		'canvas.DrawIndexedPrimitives(3,trisoup.indexes.Length()/3,trisoup.points,trisoup.indexes)
		
		canvas.DrawIndexedPrimitives(3,trisoup.indexes.Length()/3,trisoup.points,trisoup.texcoords,trisoup.indexes,material)
		
		
		'If drawpoints.Length() Print drawpoints[0]
		
		
		canvas.Flush
		Return 0
	End
End

Function Main:Int()
	New MyApp
	Return 0
End

Function MakeImage:Image()

	Local img:=New Image( 256,256 )
		Local icanvas:=New Canvas( img )

'render to image...
		For Local x:=0 Until 16
			For Local y:=0 Until 16
				If (x~y)&1
					icanvas.SetColor 0,1,0
				Else
					icanvas.SetColor 1,1,0
				Endif
				icanvas.DrawRect x*16,y*16,16,16
			Next
		Next
		icanvas.Flush
		
		Return img

End Function

		
		
	
	
	