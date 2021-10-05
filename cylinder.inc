Type
	TFaces = array of TVec3i;
	TVertices = array of TVec3;

function  getFirstPerpVector(v1: TVec3): TVec3;
//https://stackoverflow.com/questions/1878257/how-can-i-draw-a-cylinder-that-connects-two-points-in-opengl
//return a vector that is perpendicular to the first
begin
 result := Vec3(0.0,0.0,0.0);
 if ((v1.x = 0.0) or (v1.y = 0.0) or (v1.z = 0.0)) then begin
   if (v1.x = 0.0) then
     result.x := 1.0
   else if (v1.y = 0.0) then
     result.y := 1.0
   else
     result.z := 1.0;
 end else begin
   // If xyz is all set, we set the z coordinate as first and second argument .
   // As the scalar product must be zero, we add the negated sum of x and y as third argument
   result.x := v1.z;      //scalp = z*x
   result.y := v1.z;      //scalp = z*(x+y)
   result.z := -(v1.x+v1.y); //scalp = z*(x+y)-z*(x+y) = 0
   // Normalize vector
   result := result.Normalize;
 end;
end;

procedure makeCylinder(radius: TVec3; start, dest: TVec3; var faces: TFaces; var vertices: TVertices; var f0, v0: integer; sides: integer = 8); overload;
//https://stackoverflow.com/questions/1878257/how-can-i-draw-a-cylinder-that-connects-two-points-in-opengl
//{$DEFINE ENDCAPS}
var
	v1, v2, v3, pt: TVec3;
    c, s: single;
    i0, i, num_v, num_f, nxt: integer;
    {$IFDEF ENDCAPS}
    startPole, destPole: integer;
    {$ENDIF}
begin
    if (sides < 3) then sides := 3; //prism is minimal 3D cylinder
  	v1 := (dest - start).Normalize; //principle axis of cylinder
    v2 := getFirstPerpVector(v1); //a unit length vector orthogonal to v1
    // Get the second perp vector by cross product
    v3 := (v1.Cross(v2)).Normalize; //a unit length vector orthogonal to v1 and v2
    num_v := 2 * sides;
    num_f := 2 * sides;
    {$IFDEF ENDCAPS}
    num_f += 2 * sides;
    num_v += 2;
    {$ENDIF}
    if (length(faces) < (num_f + f0)) then
    	setlength(faces, num_f + f0);
    if (length(vertices) < (num_v + v0)) then
    	setlength(vertices, num_v + v0);
    {$IFDEF ENDCAPS}
    startPole := v0 + 2 * sides;
    destPole := startPole + 1;
    vertices[startPole] := start; //pole of start
    vertices[destPole] := dest; //pole of end
    {$ENDIF}
    for i := 0 to (sides-1) do begin
      c :=  cos(i/sides * 2 * PI);
      s :=  sin(i/sides * 2 * PI);
      pt.x := (radius.x * (c * v2.x+ s *v3.x));
      pt.y := (radius.y * (c * v2.y+ s *v3.y));
      pt.z := (radius.z * (c * v2.z+ s *v3.z));
      vertices[v0+ i] := start+pt;
      vertices[v0 + i + sides] := dest+pt;
      if i < (sides-1) then
        nxt := i + 1
      else //final 2 triangles of cylinder share vertices with first triangle (close the loop)
        nxt := 0;
      nxt += v0;
      i0 := i + v0;
      faces[f0 + i * 2] := pti( i0,  nxt, i0 + sides);
      faces[f0 + (i * 2)+1] := pti(nxt,  nxt + sides, i0 + sides);
      {$IFDEF ENDCAPS}
      faces[f0 + (sides*2)+i] := pti(i0, startPole, nxt);
      faces[f0 + (sides*2)+i+sides] := pti( destPole, i0 + sides, nxt + sides);
      {$ENDIF}
    end;
    f0 += num_f;
    v0 += num_v;
end;

procedure MakeCyl(radius, sliceFrac: TVec3;  var faces: TFaces; var vertices: TVertices);
var
    f0: integer = 0;
    v0: integer = 0;
begin
  MakeCylinder(radius, Vec3(sliceFrac.x, sliceFrac.y, -0.1), Vec3(sliceFrac.x, sliceFrac.y, 1.1), faces, vertices, f0, v0);
  MakeCylinder(radius, Vec3(sliceFrac.x, -0.1, sliceFrac.z),  Vec3(sliceFrac.x, 1.1, sliceFrac.z), faces, vertices, f0, v0);
  MakeCylinder(radius, Vec3(-0.1, sliceFrac.y, sliceFrac.z),  Vec3(1.1, sliceFrac.y, sliceFrac.z), faces, vertices, f0, v0);
end;
