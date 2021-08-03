(* ::Package:: *)

(* :Mathematica Version: 12.1 *)

(* :Name: Graphics`Graphics3D` *)

(* :Title: Graphics3D *)

(* :Author: Wolfram Research, Inc. *)
(* :Copyright: Copyright 1990-2007, Wolfram Research, Inc.*)

(* :History:
        Created March 1991 by John M. Novak. A collection of functions
        originally intended for Graphics.m by Michael Chan and Kevin McIsaac,
        with modifications by Bruce Sawhill and ECM.  Modifications to Project
        and Shadow by John M. Novak.
        V1.0.5 April 1994 by John M. Novak. Minor fixes to ScatterPlot3D.
        Modifications to StackGraphics for correct display of polygons
          by Tom Wickham-Jones. Change to ShadowPlot3D and ListShadowPlot3D
          to use a ColorFunction option in place of the current Color
          option (which is retained for compatibility); this was done at
          the suggestion of John Fultz.
*)
(* :Summary:
This package provides special functions for plotting in three
dimensions.  Special formats include scatter plots,
surface plots, shadow plots, and projections.
*)

(* :Context: Graphics`Graphics3D` *)

(* :Package Version: 1.1 *)

(* :Keywords:
	Graphics, 3D, Surface, Project, Shadow
*)

(* :Warning: Adds to the definition of the function Graphics3D. *)

Message[General::obspkg, "Graphics`Graphics3D`"]
Quiet[BeginPackage["Graphics`Graphics3D`"], {General::obspkg, General::newpkg}]


ScatterPlot3D::usage = 
"ScatterPlot3D[{{x1, y1, z1}, ...}, (options)] plots points in three \
dimensions as a scatter plot.";

ShadowPlot3D::usage =
"ShadowPlot3D[f, {x, xmin, xmax}, {y, ymin, ymax}] plots the function f over \
the the x and y ranges with polygons shaded according to the height of the \
surface, with a projection of the surface onto the x-y plane.";

(* The Color option determines whether the plot is in color or gray scale.
SurfaceMesh determines whether a mesh is drawn on the surface.
ShadowMesh determines whether a mesh is drawn on the projection.
SurfaceMeshStyle determines the style of the mesh on the surface.
ShadowMeshStyle determines the style of the mesh on the projection.
ShadowPosition determines the location of the projection. *)

If[StringQ[Color::usage] && StringFreeQ[Color::usage, "ShadowPlot"],
Color::usage =  Color::usage <>
" Color is also an option for ShadowPlot3D and ListShadowPlot3D, which determines \
whether the plot should be drawn in color. If True, the ColorFunction option \
is employed (set to Hue by default), otherwise a greyscale is used.";
]

If[StringQ[SurfaceMesh::usage] && StringFreeQ[SurfaceMesh::usage, "ShadowPlot"],
SurfaceMesh::usage = SurfaceMesh::usage <>
" SurfaceMesh is also an option for ShadowPlot3D and ListShadowPlot3D, which \
determines whether a mesh is drawn on the surface.";
]

ShadowMesh::usage =
"ShadowMesh is an option for ShadowPlot3D and ListShadowPlot3D, which determines \
whether a mesh is drawn on the projection.";

SurfaceMeshStyle::usage =
"SurfaceMeshStyle is an option for ShadowPlot3D and ListShadowPlot3D, which \
defines the style of the mesh on the surface.";

ShadowMeshStyle::usage =
"ShadowMeshStyle is an option for ShadowPlot3D and ListShadowPlot3D, which \
defines the style of the mesh on the projection.";

ShadowPosition::usage =
"ShadowPosition is an option for ShadowPlot3D and ListShadowPlot3D, which \
determines whether the projection is above or below the surface (in the \
positive or negative z direction).";

ListShadowPlot3D::usage =
"ListShadowPlot3D[array, (opts)] generates a surface representing an array of \
height values with polygons shaded according to the height of the surface and a \
projection of the surface onto the x-y plane.";

Project::usage = 
"Project[graphic, point] projects an image of the graphic onto a plane \
perpendicular to the line from the center of the graphic to point. \
Project[graphic, {e1, e2}, point] projects an image of the graphic onto a plane \
with basis vectors {e1, e2} at point, along the line from the origin to point. \
Project[graphic, {e1,e2},point,center] project as before, except along the \
line from center to point.  The projection is as seen from Infinity.";

Shadow::usage = 
"Shadow[graphic, (opts)] projects images of the graphic onto the \
x-y, x-z, and y-z planes.  Options XShadow, YShadow, ZShadow, \
XShadowPosition, YShadowPosition, and ZShadowPosition determine \
which projections are shown and where they are located.  The \
magnitude of the positions is scaled so that 1 is the width of the \
plot on the given axis; it is measured from the center of the \
plot.";

XShadow::usage =
"XShadow is an option for Shadow that determines whether to draw a \
projection of the graphic in the x direction.";

YShadow::usage =
"YShadow is an option for Shadow that determines whether to draw a \
projection of the graphic in the y direction.";

ZShadow::usage =
"ZShadow is an option for Shadow that determines whether to draw a \
projection of the graphic in the z direction.";

XShadowPosition::usage = 
"XShadowPosition is an option for Shadow that determines whether the \
projection of the graphic is in the positive or negative x direction.";

YShadowPosition::usage = 
"YShadowPosition is an option for Shadow that determines whether the \
projection of the graphic is in the positive or negative y direction.";

ZShadowPosition::usage = 
"ZShadowPosition is an option for Shadow that determines whether the \
projection of the graphic is in the positive or negative z direction.";

ShadowColor::usage = 
"ShadowColor"

GeneralizedBarChart3D::usage =
"GeneralizedBarChart3D[{{{xpos1, ypos1}, height1, {xwidth1, ywidth1}}, \
{{xpos2, ypos2}, height2, {xwidth2, ywidth2}}, ...}] generates a  \
three-dimensional bar graph with the solid bars at the given positions, heights, \
and widths."

StackGraphics::usage =
"StackGraphics[{g1, g2, ...}] generates a Graphics3D object corresponding to a \
\"stack\" of two-dimensional graphics objects.";

TransformGraphics3D::usage =
"TransformGraphics3D[graphics3d, f] applies the function f to all lists of \
coordinates in graphics3d.";

SkewGraphics3D::usage = 
"SkewGraphics3D[graphics, m] applies the matrix m to all coordinates in graphics.";

Graphics3D::usage =
"Graphics3D[primitives, options] represents a three-dimensional graphic \
image.  Graphics3D[graphics] projects a two-dimensional graphic image into \
a three-dimensional graphic image."; 


Begin["`Private`"]

Clear@Shadow

If[$VersionNumber>=9,Graphics`Graphics3D`Private`FilterOptions[a_,b___]:=Sequence@@FilterRules[{b},Options[a]]];

issueObsoleteFunMessage[fun_, context_] := Message[General::obspkgfn, fun, context];

(* Define a better NumberQ *)

numberQ[x_] := NumberQ[N[x]]

(* vector cross product *)

cross[v1_?VectorQ, v2_?VectorQ] :=
	Module[{m=Minors[{v1,v2},2][[1]]},
		{m[[3]], -m[[2]], m[[1]]}
	]	/; Length[v1]==Length[v2]==3

(* Unit vector in the vec direction *)

normalize[vec:{_,_,_}] :=
	vec/Sqrt[Apply[Plus,vec^2]]


(* GeneralizedBarChart3D *)

Options[GeneralizedBarChart3D] =
  {
   SolidBarEdges -> True,
   SolidBarEdgeStyle -> GrayLevel[0],
   SolidBarStyle -> SurfaceColor[GrayLevel[1]]
  };



(* NOTE that BarSpacing is NOT an option of
	Graphics`Graphics`GeneralizedBarChart,
	so XSpacing and YSpacing are NOT options of
	Graphics`Graphics3D`GeneralizedBarChart3D. *)
(* NOTE that the data in "list" are of the form...
	{  {{xpos1, ypos1}, height1, {xwidth1, ywidth1}},
	   {{xpos2, ypos2}, height2, {xwidth2, ywidth2}}, ...}
*)
GeneralizedBarChart3D[
	list:{ {{_?numberQ,_?numberQ}, _?numberQ, {_?numberQ,_?numberQ}}... },
	opts___] :=
  (issueObsoleteFunMessage[GeneralizedBarChart3D,"Graphics`Graphics3D`"];
	Module[{edges, g3dopts, barstyle},
   (
    If[TrueQ[SolidBarEdges/.Flatten[{opts, Options[GeneralizedBarChart3D]}]],
	    edges = EdgeForm[SolidBarEdgeStyle/.Flatten[{opts, Options[GeneralizedBarChart3D]}]],
	    edges = EdgeForm[]
    ];
    barstyle = SolidBarStyle/.Flatten[{opts, Options[GeneralizedBarChart3D]}];
    g3dopts = ({FilterOptions[Graphics3D, ##]}&) @@ 
		Flatten[{opts, Options[GeneralizedBarChart3D]}];
    Show[
   	Graphics3D[{If[ListQ[barstyle], Sequence @@ barstyle, barstyle], Map[Flatten[{edges,
   		Cuboid[{#[[1,1]]-#[[3, 1]]/2, #[[1,2]]-#[[3, 2]]/2, 0},
		       {#[[1,1]]+#[[3, 1]]/2, #[[1,2]]+#[[3, 2]]/2, #[[2]]}]
		      }]&,
		   list]}],
	Flatten[Join[
	   {g3dopts},
	   {Axes -> Automatic, BoxRatios -> {1,1,1}, PlotRange -> All} 
		]  ]
    ]
   ) 
  ]) (* end GeneralizedBarChart3D *)                                        


(* ScatterPlot3D *)

Options[ScatterPlot3D] =
    Join[{PlotJoined->False,
                  PlotStyle -> GrayLevel[0]},
                 Developer`Graphics3DOptions[]
    ];

SetOptions[ScatterPlot3D, Axes -> True]

ScatterPlot3D[l3:{{_, _, _}..}, opts___?OptionQ] :=
(issueObsoleteFunMessage[ScatterPlot3D,"Graphics`Graphics3D`"];
	Module[{sty, join},
        {sty, join} = {PlotStyle, PlotJoined}/.Flatten[{opts}]/.
                   Options[ScatterPlot3D];
        If[join,
                Show[Graphics3D[ Flatten[{ sty, Line[l3] }] ],
                        FilterOptions[ Graphics3D, ##] & @@
                            Flatten[{opts, Options[ScatterPlot3D]}]
                ],
                Show[Graphics3D[ Flatten[{ sty, Map[Point,l3] }] ],
                        FilterOptions[ Graphics3D, ##] & @@
                            Flatten[{opts, Options[ScatterPlot3D]}]
                ]
        ]
])

(* Modified MakePolygon, used in ShadowPlot3D *)

MakePolygonCoords[vl_List] :=
    Module[{l = vl,
    	   l1 = Map[RotateLeft, vl],
    	   mesh},
	mesh = {l, l1, RotateLeft[l1], RotateLeft[l]};
	mesh = Map[Drop[#, -1]&, mesh, {1}];
	mesh = Map[Drop[#, -1]&, mesh, {2}];
	Transpose[ Map[Flatten[#, 1]&, mesh] ]
    ]

(* ShadowPlot3D *)

Options[ShadowPlot3D] = 
	Join[{PlotPoints->15, Color->True, ColorFunction -> Hue,
		SurfaceMesh->True,
		SurfaceMeshStyle -> RGBColor[0,0,0],
		ShadowMesh -> True, 
		ShadowMeshStyle -> RGBColor[0,0,0],
		ShadowPosition -> -1}, Developer`Graphics3DOptions[]];

SetOptions[ShadowPlot3D, BoxRatios -> {1,1,1}]

ShadowPlot3D[func_, {u_, umin_, umax_},  {v_, vmin_, vmax_}, opts___?OptionQ] :=
	(issueObsoleteFunMessage[ShadowPlot3D,"Graphics`Graphics3D`"];
	Module[{plotpoints = PlotPoints /. {opts} /. Options[ShadowPlot3D]},
		SP0[MakePolygonCoords[
			Table[N[{u, v, func}],
				{u,umin,umax,(umax-umin)/plotpoints},
				{v,vmin,vmax,(vmax-vmin)/plotpoints}]],
			Flatten[{{opts},Options[ShadowPlot3D]}]]])
		

(* ListShadowPlot3D *)

Options[ListShadowPlot3D] = 
	Join[{Color -> True, ColorFunction -> Hue, SurfaceMesh->True, 
	SurfaceMeshStyle -> RGBColor[0,0,0],
	ShadowMesh -> True,
	ShadowMeshStyle -> RGBColor[0,0,0],
	ShadowPosition -> -1}, Developer`Graphics3DOptions[]];

SetOptions[ListShadowPlot3D, BoxRatios -> {1,1,1}];

ListShadowPlot3D[list_?MatrixQ, opts___?OptionQ] :=
	(issueObsoleteFunMessage[ListShadowPlot3D,"Graphics`Graphics3D`"];
	SP0[MakePolygonCoords[MapIndexed[Append[Reverse[#2], #1]&, N[list],{2}]],
		Flatten[{{opts},Options[ListShadowPlot3D]}]])

SP0[list_, opts___] :=
	Module[{gopts, z, zmin, zmax, zrange, zshadow, shades,
			color, colorfunc, surfacemesh, surfacemeshstyle, 
			shadowmesh, shadowmeshstyle, pos, g},
		gopts = FilterOptions[Graphics3D, ##] & @@  opts;
		{color, colorfunc, surfacemesh, surfacemeshstyle, shadowmesh,
			 shadowmeshstyle, pos} = 
			{Color, ColorFunction, SurfaceMesh, SurfaceMeshStyle, 
				ShadowMesh, ShadowMeshStyle, 
				ShadowPosition} /. opts;
		z = Map[#[[3]]&,list,{-2}];
		{zmin, zmax} = {Min[z], Max[z]};
		zrange = zmax - zmin;
		zshadow = If[!TrueQ[pos == -1],
					zmax + zrange/2,
					zmin - zrange/2];
	        colorfunc = If[TrueQ[color], colorfunc, GrayLevel];
                shades = Map[colorfunc,
                                (Apply[Plus,z,{-2}]/4 - zmin)/zrange
                ];
                g = Transpose[{shades,Polygon /@ list}];
                Show[
                        Graphics3D[
                                {If[TrueQ[surfacemesh],
                                        EdgeForm[surfacemeshstyle],EdgeForm[]],
                                 g,
                                 If[TrueQ[shadowmesh],
                                        EdgeForm[shadowmeshstyle],EdgeForm[]],
                                tg3D[g, {#[[1]],#[[2]],zshadow}& ]}
                        ],
                        Flatten[{gopts, BoxRatios->{1,1,1}}]
                    ]
         ]


(* TransformGraphcs3D *)
TransformGraphics3D[obj_, f_] := (issueObsoleteFunMessage[TransformGraphics3D,"Graphics`Graphics3D`"];
	tg3D[obj, f])

cuboidtopolygons[Cuboid[{xmin_, ymin_, zmin_}]] :=
        cuboidtopolygons[Cuboid[{xmin, ymin, zmin}, {xmin, ymin, zmin} + 1]]

cuboidtopolygons[Cuboid[{xmin_, ymin_, zmin_}, {xmax_, ymax_, zmax_}]] :=
  {Polygon[{{xmax, ymax, zmax}, {xmin, ymax, zmax}, {xmin, ymin, zmax},
     {xmax, ymin, zmax}}], Polygon[{{xmax, ymax, zmax}, {xmax, ymin, zmax},
     {xmax, ymin, zmin}, {xmax, ymax, zmin}}],
   Polygon[{{xmax, ymax, zmax}, {xmax, ymax, zmin}, {xmin, ymax, zmin},
     {xmin, ymax, zmax}}], Polygon[{{xmin, ymax, zmax}, {xmin, ymax, zmin},
     {xmin, ymin, zmin}, {xmin, ymin, zmax}}],
   Polygon[{{xmin, ymin, zmin}, {xmin, ymax, zmin}, {xmax, ymax, zmin},
     {xmax, ymin, zmin}}], Polygon[{{xmin, ymin, zmax}, {xmin, ymin, zmin},
     {xmax, ymin, zmin}, {xmax, ymin, zmax}}]}

tg3D[HoldPattern[Graphics3D][g_, o___], f_, p_:0] := Graphics3D[tg3D[g, f, p], o]

tg3D[d_List, f_, p_:0] := Map[ tg3D[#, f, p]& , d ]

tg3D[GraphicsGroup[a_, b___], f_, p_:0] := GraphicsGroup[tg3D[a, f, p], b]

tg3D[HoldPattern[GraphicsComplex][pts_, prims_, o___], f_, p_:0] :=
    GraphicsComplex[If[VectorQ[pts], pts, Map[f, pts]],
         tg3D[prims, f, Length[pts]]]

tg3D[Point[d_], f_, p_:0]/;(p === 0 || !gcq[d, p]) := Point[f/@d]

tg3D[Line[d_List], f_, p_:0]/;(p === 0 || !gcq[d, p]) :=
    Line[Map[f, d, If[MatrixQ[d[[1]]], {2},{1}]]]

tg3D[Polygon[d_List], f_, p_:0]/;(p === 0 || !gcq[d, p]) :=
    Polygon[Map[f, d, If[MatrixQ[d[[1]]], {2},{1}]]]

tg3D[c:Cuboid[{_,_,_}], f_, p_:0] :=
    tg3D[cuboidtopolygons[c], f, p]

tg3D[c:Cuboid[{_,_,_}, {_,_,_}], f_, p_:0] :=
    tg3D[cuboidtopolygons[c], f, p]

tg3D[expr_, f_, p_:0] := expr

(* gcq tests for compliance with a GraphicsComplex indexed primitive *)
gcq[_Integer] := True
gcq[pts:{__Integer}, count_]/;(Max[pts] <= count && Min[pts] >= 1) := True
gcq[pts:{{_Integer..}..}, count_]/;(Max[pts] <= count && Min[pts] >= 1) := True
gcq[any_] := False

(* SkewGraphics3D *)

SkewGraphics3D[g_Graphics3D, m_?MatrixQ] :=
	(issueObsoleteFunMessage[SkewGraphics3D,"Graphics`Graphics3D`"];
	TransformGraphics3D[g, (m . #)&])

(* Project *)

Project[g_Graphics3D, point:{_,_,_}] :=
	(issueObsoleteFunMessage[Project,"Graphics`Graphics3D`"];
	Module[{p1, p2, t1, t2, t3, b1, b2,c},
		p1 = PlotRange[g];
		c = Map[((#[[1]] + #[[2]])/2)&,p1];
		p2 = point-c;
		t1 = If[TrueQ[(t2 = cross[{0,0,1},p2]) == {0,0,0}],
				cross[{0,1,0},p2],t2];
		b1 = normalize[t1];
		t3 = cross[p2,b1];
		b2 = normalize[t3];
		Project[g,{b1,b2},point,c]])

Project[g_Graphics3D, basis:{{_,_,_},{_,_,_}}, location:{_,_,_},
          Optional[center:{_,_,_},{0,0,0}]] :=
	(issueObsoleteFunMessage[Project,"Graphics`Graphics3D`"];
	TransformGraphics3D[g,
		(Apply[Plus, (basis.(# - center)) basis] + location)&])

Project[g_,  point:{_,_,_}] := Project[Graphics3D[g],point]

Project[g_, basis:{{_,_,_},{_,_,_}}, location:{_,_,_},
                 Optional[center:{_,_,_},{0,0,0}]] :=
    Project[Graphics3D[g], basis, location, center]


(* Shadow *)

Options[Shadow] = 
	Join[{XShadow -> True, YShadow -> True, ZShadow -> True,
	XShadowPosition -> -1, YShadowPosition -> 1,
	ZShadowPosition -> -1, ShadowColor -> RGBColor["#000000"]},
	Developer`Graphics3DOptions[]];

Shadow[g_, opts___] :=
	(issueObsoleteFunMessage[Shadow,"Graphics`Graphics3D`"];
	Module[{xmin, xmax, ymin, ymax, zmin, zmax, xshadow, 
			yshadow, zshadow, xshadowposition, 
			yshadowposition, zshadowposition, shadowcolor, 
			image,br},
		{xshadow, yshadow, zshadow, xshadowposition, 
			yshadowposition, zshadowposition, shadowcolor} = 
			{XShadow, YShadow, ZShadow, XShadowPosition,
			 YShadowPosition, ZShadowPosition, ShadowColor} /. {opts} /.
			 	Options[Shadow];
		gopts = FilterOptions[Graphics3D, opts,
				Sequence @@ Options[Shadow]];
	    graph = Graphics3D[g];
		{xmin, xmax, ymin, ymax, zmin, zmax} = 
			Flatten[PlotRange[graph]];
		br = FullOptions[graph,BoxRatios];
		image = {graph};
		If[xshadow, 
			AppendTo[image,
				Project[Show[Graphics3D[{shadowcolor,Opacity[0.1],g}]],
					{(xmax+xmin)/2 + xshadowposition (xmax - xmin),
						(ymax+ymin)/2,
						(zmax+zmin)/2}]];
			If[Abs[xshadowposition] > 1/2,
				br = br {(Abs[xshadowposition] + 1/2),1,1}]];
		If[yshadow, 
			AppendTo[image,
				Project[Graphics3D[{shadowcolor,Opacity[0.1],g}],
					{(xmax+xmin)/2,
					(ymax+ymin)/2 + yshadowposition (ymax - ymin),
					(zmax+zmin)/2}]];
			If[Abs[yshadowposition] > 1/2,
				br = br {1, (Abs[yshadowposition] + 1/2),1}]];
		If[zshadow, 
			AppendTo[image,
				Project[Graphics3D[{shadowcolor,Opacity[0.1],g}],
					{(xmax+xmin)/2,
					(ymax+ymin)/2,
					(zmax+zmin)/2 + zshadowposition (zmax - zmin)}]];
			If[Abs[zshadowposition] > 1/2,
				br = br {1, 1, (Abs[zshadowposition] + 1/2)}]];
		Show[image,Flatten[{gopts,BoxRatios->br,PlotRange->All}]]])

Shadow[Graphics3D[g_,others___],opts___] := Shadow[g,opts]

(* Graphics3D *)
Unprotect[Graphics3D];

Graphics3D[HoldPattern[Graphics][primitives_,options___]] :=
	Graphics3D[ZTG[primitives, 0], BoxRatios->{1,1,1},
        Axes->{True, False, True},
        PlotRange->{Automatic, {-1,1}, Automatic},
        ViewPoint->{0, -1, -3}
	]

Protect[Graphics3D];


(* StackGraphics *)
(* allow graphics to be specified in a sequence instead of a list *)
StackGraphics[grs__Graphics, opts___?OptionQ] :=
    StackGraphics[{grs}, opts]

StackGraphics[list:{__Graphics}, opts___?OptionQ] :=
	(issueObsoleteFunMessage[StackGraphics,"Graphics`Graphics3D`"];
	Module[{i, pr, as},
        {pr, as} = {PlotRange, BoxRatios}/.
             Flatten[{opts, BoxRatios -> {1, 1, 1}, Options[Graphics3D]}];
      (* hopefully following not too clever for its own good...
          if pr is not a list, immediately do If clause; otherwise, it
          must be a list, we can drop the middle element and see if the
          result is numeric; if not, drop into if clause with 2-element pr *)
        If[!ListQ[pr] || !VectorQ[pr = Drop[pr, {2}], NumericQ],
            pr = PlotRange[Show[list,
                      DisplayFunction -> Identity, PlotRange -> pr]]
        ];
        If[as === Automatic,
            as = AspectRatio/.AbsoluteOptions[Show[list,
                     DisplayFunction -> Identity, PlotRange -> pr,
                     AspectRatio -> Automatic], AspectRatio],
          (* else it must be a triplet... *)
            as = Divide @@ Reverse @ Drop[as, {2}]
        ];
        Graphics3D[ Table[{ZTG[First[ list[[i]] ], i/Length[list], pr, as]}, 
		                 {i, Length[list]}],
             opts,
             BoxRatios->{1,1,1},
		     Axes->{True, False, True}
        ]
	])

(* ZTG transforms elements from 2D to 3D *)

ZTG[d_List, all___] := Map[ ZTG[#, all]& , d ]

ZTG[GraphicsGroup[g_, o___], all___] := GraphicsGroup[ZTG[g, all], o]

ZTG[Point[d_List?(Depth[#] === 3 &), o___], z_, ___] := Point[Map[Insert[#, z, 2]&, d], o]

ZTG[Point[{x_, y_}, o___], z_, ___] := Point[{x, z, y}, o]

ZTG[(h:(Line | Polygon))[d_List?(Depth[#] === 4 &), o___], z_, ___] :=
    h[Map[Insert[#, z, 2]&, d, {2}], o]

ZTG[(h:(Line | Polygon))[d:{{_,_}...}, o___], z_, ___] :=
    h[ Map[Insert[#, z, 2]&, d], o]

(* ****** following only handles a simple GraphicsComplex; nested or ones with embedded
   exacts will fail, this needs to be fixed. *)
ZTG[HoldPattern[GraphicsComplex][p_, r___], z_, ___] :=
    GraphicsComplex[Map[Insert[#, z, 2]&, p], r]

ZTG[(h:(Disk | Circle))[cent:{_,_}], all___] :=
    ZTG[h[cent, {1, 1}, {0, 2 Pi}], all]

ZTG[(h:(Disk | Circle))[cent:{_,_}, rad_?NumericQ], all___] :=
    ZTG[h[cent, {rad, rad}, {0, 2 Pi}], all]

ZTG[(h:(Disk | Circle))[cent:{_,_}, rad:{_,_}], all___] :=
    ZTG[h[cent, rad, {0, 2 Pi}], all]

ZTG[(h:(Disk | Circle))[cent:{_,_}, rad_?NumericQ, ang:{_,_}], all___] :=
    ZTG[h[cent, {rad, rad}, ang], all]

$ZTGFraction = 0.05; (* percent of longest plotrange to use in computation
                        of approximate increment for arcs *)

ZTG[(h:(Disk | Circle))[cent:{_,_}, {rlen_, rheight_}, {angmin_, angmax_}],
       z_, pr_, ___] :=
    Module[{pts, t, div},
      (* compute number of divisions to use in arc, based on a cirle of the
          larger radius divided into sections based on a global fraction of
          the longer plotrange *)
        div = Max[Map[(Subtract @@ Reverse[#])&, pr]] * $ZTGFraction;
        If[(div = 6.3 Max[rlen, rheight]/div) < 1, div = 1, div = Ceiling[div]];
      (* compute points, using above-computed divisions *)
        pts = Table[{rlen Sin[t], rheight Cos[t]},
                    {t, angmin, angmax, (angmax - angmin)/div}];
        pts = Map[# + cent &, pts];
        If[!(angmin === 0 && angmax == 2 Pi),
             pts = Append[pts, cent]
        ];
      (* don't bother with recursive ZTG call,
         since poly is guaranteed convex *)
        pts = Map[Insert[#, z, 2]&, pts];
        If[h === Disk,
            Polygon[pts],
            Line[pts]
        ]
    ]

ZTG[Rectangle[{lx_, ly_}, {ux_, uy_}], all___] :=
    ZTG[Polygon[{{lx, ly}, {lx, uy}, {ux, uy}, {ux, ly}}], all]

ZTG[Raster[mat_?MatrixQ, opts___?OptionQ], all___] :=
    ZTG[Raster[mat, {{0,0}, Reverse[Dimensions[mat]]}, opts], all]

ZTG[Raster[mat_?MatrixQ, coords:{{_,_},{_,_}}, opts___?OptionQ], all___] :=
   ZTG[Raster[mat, coords, {0, 1}, opts], all]

ZTG[Raster[mat_?MatrixQ,
           coords:{{_,_},{_,_}},
           {smin_, smax_}, opts___?OptionQ
      ], all___] :=
    Module[{cf, cfs, newmat, sdiff = smax - smin},
        {cf, cfs} = {ColorFunction, ColorFunctionScaling}/.
            Flatten[{opts, Options[Raster]}];
        If[cf === Automatic, cf = GrayLevel];
        If[TrueQ[cfs],
            newmat = Map[
                (cf[If[#< smin, 0, If[# > smax, 1, (# - smin)/sdiff]]])&,
                mat,
                {2}
            ],
            newmat = Map[cf, mat, {2}]
        ];
        ZTG[RasterArray[newmat, coords], all]
    ]

ZTG[RasterArray[colormat_?MatrixQ], all___] :=
    ZTG[RasterArray[colormat, {{0,0}, Reverse[Dimensions[colormat]]}], all]

ZTG[RasterArray[colormat_?MatrixQ, {{minx_, miny_}, {maxx_, maxy_}}], all___] :=
  Module[{xincr = (maxx - minx)/Length[First[colormat]],
          yincr = (maxy - miny)/Length[colormat]},
    ZTG[{EdgeForm[], Transpose[{colormat,
            Table[Polygon[{{i, j}, {i + xincr, j}, {i + xincr, j + yincr},
                           {i, j + yincr}}],
                  {j, miny, maxy - yincr, yincr},
                  {i, minx, maxx - xincr, xincr}
             ]}, {3, 1, 2}
        ]},
        all
    ]
  ]

(* ****** Arrow handling here is strictly a 'zero-th order' solution;
          this is good enough for basic use, but can be better. Better
          unfortunately involves more complex coding... --JMN 05-11-21 *)
ZTG[Arrowheads[___], all___] := {}
ZTG[Arrow[c_, ___], all___] := ZTG[Line[c], all]

(* For the following entries, the weird pattern is to prevent
   stub autoloading *)
(* but, 'subsumed' splines in existing lines and polys not handled yet... *)
ZTG[(_Symbol?(Context[#] === "Graphics`Spline`" &&
              SymbolName[#] === "Spline" &))[args___], all___] :=
   ZTG[Line[{
       Graphics`Spline`Private`splinesubsume[Symbol["Graphics`Spline`Spline"][args]]
   }], all]

ZTG[Text[d_String, {x_, y_}, dd___], z_, ___] := Text[d, {x,z,y}, dd]

ZTG[expr_, ___] := expr


End[] (* Private` *)

EndPackage[] (* Graphics`Graphics3D` *)

(* :Examples:
	
	g1 = CylindricalPlot3D[ r^2,{r,0,1},{phi,0,2 Pi}];
	Show[ TransformGraphics3D[ g1, Cos[#] & ] ]
	
	
	
	g1 = CylindricalPlot3D[ r^2,{r,0,1},{phi,0,2 Pi}];
	Show[ SkewGraphics3D[ g1, {{1,2,0},{0,1,0},{0,0,1}}] ]
	
	
	
	g1 = Table[ Plot[x^n, {x,0,5}], {n,5}];
	Show[ StackGraphics[g1] ] 
	
	
	
	g1 = Plot[ Sin[x],{x,0,Pi}];
	g2 = Plot[ Sin[x+0.5],{x,0,Pi}];
	g3 = Plot[ Sin[x+1],{x,0,Pi}];
	Show[ StackGraphics[{g1,g2,g3}] ]
	
	
	
	ScatterPlot3D[ Table[ { t,Sin[t],Cos[t]},{t,0,10,0.1}]]
	ScatterPlot3D[ Table[ { t,Sin[t],Cos[t]},{t,0,10,0.1}],PlotJoined->True]
	
	
	
	graphics = Plot3D[Sin[x y],{x,0,Pi},{y,0,Pi}];
	Show[ Project[ graphics, {1,1,0}] ]
	Shadow[ graphics, ZShadow -> False ]

*)
