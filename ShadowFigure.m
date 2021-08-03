(* ::Package:: *)

(* :Mathematica Version: 12.1 *)

(* :Name: ShadowFigure *)

(* :Title: ShadowFigure *)

(* :Author: Li Ruilong *)
(* :\:6b22\:8fce\:5173\:6ce8\:9752\:5d16\:540c\:5b66\:5fae\:4fe1\:516c\:4f17\:53f7*)

(* :Summary:
This package provides functions for shadow plots.
*)

(* :Package Version: 1.1 *)

(* :Keywords:
	Project, Shadow
*)

BeginPackage["ShadowFigure`"]


ShadowFigure::usage = 
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


Begin["`Private`"]


Options[ShadowFigure]=
	Join[{XShadow->True,YShadow->True, ZShadow->True,XShadowPosition->Automatic,YShadowPosition->Automatic,ZShadowPosition->Automatic}];
ShadowFigure[graph_Graphics3D,shadowOpts___]:=Module[{xshadow,yshadow,zshadow,xshadowposition,yshadowposition,zshadowposition},
{xshadow,yshadow,zshadow,xshadowposition,yshadowposition,zshadowposition}={XShadow,YShadow,ZShadow,XShadowPosition,YShadowPosition,ZShadowPosition}/.{shadowOpts}/.Options[ShadowFigure];
lim=Transpose@PlotRange@graph;
deltaLim=Differences@lim;
meanLim=Mean@lim;
maxLim=1.5*Max@deltaLim/2;
newLim={meanLim-maxLim,meanLim+maxLim}//Transpose;
xPos=If[xshadowposition===Automatic,newLim[[1,1]],xshadowposition];
yPos=If[yshadowposition===Automatic,newLim[[2,2]],yshadowposition];
zPos=If[zshadowposition===Automatic,newLim[[3,1]],zshadowposition];
newLim[[1,1]]=xPos;
newLim[[2,2]]=yPos;
newLim[[3,1]]=zPos;
graph/.Graphics3D[gr_,grOpts___]:>Graphics3D[{gr,Scale[{RGBColor["#000000"],Opacity[0.1],gr},#,{xPos,yPos,zPos}]&/@(1+10^-3-DiagonalMatrix@Boole@{xshadow,yshadow,zshadow})},grOpts,PlotRange->newLim]]


End[] (* Private` *)

EndPackage[]

(* :Examples:
	
	ShadowFigure[Graphics3D[Cuboid[]]]
	
	ShadowFigure[Graphics3D[Cuboid[]],XShadowPosition\[Rule]10]
	
	ShadowFigure[Graphics3D[Cuboid[]],XShadow\[Rule]False]
	
	ShadowFigure[Graphics3D[Cuboid[]],XShadow\[Rule]False,ZShadowPosition\[Rule]-5]

*)
