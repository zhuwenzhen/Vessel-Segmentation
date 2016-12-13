(* ::Package:: *)

(* ::Section:: *)
(*Helper function*)


PNGtoGray[filename_]:=Reverse[Map[Map[First,#]&, Import[filename,"Data"]]];
GIFtoGray[filename_]:=255-Reverse[Map[Map[First,#]&, Import[filename,"Data"]]];
showGrayImage[img_]:=Graphics[Raster[img/Max[img]], Frame->False];
showBinaryImage[img_]:=Graphics[Raster[img], Frame->False];
showBinaryOverlay[img_,mask_]:=Graphics[Raster[MapThread[MapThread[If[#2==1,{1,0,0},{#1,#1,#1}]&,{#1,#2}]&,
{img,mask}]],Frame->False];
showGrayOverlay[img_,mask_]:=Graphics[{Raster[img/Max[img]],Raster[Map[Map[{#,0,0,If[#==1,.5,0]}&,#]&,mask]]},Frame->False];


(* ::Section:: *)
(*Morphological Operators*)


kernelFromStructure[structure_] := Block[
	{size = 2 Max @ Abs @ MinMax @ structure + 1},
	Transpose@ReplacePart[Table[0, size, size], Thread[Append[structure, {0, 0}] + (size + 1)/2 -> 1]]
]



(* this erosion, dilation, opening, closing is for the normal squared neighbors *)
erosion[imageData_List, scale_]:= Block[
	{neighbors, paddedData},
	
	(* padding outmost image with 1 *)
	paddedData = ArrayPad[imageData, (scale - 1)/2, 1];
	neighbors = Partition[paddedData, Table[scale, 2], 1];	
	Map[Min[#]&, neighbors, {2}]
]

dilation[imageData_List, scale_]:= Block[
	{neighbors, paddedData},
	(* padding outpost boundary with 0 *)
	paddedData = ArrayPad[imageData, (scale - 1)/2, 0];
	neighbors = Partition[paddedData, Table[scale, 2], 1];
	Map[Max[#]&, neighbors, {2}]
]

opening[imageData_List, scale_]:= 
	dilation[erosion[imageData, scale], scale]

closing[imageData_List, scale_]:= 
	erosion[dilation[imageData, scale], scale]
	
topHat[imageData_List, scale_]:= imageData - opening[imageData, scale]


geodesicDilation[marker_, mask_, 0]:= mask
geodesicDilation[marker_, mask_, 1]:= Block[
	{neighbors, paddedData, dilationResult},	
	paddedData = ArrayPad[marker, 1, 0];
	neighbors = Partition[paddedData, Table[3, 2], 1];	
	dilationResult = Map[Max[#]&, neighbors, {2}];		
	MapThread[Min[#1, #2]&, {dilationResult, mask}, 2]
]
geodesicDilation[marker_, mask_ n_]:=
	geodesicDilation[geodesicDilation[marker, mask, n - 1], mask, 1]
	
	

geodesicErosion[marker_, mask_, 0]:= mask
geodesicErosion[marker_, mask_, 1]:= Block[
	{neighbors, paddedData, dilationResult},	
	paddedData = ArrayPad[marker, 1, 0];
	neighbors = Partition[paddedData, Table[3, 2], 1];	
	dilationResult = Map[Min[#]&, neighbors, {2}];		
	MapThread[Min[#1, #2]&, {dilationResult, mask}, 2]
]
geodesicErosion[marker_, mask_ n_]:=
	geodesicErosion[geodesicErosion[marker, mask, n - 1], mask, 1]
	
(* Use the built-in GeodesicDilation *)
(* have to use my own geodesic Opening because the built-in one doesn't support mask*)
geodesicOpening[marker_, mask_]:= 
	MapThread[Max, ImageData/@ FixedPointList[GeodesicDilation[#, mask]&, marker], 2]
	
(*morphologicalReconstructionByDilation[marker_, mask_]:= 
	FixedPointList[GeodesicDilation[#, mask]&, marker]

morphologicalReconstructionByErosion[marker_, mask_]:= 
	FixedPointList[GeodesicErosion[#, mask]&, marker]*)


(* marker and mask are both data *)
(*geodesicClosing[marker_, mask_]:= Block[
	{Nmax, maskData, markerData, temp, openingResult},
	maskData = ImageData @ mask;
	Nmax = Max[maskData];
	markerData = Nmax - ImageData[marker];
	temp = Nmax - maskData;	
	openingResult = geodesicOpening[markerData, temp];
	Nmax - openingResult
]
*)

geodesicClosing[marker_, mask_]:= Block[
	{Nmax, markerC, maskC},
	Nmax = Max[mask];
	markerC = Nmax - marker;
	maskC = Nmax - mask;	
	Nmax - geodesicOpening[Image @ markerC, Image@maskC]	
]
