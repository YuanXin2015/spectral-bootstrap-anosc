(* ::Package:: *)

(* ::Section:: *)
(*sub-routines*)


(* ::Subsubsection:: *)
(*define function for single sdpb run - mixed - all gradients*)


Clear[execMixedOptAllGrads]
Options[execMixedOptAllGrads]={
dualityGapThreshold->10^-30,
saveSDP->False,
precision->512,
path->ioPath,
redirect->False
};
execMixedOptAllGrads[K_,\[Omega]val_,gval_,eShift_,OptionsPattern[]][varsList_]:=Module[
{
sdpGen=sdpOptMixedAllGrads[K]["sdp"],dsdp=sdpOptMixedAllGrads[K]["dsdp"],
prec10=Log10[2^OptionValue[precision]]//Round,
pvm2sdp="exe/pvm2sdp",
pvm2sdpPrec=OptionValue[precision],
approxObj="exe/approx_objective",
out,sdp,newSDP,newSDP$xml,newSDP$sdp,newSDP$xmlFdr,
newSDP$approx,gradient,gradComponent,
\[CapitalDelta]list=varsList[[;;2]],
\[Lambda]list=varsList[[3;;]]
},
dsdp=dsdp[\[Omega]val,gval,Sequence@@(SetPrecision[\[CapitalDelta]list+eShift,prec10]),
Sequence@@(SetPrecision[\[Lambda]list,prec10])];
out=runSDP[
sdp=SetPrecision[
sdpGen[\[Omega]val,gval,Sequence@@(\[CapitalDelta]list+eShift),
Sequence@@(\[Lambda]list)],
prec10]
,OptionValue[path],
sdpbOpt->"--precision "<>ToString[pvm2sdpPrec]<>" --primalErrorThreshold=1e-20 --dualErrorThreshold=1e-20 --initialMatrixScalePrimal=1e+10 --initialMatrixScaleDual=1e+10 --maxComplementarity=1e+100 --dualityGapThreshold="<>ToString[CForm[OptionValue[dualityGapThreshold]]],
sdpbCenteringOpt->"--precision "<>ToString[pvm2sdpPrec]<>" --initialMatrixScalePrimal=1e+20 --initialMatrixScaleDual=1e+20 --maxComplementarity=1e+100",
centering->True,
deleteFiles->False,
redirect->OptionValue[redirect]
];
gradient=Table[
newSDP=dsdp[[idxGrad]];
(*tempNewSDP1=newSDP;*)
newSDP$xmlFdr=ioPath<>"/newSDP_"<>ToString[idxGrad]<>"_xmls";
newSDP$xml=writeSDPToFiles[newSDP,newSDP$xmlFdr,prec10];
newSDP$sdp=ioPath<>"/newSDP_"<>ToString[idxGrad]<>".sdp";
newSDP$approx=ioPath<>"/newSDP_"<>ToString[idxGrad]<>"_approx";
Run[StringRiffle[{pvm2sdp,ToString[pvm2sdpPrec],newSDP$xml,newSDP$sdp}]];
Run[StringRiffle[{
approxObj,
"--precision",ToString[pvm2sdpPrec],
"--sdp",out["sdpPath"],
"--solutionDir",out["outPath"],
"--newSdp",newSDP$sdp,
"--linear",
">",newSDP$approx
}] ];
gradComponent=getApproxObjOutput[newSDP$approx];
Run["rm -rf "<>newSDP$approx];
Run["rm -rf "<>newSDP$xmlFdr];
Run["rm -rf "<>newSDP$xml];
Run["rm -rf "<>newSDP$sdp];
gradComponent
,
{idxGrad,Length[dsdp]}
];
If[True(*OptionValue[deleteFiles]*),
Run["rm -rf "<>out["xmlFdr"]];
Run["rm -rf "<>out["sdpPath"]];
Run["rm -rf "<>out["outPath"]];
Run["rm -rf "<>out["sdpPath"]<>".ck"];
];
If[OptionValue[saveSDP],AppendTo[out,"sdp"->sdp]];
Prepend[
out,
<|
"Pobj"->out[dualObjective],
"gradient"->gradient
|>
]
]


ClearAll[execFindBoundaryFromAllDir]
Options[execFindBoundaryFromAllDir]={
debugPrint->False,
maxSteps->30,
dualityGapThreshold->10^-30,
precision->512,
objTol->10^-30
};
execFindBoundaryFromAllDir[K_,\[Omega]_,g_,eShift_,varsCenter_,zoomOutFactor_,OptionsPattern[]][dir_]:=Module[
{
op=execMixedOptAllGrads[K,\[Omega],g,eShift,
	dualityGapThreshold->OptionValue[dualityGapThreshold],
	saveSDP->True,
	precision->OptionValue[precision],
	path->ioPath(*<>"/"<>StringRiffle[ToString[CForm[#]]&/@N[dir],"_"]*),
	redirect->"sdpb_terminal_"<>StringRiffle[ToString[CForm[#]]&/@N[dir],"_"]
],
out,x,dx,varsIni=varsCenter+dir*zoomOutFactor
},
x=varsIni;
Do[
If[OptionValue[debugPrint],printMsg["step ",step]];
out=op[x];
If[OptionValue[debugPrint],printMsg[{out["Pobj"],out["gradient"] . dir}//N] ];
If[Abs[out["Pobj"]]<OptionValue[objTol],Break[]];
dx=-out["Pobj"]/out["gradient"] . dir;
dx=dir*dx;
(*dx={-out["Pobj"]/out["gradient"][[1]],0,0,0,0};*)
x=x+dx,
{step,OptionValue[maxSteps]}
];
Prepend[out,{"varsList"->x,"dir"->dir,"varsIni"->varsIni,"varsCenter"->varsCenter,"zoomOutFactor"->zoomOutFactor}]
];


truncSol={
SetPrecision[2.83985723876017052470229034647809080128766095243511813106208822353411843936658,100],(*Rationalize[2.9993246377107723`,10^-16]-25/4,*)
SetPrecision[2.99932463771076401977148622445226312284539885236057585700469856619246409525069,100],
SetPrecision[2.02444912500648196827130786184780475819171552062164746418706427499793357116715,100],
SetPrecision[2.21887682633257870946743973346182279563938957382553145989306493810740642785601,100],
SetPrecision[1.38231605238549568982380968699200613182606688130015025835424238523414646883074,100]
};
