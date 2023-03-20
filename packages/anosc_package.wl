(* ::Package:: *)

(* ::Chapter:: *)
(*External tools*)


(* ::Section:: *)
(*SDPB IO *)


(* ::Subsection::Closed:: *)
(*read output*)


readMatrixFile[file_]:=Block[
{dat,dims},
dat=StringSplit[
ReadString[file],
"\n"];
dims=ToExpression/@StringSplit[dat[[1]]," "];
ArrayReshape[
ToExpression/@StringReplace[dat[[2;;]],"e"->"*^"],
dims]
]


readMatricesList[path_,title_]:=Block[
{
(*path="/mnt/sda1/Dropbox/myml/spin-extremal/dualBFGS/tempSDP.out",
title="x",*)
files,indices,orders
},
files=FileNames[path<>"/"<>title<>"_*.txt",IgnoreCase->False];
indices=Flatten[ Reap[StringReplace[files,title~~"_"~~num__~~".txt":>Sow[ToExpression[num]]]  ][[2]] ];
(*Print[orders];
orders[[ Ordering[orders] ]]*)
orders=Ordering[indices];
readMatrixFile/@files[[orders]]
]


readSolution[path_]:=Block[
{
(*path="/mnt/sda1/Dropbox/myml/spin-extremal/dualBFGS/tempSDP.out",*)
xList,yList,matXList,matYList,outputParameters
},
xList=Flatten@readMatricesList[path,"x"];
yList=Flatten@readMatrixFile[path<>"/y.txt"];
matXList=Flatten@readMatricesList[path,"X_matrix"];
matYList=Flatten@readMatricesList[path,"Y_matrix"];
outputParameters=outputReader[path<>"/out.txt"];
Append[outputParameters,
<|"x"->xList,"y"->yList,"X"->matXList,"Y"->matYList|>
]
]


replaceExponentialMark[str_]:=StringReplace[str,"e"->"*^"]


outputReader[filePath_]:=Module[
	{fileHdl,terminateReasonRead,primalObjectiveRead,dualObjectiveRead,dualityGapRead,primalErrorRead,dualErrorRead},
	fileHdl=OpenRead[filePath];
	(*(*terminateReason=*)Read[fileHdl];*)
	terminateReasonRead=StringSplit[Read[fileHdl,String],"\""][[-2]];
	primalObjectiveRead=StringSplit[Read[fileHdl,String],{" ",";"}][[-1]]//replaceExponentialMark//ToExpression;
	dualObjectiveRead=StringSplit[Read[fileHdl,String],{" ",";"}][[-1]]//replaceExponentialMark//ToExpression;
	dualityGapRead=StringSplit[Read[fileHdl,String],{" ",";"}][[-1]]//replaceExponentialMark//ToExpression;
	primalErrorRead=StringSplit[Read[fileHdl,String],{" ",";"}][[-1]]//replaceExponentialMark//ToExpression;
	dualErrorRead=StringSplit[Read[fileHdl,String],{" ",";"}][[-1]]//replaceExponentialMark//ToExpression;
	Close[fileHdl];
	Association[{
		terminateReason->terminateReasonRead,
		primalObjective->primalObjectiveRead,
		dualObjective->dualObjectiveRead,
		dualityGap->dualityGapRead,
		primalError->primalErrorRead,
		dualError->dualErrorRead
	}]
];


(* ::Subsection:: *)
(*run SDPB*)


ClearAll[runSDP]
Options[runSDP]={
sdpbOpt->"--precision 768 --noFinalCheckpoint --primalErrorThreshold=1e-60 --dualErrorThreshold=1e-60 --initialMatrixScalePrimal=1e+20 --initialMatrixScaleDual=1e+20 --maxComplementarity=1e+100",
sdpbCenteringOpt->"--precision 768 --initialMatrixScalePrimal=1e+20 --initialMatrixScaleDual=1e+20 --maxComplementarity=1e+100",
(*sdpbPath\[Rule]"mpirun -n 4 sdpb_exp --procsPerNode=4",*)
sdpbPath->"exe/sdpb",
(*sdp2inputPath\[Rule]"mpirun -n 4 sdp2input_exp",*)
pvm2sdpPath->"exe/pvm2sdp",
writeInput->True,
inputPrecision->768,
deleteFiles->True,
centering->False,
redirect->False
};
runSDP[sdp_,path_,OptionsPattern[]]:=Block[
{sdpb=OptionValue[sdpbPath],
pvm2sdp=OptionValue[pvm2sdpPath],
opt=OptionValue[sdpbOpt],
prec=OptionValue[inputPrecision],
xmlFdr,xmlPath,sdpPath,outputPath,sdpbStatus,
sol,db,db0,dc,dB,x,y,gradient,sol2,
redirectStr
},
Run["mkdir -p "<>path];

xmlFdr=path<>"/xmls";
sdpPath=path<>"/input.sdp";
outputPath=path<>"/output.out";
redirectStr=If[OptionValue[redirect]=!=False&&StringQ[OptionValue[redirect]],
	">> "<>path<>"/"<>OptionValue[redirect]<>".txt",
	""
];
(*Put[sdpPackage["sdp"],mmasdpPath];*)
If[OptionValue[writeInput],
xmlPath=writeSDPToFiles[sdp,xmlFdr,Log[2^prec]//Round];
Run[StringRiffle[{pvm2sdp,ToString[prec],xmlPath,sdpPath}]];
];
sdpbStatus=Run[StringRiffle[{
sdpb,opt,"--writeSolution x,y,X,Y",
"-s",sdpPath,
"-o",outputPath,
redirectStr
}] ];
If[sdpbStatus=!=0,Print["sdpb failed"];Return[False]];
sol=readSolution[outputPath];
AppendTo[sol,
{"xmlFdr"->xmlFdr,
"sdpPath"->sdpPath,
"outPath"->outputPath}];
If[OptionValue[centering],
Run[tempStr=StringRiffle[{
sdpb,OptionValue[sdpbCenteringOpt],"--writeSolution x,y,X,Y",
"--maxIterations 10",
"--infeasibleCenteringParameter=1",
"--stepLengthReduction=1",
"--dualityGapThreshold=0",
"--primalErrorThreshold=0",
"--dualErrorThreshold=0",
"-s",sdpPath,
"-o",outputPath,
redirectStr
}] ];
(*Run[StringRiffle[{
sdpb,OptionValue[sdpbCenteringOpt],"--writeSolution x,y,X,Y",
"--maxIterations 10",
"--feasibleCenteringParameter 1",
"-s",sdpPath,
"-o",outputPath
}] ];*)
];
sol2=readSolution[outputPath];
AppendTo[sol2,
{"xmlFdr"->xmlFdr,
"sdpPath"->sdpPath,
"outPath"->outputPath}];
If[OptionValue[deleteFiles],
Run["rm -rf "<>xmlFdr];
Run["rm -rf "<>sdpPath];
Run["rm -rf "<>outputPath];
Run["rm -rf "<>sdpPath<>".ck"];
];
If[OptionValue[centering],
Prepend[sol2,terminateReason->sol[terminateReason]],
sol]
]


(* ::Subsection::Closed:: *)
(*restore coeff*)


restoreCoefficients[yVector_,norm_]:=Module[
	{\[Alpha],j=maxIndexBy[norm,Abs],length=Length[norm],shortNorm},
	shortNorm=Delete[norm,j];
	\[Alpha]=Array[#/.{i_:>yVector[[i]]/;i<j,i_:>yVector[[i-1]]/;i>j,i_:>(1-yVector . shortNorm)/norm[[j]]/;i==j}&,length];
	Return[\[Alpha]];
];
(*readCoefficient[filePath_,norm_]:=restoreCoefficients[outputReader[filePath]["yVector"],norm];*)


(* ::Subsection::Closed:: *)
(*getApproxObjOutput*)


getApproxObjOutput[file_]:=Block[
	{stream=OpenRead[file],
		string,num
	},
	(*string=ReadList[stream,String][[5]];*)
	string=Find[stream,"d_objective"];
	num=StringReplace[StringSplit[string,"\""][[-1]],"e"->"*^"]//ToExpression;
	Close[stream];
	num
]


(* ::Section:: *)
(*write sdp to files *)


(* ::Subsection::Closed:: *)
(*import from SDPB.m*)


(* A matrix with constant anti-diagonals given by the list bs *)
antiBandMatrix[bs_] := Module[
    {n = Ceiling[Length[bs]/2]},
    Reverse[Normal[
        SparseArray[
            Join[
                Table[Band[{i, 1}] -> bs[[n - i + 1]], {i, n}],
                Table[Band[{1, i}] -> bs[[n + i - 1]], {i, 2, n}]],
            {n, n}]]]];

(* DampedRational[c, {p1, p2, ...}, b, x] stands for c b^x / ((x-p1)(x-p2)...) *)
(* It satisfies the following identities *)

DampedRational[const_, poles_, base_, x + a_] := 
    DampedRational[base^a const, # - a & /@ poles, base, x];

DampedRational[const_, poles_, base_, a_ /; FreeQ[a, x]] := 
    const base^a/Product[a - p, {p, poles}];

DampedRational/:x DampedRational[const_, poles_ /; MemberQ[poles, 0], base_, x] :=
    DampedRational[const, DeleteCases[poles, 0], base, x];

DampedRational/:DampedRational[c1_,p1_,b1_,x] DampedRational[c2_,p2_,b2_,x] :=
    DampedRational[c1 c2, Join[p1, p2], b1 b2, x];

(* bilinearForm[f, m] = Integral[x^m f[x], {x, 0, Infinity}] *)
(* The special case when f[x] has no poles *)
bilinearForm[DampedRational[const_, {}, base_, x], m_] :=
    const Gamma[1+m] (-Log[base])^(-1-m);

(*memoizeGamma[a_,b_]:=memoizeGamma[a,b]=Gamma[a,b];*)

(* The case where f[x] has only single poles *)
(*bilinearForm[DampedRational[const_, poles_, base_, x], m_] := 
    const Sum[
        ((-poles[[i]])^m) ( base^poles[[i]]) Gamma[1 + m] memoizeGamma[-m, poles[[i]] Log[base]]/
        Product[poles[[i]] - p, {p, Delete[poles, i]}],
        {i, Length[poles]}];*)

(* The case where f[x] can have single or double poles *)
bilinearForm[DampedRational[c_, poles_, b_, x_], m_] := Module[
    {
        gatheredPoles = Gather[poles],
        quotientCoeffs = CoefficientList[PolynomialQuotient[x^m, Product[x-p, {p, poles}], x], x],
        integral, p, rest
    },
    integral[a_,1] := b^a Gamma[0, a Log[b]];
    integral[a_,2] := -1/a + b^a Gamma[0, a Log[b]] Log[b];
    c (Sum[
        p = gatheredPoles[[n,1]];
        rest = x^m / Product[x-q, {q, Join@@Delete[gatheredPoles, n]}];
        Switch[Length[gatheredPoles[[n]]],
               1, integral[p,1] rest /. x->p,
               2, integral[p,2] rest + integral[p,1] D[rest, x] /. x->p],
        {n, Length[gatheredPoles]}] + 
       Sum[
           quotientCoeffs[[n+1]] Gamma[1+n] (-Log[b])^(-1-n),
           {n, 0, Length[quotientCoeffs]-1}])];

(* orthogonalPolynomials[f, n] is a set of polynomials with degree 0
through n which are orthogonal with respect to the measure f[x] dx *)
orthogonalPolynomials[const_ /; FreeQ[const, x], 0] := {1/Sqrt[const]};

orthogonalPolynomials[const_ /; FreeQ[const, x], degree_] := 
    error["can't get orthogonal polynomials of nonzero degree for constant measure"];

orthogonalPolynomials[DampedRational[const_, poles_, base_, x], degree_] := 
    Table[x^m, {m, 0, degree}] . Inverse[
        CholeskyDecomposition[
            antiBandMatrix[
                Table[bilinearForm[DampedRational[const, Select[poles, # < 0&], base, x], m],
                      {m, 0, 2 degree}]]]];

(* Preparing SDP for Export *)
(*rhoCrossing := SetPrecision[3-2 Sqrt[2], prec];*)

rescaledLaguerreSamplePoints[n_] := Table[
    SetPrecision[\[Pi]^2 (-1+4k)^2/(-64n Log[rhoCrossing]), prec],
    {k,0,n-1}];

maxIndexBy[l_,f_] := SortBy[
    Transpose[{l,Range[Length[l]]}],
    -f[First[#]]&][[1,2]];

(* finds v' such that a . v = First[v'] + a' . Rest[v'] when normalization . a == 1, where a' is a vector of length one less than a *)
reshuffleWithNormalization[normalization_, v_] := Module[
    {j = maxIndexBy[normalization, Abs], const},
    const = v[[j]]/normalization[[j]];
    Prepend[Delete[v - normalization*const, j], const]];

(* XML Exporting *)
nf[x_Integer] := x;
nf[x_] := NumberForm[SetPrecision[x,prec],prec,ExponentFunction->(Null&)];

safeCoefficientList[p_, x_] := Module[
    {coeffs = CoefficientList[p, x]},
    If[Length[coeffs] > 0, coeffs, {0}]];

WriteBootstrapSDP[file_, SDP[objective_, normalization_, positiveMatricesWithPrefactors_], precision_] := Block[
{prec=precision},Module[
    {
        stream = OpenWrite[file],
        node, real, int, vector, polynomial,
        polynomialVector, polynomialVectorMatrix,
        affineObjective, polynomialVectorMatrices
    },

    (* write a single XML node to file.  children is a routine that writes child nodes when run. *)
    node[name_, children_] := (
        WriteString[stream, "<", name, ">"];
        children[];
        WriteString[stream, "</", name, ">\n"];
    );

    real[r_][] := WriteString[stream, nf[r]];
    int[i_][] := WriteString[stream, i];
    vector[v_][] := Do[node["elt", real[c]], {c, v}];
    polynomial[p_][] := Do[node["coeff", real[c]], {c, safeCoefficientList[p,x]}];
    polynomialVector[v_][] := Do[node["polynomial", polynomial[p]], {p, v}];

    polynomialVectorMatrix[PositiveMatrixWithPrefactor[prefactor_, m_]][] := Module[
        {degree = Max[Exponent[m, x]], samplePoints, sampleScalings, bilinearBasis},
		
		Block[
			{rhoCrossing},
			If[FreeQ[prefactor,x],
				rhoCrossing = 1/E,
				prefactor/.DampedRational[const_, {}, base_, x]:>(rhoCrossing = base)
			];
			samplePoints   = rescaledLaguerreSamplePoints[degree + 1];
		];
        (*samplePoints   = rescaledLaguerreSamplePoints[degree + 1];*)
        sampleScalings = Table[prefactor /. x -> a, {a, samplePoints}];
        bilinearBasis  = orthogonalPolynomials[prefactor, Floor[degree/2]];
        node["rows", int[Length[m]]];
        node["cols", int[Length[First[m]]]];
        node["elements", Function[
            {},
            Do[node[
                "polynomialVector",
                polynomialVector[reshuffleWithNormalization[normalization,pv]]],
               {row, m}, {pv, row}]]];
        node["samplePoints", vector[samplePoints]];
        node["sampleScalings", vector[sampleScalings]];
        node["bilinearBasis", polynomialVector[bilinearBasis]];
    ];

    node["sdp", Function[
        {},
        node["objective", vector[reshuffleWithNormalization[normalization, objective]]];
        node["polynomialVectorMatrices", Function[
            {},
            Do[node["polynomialVectorMatrix", polynomialVectorMatrix[pvm]], {pvm, positiveMatricesWithPrefactors}];
        ]];
    ]];                                          

    Close[stream];
] ];


(* ::Subsection::Closed:: *)
(*write single dSDP to file*)


WriteBootstrapSDP[file_, dSDP[{objective_,dObj_}, {normalization_,dNorm_}, positiveMatricesAndGradWithPrefactors_], precision_] := Block[
{prec=precision},Module[
    {
        stream = OpenWrite[file],
        node, real, int, vector, polynomial,
        polynomialVector, polynomialVectorMatrix,
        affineObjective, polynomialVectorMatrices,
        pv,dpv,dimN,kStar,matQ,dMatQ,vecQ0,dVecQ0,dNRatio
    },
    dimN=Length[normalization];
	kStar=maxIndexBy[normalization,Abs];
	matQ=SparseArray[
		{
			{n_,k_}/;n<kStar&&n==k:>1,
			{n_,k_}/;n>=kStar&&n+1==k:>1,
			{n_,kStar}/;n<kStar:>-normalization[[n]]/normalization[[kStar]],
			{n_,kStar}/;n>=kStar:>-normalization[[n+1]]/normalization[[kStar]]
		},
		{dimN-1,dimN}
	];
	dNRatio=Table[With[{kp=If[n<kStar,n,n+1]},
		(dNorm[[kp]]*normalization[[kStar]]-normalization[[kp]]*dNorm[[kStar]])/normalization[[kStar]]^2
	],{n,dimN-1}];
	dMatQ=SparseArray[
		{
			{n_,kStar}:>-dNRatio[[n]]
		},
		{dimN-1,dimN}
	];
	vecQ0=SparseArray[{kStar->1/normalization[[kStar]]},{dimN}];
	dVecQ0=SparseArray[{kStar}:>-dNorm[[kStar]]/normalization[[kStar]]^2,{dimN}];
	matQ=Join[ArrayReshape[vecQ0,{1,dimN}],matQ];
	dMatQ=Join[ArrayReshape[dVecQ0,{1,dimN}],dMatQ];
    (* write a single XML node to file.  children is a routine that writes child nodes when run. *)
    node[name_, children_] := (
        WriteString[stream, "<", name, ">"];
        children[];
        WriteString[stream, "</", name, ">\n"];
    );

    real[r_][] := WriteString[stream, nf[r]];
    int[i_][] := WriteString[stream, i];
    vector[v_][] := Do[node["elt", real[c]], {c, v}];
    polynomial[p_][] := Do[node["coeff", real[c]], {c, safeCoefficientList[p,x]}];
    polynomialVector[v_][] := Do[node["polynomial", polynomial[p]], {p, v}];

    polynomialVectorMatrix[PositiveMatrixWithPrefactor[prefactor_, m_]][] := Module[
        {degree = Max[Exponent[m, x]], samplePoints, sampleScalings, bilinearBasis},
		
		Block[
			{rhoCrossing},
			If[FreeQ[prefactor,x],
				rhoCrossing = 1/E,
				prefactor/.DampedRational[const_, {}, base_, x]:>(rhoCrossing = base)
			];
			samplePoints   = rescaledLaguerreSamplePoints[degree + 1];
		];
        (*samplePoints   = rescaledLaguerreSamplePoints[degree + 1];*)
        sampleScalings = Table[prefactor /. x -> a, {a, samplePoints}];
        bilinearBasis  = orthogonalPolynomials[prefactor, Floor[degree/2]];
        node["rows", int[Length[m]]];
        node["cols", int[Length[First[m]]]];
        node["elements", Function[
            {},
            Do[{pv,dpv}=matElem;
                node[
                    "polynomialVector",
                    polynomialVector[matQ . pv+matQ . dpv+dMatQ . pv] ],
                    (*polynomialVector[reshuffleWithNormalization[normalization,pv]]],*)
               {row, m}, {matElem, row}]]];
        node["samplePoints", vector[samplePoints]];
        node["sampleScalings", vector[sampleScalings]];
        node["bilinearBasis", polynomialVector[bilinearBasis]];
    ];

    node["sdp", Function[
        {},
        (*node["objective", vector[reshuffleWithNormalization[normalization, objective]]];*)
        node["objective", vector[matQ . objective+dMatQ . objective+matQ . dObj]];
        node["polynomialVectorMatrices", Function[
            {},
            Do[node["polynomialVectorMatrix", polynomialVectorMatrix[pvm]], {pvm, positiveMatricesAndGradWithPrefactors}];
        ]];
    ]];                                          

    Close[stream];
] ];


(* ::Subsection::Closed:: *)
(*parallel submit xml*)


safeOrderStr[int_,ndigit_:5]:=ToString@NumberForm[int,ndigit,NumberPadding->{"0", ""}]


ClearAll[writeSDPToFiles]
writeSDPToFiles[sdp_,path_,prec_]:=Module[
	{jobs,head},
	Run["mkdir -p "<>path];
	jobs=Table[
		With[
			{
				subSDP=Head[sdp][
					sdp[[1]],
					sdp[[2]],
					sdp[[3,{idx}]]
				],
				subIdx=safeOrderStr[idx],
				subPath=path,
				subPrec=prec
			},
			ParallelSubmit[
				WriteBootstrapSDP[subPath<>"/"<>subIdx<>".xml",subSDP,subPrec]
			]
		],
		{idx,sdp[[3]]//Length}
	];
	WaitAll[jobs];
	Run["find "<>path<>"/ -name \"*.xml\" -print0 | sort -z > "<>path<>"/file_list.nsv"];
	path<>"/file_list.nsv"
]


(* ::Chapter:: *)
(*Define Bootstrapping Anharmonic oscillator*)


(* ::Section::Closed:: *)
(*Set potential*)


(* ::Input::Initialization:: *)
\[ScriptCapitalV][x_]=\[Omega]^2 x^2+g x^4;


(* ::Section::Closed:: *)
(*Define*)


ClearAll[comm,op]
(*SetAttributes[op,Flat];*)
op[z___,a_*b_,y___]/;FreeQ[a,x|p]:=a op[z,b,y];
op[a_]:=a;
op[z___,a_/;FreeQ[a,x]&&FreeQ[a,p],y___]:=a op[z,y];
op[z___,a_+b_,y___]:= op[z,a,y]+op[z,b,y];
op[z___,HoldPattern[a_/;MatchQ[a,x|x^_]],HoldPattern[b_/;MatchQ[b,x|x^_]],y___]:=op[z,x^(Exponent[a,x]+Exponent[b,x]),y];
op[z___,HoldPattern[a_/;MatchQ[a,p|p^_]],HoldPattern[b_/;MatchQ[b,p|p^_]],y___]:=op[z,p^(Exponent[a,p]+Exponent[b,p]),y];
op[z___,HoldPattern[op[a__]],y___]:=op[z,a,y];

op[z___,
HoldPattern[a_/;MatchQ[a,p|p^_]],
HoldPattern[b_/;MatchQ[b,x|x^_]],
y___]:=op[z,(op[b,a]+comm[a,b]),y];

comm[a_,a_]=0;
comm[a_,b_/;FreeQ[b,x|p]]=0;
comm[p,x]:=-I;
comm[x,p]:=I;
comm[a_+b_,y_]:=(
(*Print["[a+b,y]: ",comm[a,y]+comm[b,y]//HoldForm];*)
comm[a,y]+comm[b,y]
);
comm[HoldPattern[a_*b_/;FreeQ[a,x|p]],c_]:=(
(*Print["[a*b,c]: ",a comm[b,c]//HoldForm];*)
a comm[b,c]
);
comm[y_,a_+b_]:=(
(*Print["[y,a+b]: ",comm[y,a]+comm[y,b]//HoldForm];*)
comm[y,a]+comm[y,b]
);
comm[c_,a_*b_/;FreeQ[a,x|p]]:=(
(*Print["[c,ab]: ",a comm[c,b]//HoldForm];*)
a comm[c,b]
);
comm[HoldPattern[op[a__,b__]],c_]:=(
(*Print["[ab,c]: ",
op[op[a], comm[op[b],c]]+op[ comm[op[a],c],op[b]]//HoldForm
];*)
op[op[a], comm[op[b],c]]+op[ comm[op[a],c],op[b]]
);
comm[a_,HoldPattern[op[b__,c__]]]:=(
(*Print[
"[a,[b,c]]: ",
op[ comm[a,op[b]],op[c]]+op[op[b],comm[a,op[c]]]//HoldForm];*)
op[ comm[a,op[b]],op[c]]+op[op[b],comm[a,op[c]]]
);
comm[a_,HoldPattern[w_^m_]]:=(
(*Print[comm[a,w^m]=op[ comm[a,w],w^(m-1)]+op[w,comm[a,w^(m-1)]]//HoldForm];*)
comm[a,w^m]=op[ comm[a,w],w^(m-1)]+op[w,comm[a,w^(m-1)]]
);
comm[HoldPattern[w_^m_],c_]:=(
(*Print[comm[w^m,c]=op[w, comm[w^(m-1),c]]+op[ comm[w,c],w^(m-1)]//HoldForm];*)
comm[w^m,c]=op[w, comm[w^(m-1),c]]+op[ comm[w,c],w^(m-1)]
);


(* ::Section::Closed:: *)
(*Diagonal elements*)


ClearAll[opEval]
opEval[x^n_,p]:=opEval[x^n,p]=op[x^n,p]/.Solve[Expand[comm[
p^2+\[Omega]^2 x^2+g x^4,
op[x^(n+1)]
]]==0,op[x^n,p]][[1]];
opEval[x,p]:=opEval[x,p]=op[x,p]/.Solve[Expand[comm[
p^2+\[Omega]^2 x^2+g x^4,
op[x^2]
]]==0,op[x,p]][[1]];
opEval[x^n_,p^2]:=opEval[x^n,p^2]=z/.Solve[(Expand[comm[
p^2+\[Omega]^2 x^2+g x^4,
op[x^(n+1),p]
]]==0)/.{op->opEval,op[x^n,p^2]->z},z][[1]];
opEval[x,p^2]:=opEval[x,p^2]=z/.Solve[(Expand[comm[
p^2+\[Omega]^2 x^2+g x^4,
op[x^2,p]
]]==0)/.{op->opEval,op[x,p^2]->z},z][[1]];
opEval[x^n_,p^m_]:=opEval[x^n,p^m]=z/.Solve[(Expand[comm[
p^2+\[Omega]^2 x^2+g x^4,
op[x^(n+1),p^(m-1)]
]]==0)/.{op->opEval,op[x^n,p^m]->z},z][[1]];
opEval[x,p^m_]:=opEval[x,p^m]=z/.Solve[(Expand[comm[
p^2+\[Omega]^2 x^2+g x^4,
op[x^2,p^(m-1)]
]]==0)/.{op->opEval,op[x,p^m]->z},z][[1]];


ClearAll[evaluate]
Block[{expr},
expr=comm[
p^2+\[Omega]^2 x^2+g x^4,
op[x,p]
];
evaluate[p^2]=t/.Solve[(expr/.p^2->t)==0,t][[1]];
expr=op[
p^2+\[Omega]^2 x^2+g x^4-e,
1
]/. p^2->evaluate[p^2]//Expand;
evaluate[x^4]=t/.Solve[(expr/.x^4->t)==0,t][[1]];
expr=comm[
p^2+\[Omega]^2 x^2+g x^4,
op[p]
]/. p^2->evaluate[p^2]//Expand;
evaluate[x^3]=t/.Solve[(expr/.x^3->t)==0,t][[1]];
evaluate[p]=0;
];
evaluate[expr_]:=Expand[expr]//.{
x^n_/;n>2:>evaluate[x^n],
 p^n_:>evaluate[p^n]
};
evaluate[x^n_]/;n>4:=evaluate[x^n]=z/.Solve[
(Expand[op[
p^2+\[Omega]^2 x^2+g x^4-e,
x^(n-4)
]/.op->opEval]/.x^n:>z//.{
x^m_/;m>2:>evaluate[x^m],
 p^2:>evaluate[p^2]
}
)==0,
z][[1]];
evaluate[p^n_]/;n>2:=evaluate[p^n]=z/.Solve[
(Expand[comm[
p^2+\[Omega]^2 x^2+g x^4,
op[x,p^(n-1)]
]/.op->opEval]/.p^n:>z//.{
x^m_/;m>2:>evaluate[x^m],
 p^l_:>evaluate[p^l]
}
)==0,
z][[1]];
evaluate[p^2]=evaluate[p^2]//evaluate;


(* ::Section::Closed:: *)
(*Off-diag elems*)


ClearAll[opEvalOff];
opEvalOff[x^m_,p^n_]:=opEvalOff[x^m,p^n]=Block[
{operator=op[x^(m+1),p^(n-1)],expr},
expr=comm[
p^2+\[ScriptCapitalV][x],
operator
]-(e2-e1)operator//Expand;
t/.Solve[(expr/.op[x^m,p^n]->t)==0,t][[1]]
];
opEvalOff[x,p^n_]:=opEvalOff[x,p^n]=Block[
{operator=op[x^2,p^(n-1)],expr},
expr=comm[
p^2+\[ScriptCapitalV][x],
operator
]-(e2-e1)operator//Expand;
t/.Solve[(expr/.op[x,p^n]->t)==0,t][[1]]
];
opEvalOff[x^m_,p]:=opEvalOff[x^m,p]=Block[
{operator=op[x^(m+1)],expr},
expr=comm[
p^2+\[ScriptCapitalV][x],
operator
]-(e2-e1)operator//Expand;
t/.Solve[(expr/.op[x^m,p]->t)==0,t][[1]]
];
opEvalOff[x,p]:=opEvalOff[x,p]=Block[
{operator=op[x^2],expr},
expr=comm[
p^2+\[ScriptCapitalV][x],
operator
]-(e2-e1)operator//Expand;
t/.Solve[(expr/.op[x,p]->t/.op->opEvalOff)==0,t][[1]]
];
(*opEvalOff[p^n_]:=opEvalOff[p^n]=Block[
{operator=op[p^n],expr},
expr=comm[
p^2+\[ScriptCapitalV][x],
operator
]-(e2-e1)operator//Expand;
t/.Solve[(expr/.op[p^n]->t//.op->opEvalOff)==0,t][[1]]
];
opEvalOff[p]:=opEvalOff[p]=Block[
{operator=op[p],expr},
expr=comm[
p^2+\[ScriptCapitalV][x],
operator
]-(e2-e1)operator//Expand;
t/.Solve[(expr/.op[p]->t//.op->opEvalOff)==0,t][[1]]
];*)


ClearAll[evaluateOff]
evaluateOff[p^2]=Block[
{operator=op[x,p],expr},
expr=comm[
p^2+\[ScriptCapitalV][x],
operator
]-(e2-e1)operator(*//.op->opEvalOff*)//Expand;
t/.Solve[(expr/.p^2->t//.op->opEvalOff)==0,t][[1]]
];
evaluateOff[p]=Block[
{operator=op[x],expr},
expr=comm[
p^2+\[ScriptCapitalV][x],
operator
]-(e2-e1)operator(*//.op->opEvalOff*)//Expand;
t/.Solve[(expr/.p->t//.op->opEvalOff)==0,t][[1]]
];
evaluateOff[x^4]=Block[
{operator=1,expr},
expr=op[
operator,
p^2+\[ScriptCapitalV][x]
]-e1*operator//.op->opEvalOff//Expand;
expr=expr/. p^2->evaluateOff[p^2]//Expand;
t/.Solve[(expr/.x^4->t)==0,t][[1]]
];
evaluateOff[p^2]=Block[
{operator=op[x,p],expr},
expr=comm[
p^2+\[ScriptCapitalV][x],
operator
]-(e2-e1)operator(*//.op->opEvalOff*)//Expand;
t/.Solve[(expr/.p^2->t/.x^4->evaluateOff[x^4]//.op->opEvalOff)==0,t][[1]]
];
evaluateOff[x^3]=Block[
{operator=op[p],expr},
expr=comm[
p^2+\[ScriptCapitalV][x],
operator
]-(e2-e1)operator//.op->opEvalOff//Expand;
expr=expr/. p->evaluateOff[p]//Expand;
t/.Solve[(expr/.x^3->t)==0,t][[1]]
];
evaluateOff[x^n_]/;n>4:=evaluateOff[x^n]=Block[
{operator=x^(n-4),expr},
expr=op[
operator,
p^2+\[ScriptCapitalV][x]
]-e1*operator//.op->opEvalOff//Expand;
expr=expr/.x^n->t/.{
x^m_/;m>2:>evaluateOff[x^m],
 p^l_:>evaluateOff[p^l],
p->evaluateOff[p]
}//Expand;
t/.Solve[(expr)==0,t][[1]]
];
evaluateOff[p^n_]/;n>2:=evaluateOff[p^n]=Block[
{operator=op[x,p^(n-1)],expr},
expr=comm[
p^2+\[ScriptCapitalV][x],
operator
]-(e2-e1)operator//.op->opEvalOff//Expand;
expr=expr/.p^n->t/.{
x^m_/;m>2:>evaluateOff[x^m],
 p^l_:>evaluateOff[p^l],
p->evaluateOff[p]
};
t/.Solve[expr==0,t][[1]]
];
evaluateOff[expr_]:=Expand[expr]/.{
x^n_/;n>2:>evaluateOff[x^n],
p^n_:>evaluateOff[p^n],
p->evaluateOff[p]
};


(* ::Chapter:: *)
(*Spectral decomposition blocks*)


(* ::Section::Closed:: *)
(*3pt structures*)


ClearAll[block11,block12]
block11[i_]:=block11[i]=Function[
{e1val,\[Omega]val,gval},
PadRight[CoefficientList[(x^i//evaluate)/.p->0,x],3]/.{e->e1val,\[Omega]->\[Omega]val,g->gval}//Evaluate
];
block12[i_]:=block12[i]=Function[
{e1val,e2val,\[Omega]val,gval},
PadRight[CoefficientList[x^i//evaluateOff,x],3]/.{e1->e1val,e2->e2val,\[Omega]->\[Omega]val,g->gval}//Evaluate
];


(* ::Section::Closed:: *)
(*Block forms - single*)


blocksConst11[K_]:=Function[
{e1val,\[Omega]val,gval},
Flatten[Table[
{
{
block11[i][e1val,\[Omega]val,gval][[1]]block11[j][e1val,\[Omega]val,gval][[1]]-block11[i+j][e1val,\[Omega]val,gval][[1]],
block11[i][e1val,\[Omega]val,gval][[1]]block11[j][e1val,\[Omega]val,gval][[3]]-block11[i+j][e1val,\[Omega]val,gval][[3]]/2
},
{
block11[i][e1val,\[Omega]val,gval][[3]]block11[j][e1val,\[Omega]val,gval][[1]]-block11[i+j][e1val,\[Omega]val,gval][[3]]/2,
block11[i][e1val,\[Omega]val,gval][[3]]*block11[j][e1val,\[Omega]val,gval][[3]]
}
}
,
{i,1,K},{j,i,1,-2}(*{j,Mod[i,2],K,2}*)
],1](*/.{e->e1val,\[Omega]->\[Omega]val,g->gval}*)//Evaluate
]


blocksEven11[K_]:=Function[
{e1val,ekval,\[Omega]val,gval},
Flatten[Table[
{{block12[i][e1val,ekval,\[Omega]val,gval][[3]]*block12[j][ekval,e1val,\[Omega]val,gval][[3]]}}
,
{i,1,K},{j,i,1,-2}(*{j,Mod[i,2],K,2}*)
],1]//Evaluate
]


blocksOdd11[K_]:=Function[
{e1val,ekval,\[Omega]val,gval},
Flatten[Table[
{{block12[i][e1val,ekval,\[Omega]val,gval][[2]]*block12[j][ekval,e1val,\[Omega]val,gval][[2]]}}
,
{i,1,K},{j,i,1,-2}(*{j,Mod[i,2],K,2}*)
],1]//Evaluate
]


blocksPow11[K_]:=Flatten[Table[{i,j},{i,1,K},{j,i,1,-2}],1];


ClearAll[reduceBlocksSingle]
reduceBlocksSingle[cbinfo_]:=Module[
{
vConst,vEven,vOdd,exponent,rowReduced,
coArray,nCols,nRows,reducedRows,K=cbinfo["K"],
info
},
info={"mixed",K};
If[ValueQ[CBV["info"]]&&CBV["info"]===info,Return[True]];
vConst=blocksConst11[K][e1val,\[Omega]val,gval];

vEven=blocksEven11[K][e1val,e1val+x,\[Omega]val,gval];
vOdd=blocksOdd11[K][e1val,e1val+x,\[Omega]val,gval];
(*temp={vConst,vEven,vOdd}//Transpose;*)
exponent=Exponent[{vEven,vOdd},x]//Max;
(*vEven=PadRight[CoefficientList[#,x],exponent+1]&/@vEven;
vOdd=PadRight[CoefficientList[#,x],exponent+1]&/@vOdd;*)

(*temp2=Join[List/@vConst,vEven,vOdd,2];*)
coArray=Join[
CoefficientList[{1,t} . # . {1,t},t,3]&/@vConst,
CoefficientList[#,x,exponent+1]&/@Flatten[vEven],
CoefficientList[#,x,exponent+1]&/@Flatten[vOdd],
2];
nCols=Dimensions[coArray][[2]];
(*coArray=Join[coArray,IdentityMatrix[Length[coArray]],2];*)
(*Flatten[Position[#,Except[0,_?NumericQ],1,1]&/@RowReduce@Transpose@%]*)
reducedRows=Flatten[Position[#,Except[0,_?NumericQ],1,1]&/@RowReduce[
coArray/.Thread[{e1val,\[Omega]val,gval}->(*RandomSample[Prime/@Range[10],4]*)
(Prime/@{16,27,38})]//Transpose
] ];
(*nRows=DeleteCases[
rowReduced[[;;,;;nCols ]],
{0...}]//Length;
reducedRows=rowReduced[[;;nRows,nCols+1;;]];*)
nRows=Length[reducedRows];

vConst=vConst[[reducedRows]];
vEven=vEven[[reducedRows]];
vOdd=vOdd[[reducedRows]];
(*vConst=ArrayReshape[
coArray[[reducedRows,;;4]]//Expand,
{nRows,2,2}
];
vEven=coArray[[reducedRows,5;;exponent+5]];
vOdd=coArray[[reducedRows,exponent+6;;2exponent+6]];*)
(*Print[{vConst//Dimensions,vEven//Dimensions,vOdd//Dimensions}];
exponent//Print;*)
(*vEven=ArrayReshape[
vEven.Table[x^i,{i,0,exponent}]//Expand,
{nRows,1,1}
];
vOdd=ArrayReshape[
vOdd.Table[x^i,{i,0,exponent}]//Expand,
{nRows,1,1}
];*)

CBV["const"]=Function[
{\[Omega]val1,gval1,e1val1},
vConst/.Thread[
{\[Omega]val,gval,e1val}->
{\[Omega]val1,gval1,e1val1}
]//Evaluate
];
CBV["odd"]=Function[
{\[Omega]val1,gval1,e1val1},
vOdd/.Thread[
{\[Omega]val,gval,e1val}->
{\[Omega]val1,gval1,e1val1}
]//Evaluate
];
CBV["even"]=Function[
{\[Omega]val1,gval1,e1val1},
vEven/.Thread[
{\[Omega]val,gval,e1val}->
{\[Omega]val1,gval1,e1val1}
]//Evaluate
];
CBV["pdTab"]={{blocksPow11[K][[reducedRows]]}};
CBV["info"]=info;
(*vEven=PositiveMatrixWithPrefactor[
DampedRational[1,{},1/E,x],
{{vEven}}
];
vOdd=PositiveMatrixWithPrefactor[
DampedRational[1,{},1/E,x],
{{vOdd}}
];
Function[
{\[Omega]val1,gval1,e1val1,xSq11val},
SDP[vConst*0,vConst,{vEven,vOdd}]/.Thread[
{\[Omega]val,gval,e1val,xSq11}->
{\[Omega]val1,gval1,e1val1,xSq11val}
]//Evaluate
]*)
]


(* ::Section::Closed:: *)
(*Block forms - mixed*)


blocksMixedConst00[K_]:=Function[
{e0val,e1val,\[Omega]val,gval},
Flatten[Table[
{
{
block11[i][e0val,\[Omega]val,gval][[1]]block11[j][e0val,\[Omega]val,gval][[1]]-block11[i+j][e0val,\[Omega]val,gval][[1]],
block11[i][e0val,\[Omega]val,gval][[1]]block11[j][e0val,\[Omega]val,gval][[3]]-block11[i+j][e0val,\[Omega]val,gval][[3]]/2,
0,0
},
{
block11[i][e0val,\[Omega]val,gval][[3]]block11[j][e0val,\[Omega]val,gval][[1]]-block11[i+j][e0val,\[Omega]val,gval][[3]]/2,
block11[i][e0val,\[Omega]val,gval][[3]]*block11[j][e0val,\[Omega]val,gval][[3]],
0,0
},
{
0,0,0,0
},
{
0,0,0,
block12[i][e0val,e1val,\[Omega]val,gval][[2]]block12[j][e1val,e0val,\[Omega]val,gval][[2]]
}
}
,
{i,1,K},{j,i,1,-2}(*{j,Mod[i,2],K,2}*)
],1](*/.{e->e1val,\[Omega]->\[Omega]val,g->gval}*)//Evaluate
]


blocksMixedConst11[K_]:=Function[
{e0val,e1val,\[Omega]val,gval},
Flatten[Table[
{
{
block11[i][e1val,\[Omega]val,gval][[1]]block11[j][e1val,\[Omega]val,gval][[1]]-block11[i+j][e1val,\[Omega]val,gval][[1]],
0,
block11[i][e1val,\[Omega]val,gval][[1]]block11[j][e1val,\[Omega]val,gval][[3]]-block11[i+j][e1val,\[Omega]val,gval][[3]]/2,
0
},
{
0,0,0,0
},
{
block11[i][e1val,\[Omega]val,gval][[3]]block11[j][e1val,\[Omega]val,gval][[1]]-block11[i+j][e1val,\[Omega]val,gval][[3]]/2,
0,
block11[i][e1val,\[Omega]val,gval][[3]]*block11[j][e1val,\[Omega]val,gval][[3]],
0
},
{
0,0,0,
block12[i][e1val,e0val,\[Omega]val,gval][[2]]block12[j][e0val,e1val,\[Omega]val,gval][[2]]
}
}
,
{i,1,K},{j,i,1,-2}(*{j,Mod[i,2],K,2}*)
],1](*/.{e->e1val,\[Omega]->\[Omega]val,g->gval}*)//Evaluate
]


blocksMixedConst01[K_]:=Function[
{e0val,e1val,\[Omega]val,gval},
Flatten[Table[
Module[{
m14=block11[i][e0val,\[Omega]val,gval][[1]]block12[j][e0val,e1val,\[Omega]val,gval][[2]]
+block12[i][e0val,e1val,\[Omega]val,gval][[2]]block11[j][e1val,\[Omega]val,gval][[1]]
-block12[i+j][e0val,e1val,\[Omega]val,gval][[2]],
m24=block11[i][e0val,\[Omega]val,gval][[3]]block12[j][e0val,e1val,\[Omega]val,gval][[2]],
m34=block12[i][e0val,e1val,\[Omega]val,gval][[2]]block11[j][e1val,\[Omega]val,gval][[3]]},
1/2 ({
 {0, 0, 0, m14},
 {0, 0, 0, m24},
 {0, 0, 0, m34},
 {m14, m24, m34, 0}
})
],
{i,1,K},{j,i-1,1,-2}(*{j,Mod[i,2],K,2}*)
],1](*/.{e->e1val,\[Omega]->\[Omega]val,g->gval}*)//Evaluate
]


blocksMixedEven00[K_]:=Function[
{e0val,e1val,ekval,\[Omega]val,gval},
Flatten[Table[
{
{block12[i][e0val,ekval,\[Omega]val,gval][[3]]*block12[j][ekval,e0val,\[Omega]val,gval][[3]],0},
{0,0}
}
,
{i,1,K},{j,i,1,-2}(*{j,Mod[i,2],K,2}*)
],1]//Evaluate
]


blocksMixedEven11[K_]:=Function[
{e0val,e1val,ekval,\[Omega]val,gval},
Flatten[Table[
{
{0,0},
{0,block12[i][e1val,ekval,\[Omega]val,gval][[2]]*block12[j][ekval,e1val,\[Omega]val,gval][[2]]}
}
,
{i,1,K},{j,i,1,-2}(*{j,Mod[i,2],K,2}*)
],1]//Evaluate
]


blocksMixedEven01[K_]:=Function[
{e0val,e1val,ekval,\[Omega]val,gval},
Flatten[Table[
{
{0,1/2block12[i][e0val,ekval,\[Omega]val,gval][[3]]*block12[j][ekval,e1val,\[Omega]val,gval][[2]]},
{1/2block12[i][e0val,ekval,\[Omega]val,gval][[3]]*block12[j][ekval,e1val,\[Omega]val,gval][[2]],0}
}
,
{i,1,K},{j,i-1,1,-2}(*{j,Mod[i,2],K,2}*)
],1]//Evaluate
]


blocksMixedOdd00[K_]:=Function[
{e0val,e1val,ekval,\[Omega]val,gval},
Flatten[Table[
{
{block12[i][e0val,ekval,\[Omega]val,gval][[2]]*block12[j][ekval,e0val,\[Omega]val,gval][[2]],0},
{0,0}
}
,
{i,1,K},{j,i,1,-2}(*{j,Mod[i,2],K,2}*)
],1]//Evaluate
]


blocksMixedOdd11[K_]:=Function[
{e0val,e1val,ekval,\[Omega]val,gval},
Flatten[Table[
{
{0,0},
{0,block12[i][e1val,ekval,\[Omega]val,gval][[3]]*block12[j][ekval,e1val,\[Omega]val,gval][[3]]}
}
,
{i,1,K},{j,i,1,-2}(*{j,Mod[i,2],K,2}*)
],1]//Evaluate
]


blocksMixedOdd01[K_]:=Function[
{e0val,e1val,ekval,\[Omega]val,gval},
Flatten[Table[
{
{0,1/2block12[i][e0val,ekval,\[Omega]val,gval][[2]]*block12[j][ekval,e1val,\[Omega]val,gval][[3]]},
{1/2block12[i][e0val,ekval,\[Omega]val,gval][[2]]*block12[j][ekval,e1val,\[Omega]val,gval][[3]],0}
}
,
{i,1,K},{j,i-1,1,-2}(*{j,Mod[i,2],K,2}*)
],1]//Evaluate
]


blocksPowMixedEven[K_]:=Flatten[Table[{i,j},{i,1,K},{j,i,1,-2}],1];
blocksPowMixedOdd[K_]:=Flatten[Table[{i,j},{i,1,K},{j,i-1,1,-2}],1];


ClearAll[reduceBlocksMixed]
reduceBlocksMixed[cbinfo_]:=Module[
{
vConst,vEven,vOdd,rowReduced,
coArray,nCols,nRows,reducedRows,K=cbinfo["K"],
blocksByChns={
{blocksMixedConst00,blocksMixedEven00,blocksMixedOdd00},{blocksMixedConst11,blocksMixedEven11,blocksMixedOdd11},
{blocksMixedConst01,blocksMixedEven01,blocksMixedOdd01}
},
chns={"const","even","odd"},
exponentOfChns,dimOfChns,vBlocks,
pdsets,pdsets$reduced={},
info
},
info={"mixed",K};
If[ValueQ[CBV["info"]]&&CBV["info"]===info,Return[True]];
pdsets={blocksPowMixedEven[K],blocksPowMixedEven[K],blocksPowMixedOdd[K]};
vBlocks=MapThread[
Join[##]&,
Table[
vConst=blocksByChns[[i,1]][K][e0val,e1val,\[Omega]val,gval];
vEven=blocksByChns[[i,2]][K][e0val,e1val,x,\[Omega]val,gval];
vOdd=blocksByChns[[i,3]][K][e0val,e1val,x,\[Omega]val,gval];
vBlocks=<|
"const"->vConst,
"even"->vEven,
"odd"->vOdd
|>;
exponentOfChns=Max[Exponent[#,x],0]&/@vBlocks;
dimOfChns=Dimensions[#[[1]]][[1]]&/@vBlocks;
coArray=Join[
Sequence@@Table[
Join@@CoefficientList[
Table[Sequence@@Diagonal[#,j],{j,0,Length[#]-1}],
x,exponentOfChns[chn]+1
]&/@vBlocks[chn],
{chn,chns}
],2];
reducedRows=Flatten[Position[#,Except[0,_?NumericQ],1,1]&/@RowReduce[
coArray/.Thread[{e0val,e1val,\[Omega]val,gval}->(*RandomSample[Prime/@Range[10],4]*)
(Prime/@{12,16,27,38})]//Transpose
] ];
AppendTo[pdsets$reduced,pdsets[[i,reducedRows]] ];
vBlocks=#[[reducedRows]]&/@vBlocks;
vBlocks,
{i,3}
]];
Do[
CBV[chn]=Function[
{\[Omega]val1,gval1,e0val1,e1val1},
vBlocks[chn]/.Thread[
{\[Omega]val,gval,e0val,e1val}->
{\[Omega]val1,gval1,e0val1,e1val1}
]//Evaluate
],
{chn,chns}
];
CBV["pdTab"]=List/@pdsets$reduced;
CBV["info"]=info
]


(* ::Section:: *)
(*Define SDP*)


(* ::Subsection::Closed:: *)
(*mixed matrix element navigator function (sdp and dsdp)*)


ClearAll[sdpOptMixed]
sdpOptMixed[K_]:=sdpOptMixed[K]=Module[
{
matOPE,matsDOPE,exponent,rowReduced,
coArray,nCols,nRows,reducedRows,vNorm,
sdp,dsdp,obj,dObj,norm,dNorm,polyMats,pwmps
},
reduceBlocksMixed[<|"K"->K|>];
obj={1,cXSq00,cXSq11,cX01} . # . {1,cXSq00,cXSq11,cX01}&/@CBV["const"][\[Omega]val,gval,e0val,e1val];
matOPE=KroneckerProduct[{1,cXSq00,cXSq11,cX01},{1,cXSq00,cXSq11,cX01}];
matsDOPE=Transpose[D[matOPE,{{cXSq00,cXSq11,cX01}}],{2,3,1}];
dObj=Table[
Tr[# . mat]&/@CBV["const"][\[Omega]val,gval,e0val,e1val],
{mat,matsDOPE}
];
norm=(
(Module[
{cList=CoefficientList[Tr[#],x]},
Table[integralX[pow,{0,1}],{pow,0,Length[cList]-1}] . cList
]&/@(CBV["odd"][\[Omega]val,gval,e0val,e1val]/.x->e1val+x) )
+(Module[
{cList=CoefficientList[Tr[#],x]},
Table[integralX[pow,{0,1}],{pow,0,Length[cList]-1}] . cList
]&/@(CBV["even"][\[Omega]val,gval,e0val,e1val]/.x->e1val+x) )
);

dNorm={0*norm,0*norm,0*norm};
pwmps={
PositiveMatrixWithPrefactor[
DampedRational[1,{},1/E,x],
Transpose[CBV["even"][\[Omega]val,gval,e0val,e1val],{3,1,2}]/.x->e1val+x
],
PositiveMatrixWithPrefactor[
DampedRational[1,{},1/E,x],
Transpose[CBV["odd"][\[Omega]val,gval,e0val,e1val],{3,1,2}]/.x->e1val+x
]
};
polyMats=Transpose[{
{
0*Transpose[CBV["even"][\[Omega]val,gval,e0val,e1val],{3,1,2}],
0*Transpose[CBV["even"][\[Omega]val,gval,e0val,e1val],{3,1,2}],
0*Transpose[CBV["even"][\[Omega]val,gval,e0val,e1val],{3,1,2}]
},
{
0*Transpose[CBV["odd"][\[Omega]val,gval,e0val,e1val],{3,1,2}],
0*Transpose[CBV["odd"][\[Omega]val,gval,e0val,e1val],{3,1,2}],
0*Transpose[CBV["odd"][\[Omega]val,gval,e0val,e1val],{3,1,2}]
}
}];


sdp=Function[
{\[Omega]val1,gval1,e0val1,e1val1,cXSq00val,cXSq11val,cX01val},
SDP[
obj,norm,pwmps
]/.Thread[
{\[Omega]val,gval,e0val,e1val,cXSq00,cXSq11,cX01}->
{\[Omega]val1,gval1,e0val1,e1val1,cXSq00val,cXSq11val,cX01val}
]//Evaluate
];
(*dsdp=Function[
{\[Omega]val1,gval1,e0val1,e1val1,cXSq00val,cXSq11val,cX01val},
{
{obj,dObj},
{norm,dNorm},
polyMats
}/.Thread[
{\[Omega]val,gval,e0val,e1val,cXSq00,cXSq11,cX01}->
{\[Omega]val1,gval1,e0val1,e1val1,cXSq00val,cXSq11val,cX01val}
]//Evaluate
];*)
dsdp=Function[
{\[Omega]val1,gval1,e0val1,e1val1,cXSq00val,cXSq11val,cX01val},
MapThread[
Function[
{dObj1,dNorm1,polyMats1},
dSDP[{obj,dObj1},{norm,dNorm1},
MapThread[
PositiveMatrixWithPrefactor[
#1[[1]],
Transpose[{#1[[2]],#2},{3,1,2,4}]
]&,{pwmps,polyMats1}
] ]
],
{dObj,dNorm,polyMats}
]/.Thread[
{\[Omega]val,gval,e0val,e1val,cXSq00,cXSq11,cX01}->
{\[Omega]val1,gval1,e0val1,e1val1,cXSq00val,cXSq11val,cX01val}
]//Evaluate
];
<|"sdp"->sdp,"dsdp"->dsdp|>

]


(* ::Subsection::Closed:: *)
(*mixed matrix element navigator function with all parameter's gradients (sdp and dsdp)*)


ClearAll[integralX];
integralX[pow_,{x0_,x1_}]:=integralX[pow,{x0,x1}]=Gamma[1+pow,x0];


ClearAll[sdpOptMixedAllGrads]
sdpOptMixedAllGrads[K_]:=sdpOptMixedAllGrads[K]=Module[
{
matOPE,matsDOPE,exponent,rowReduced,
coArray,nCols,nRows,reducedRows,vNorm,
sdp,dsdp,obj,dObj,norm,dNorm,polyMats,pwmps,
dVConst,dVEven,dVOdd
},
reduceBlocksMixed[<|"K"->K|>];
dVConst=Transpose[D[
CBV["const"][\[Omega]val,gval,e0val,e1val],
{{e0val,e1val}}
],{2,3,4,1}];
dVEven=Transpose[D[
CBV["even"][\[Omega]val,gval,e0val,e1val]/.x->e1val+x,
{{e0val,e1val}}
],{2,3,4,1}];
dVOdd=Transpose[D[
CBV["odd"][\[Omega]val,gval,e0val,e1val]/.x->e1val+x,
{{e0val,e1val}}
],{2,3,4,1}];
obj={1,cXSq00,cXSq11,cX01} . # . {1,cXSq00,cXSq11,cX01}&/@CBV["const"][\[Omega]val,gval,e0val,e1val];
matOPE=KroneckerProduct[{1,cXSq00,cXSq11,cX01},{1,cXSq00,cXSq11,cX01}];
matsDOPE=Transpose[D[matOPE,{{cXSq00,cXSq11,cX01}}],{2,3,1}];
dObj=Join[
Map[
{1,cXSq00,cXSq11,cX01} . # . {1,cXSq00,cXSq11,cX01}&,
dVConst,{2}
],
Table[
Tr[# . mat]&/@CBV["const"][\[Omega]val,gval,e0val,e1val],
{mat,matsDOPE}
]
];
norm=(
(Module[
{cList=CoefficientList[Tr[#],x]},
Table[integralX[pow,{0,1}],{pow,0,Length[cList]-1}] . cList
]&/@(CBV["odd"][\[Omega]val,gval,e0val,e1val]/.x->e1val+x) )
+(Module[
{cList=CoefficientList[Tr[#],x]},
Table[integralX[pow,{0,1}],{pow,0,Length[cList]-1}] . cList
]&/@(CBV["even"][\[Omega]val,gval,e0val,e1val]/.x->e1val+x) )
);

dNorm=Join[
Transpose[ D[norm,{{e0val,e1val}}] ],
{0*norm,0*norm,0*norm}
];
pwmps={
PositiveMatrixWithPrefactor[
DampedRational[1,{},1/E,x],
Transpose[CBV["even"][\[Omega]val,gval,e0val,e1val],{3,1,2}]/.x->e1val+x
],
PositiveMatrixWithPrefactor[
DampedRational[1,{},1/E,x],
Transpose[CBV["odd"][\[Omega]val,gval,e0val,e1val],{3,1,2}]/.x->e1val+x
]
};
polyMats=Transpose[{
Join[
Transpose[#,{3,1,2}]&/@dVEven,
{
0*Transpose[CBV["even"][\[Omega]val,gval,e0val,e1val],{3,1,2}],
0*Transpose[CBV["even"][\[Omega]val,gval,e0val,e1val],{3,1,2}],
0*Transpose[CBV["even"][\[Omega]val,gval,e0val,e1val],{3,1,2}]
}
],
Join[
Transpose[#,{3,1,2}]&/@dVOdd,
{
0*Transpose[CBV["odd"][\[Omega]val,gval,e0val,e1val],{3,1,2}],
0*Transpose[CBV["odd"][\[Omega]val,gval,e0val,e1val],{3,1,2}],
0*Transpose[CBV["odd"][\[Omega]val,gval,e0val,e1val],{3,1,2}]
}
]
} ];


sdp=Function[
{\[Omega]val1,gval1,e0val1,e1val1,cXSq00val,cXSq11val,cX01val},
SDP[
obj,norm,pwmps
]/.Thread[
{\[Omega]val,gval,e0val,e1val,cXSq00,cXSq11,cX01}->
{\[Omega]val1,gval1,e0val1,e1val1,cXSq00val,cXSq11val,cX01val}
]//Evaluate
];
(*Dimensions/@{dObj,dNorm,polyMats}//Print;Abort[];*)
dsdp=Function[
{\[Omega]val1,gval1,e0val1,e1val1,cXSq00val,cXSq11val,cX01val},
MapThread[
Function[
{dObj1,dNorm1,polyMats1},
dSDP[{obj,dObj1},{norm,dNorm1},
MapThread[
PositiveMatrixWithPrefactor[
#1[[1]],
Transpose[{#1[[2]],#2},{3,1,2,4}]
]&,{pwmps,polyMats1}
] ]
],
{dObj,dNorm,polyMats}
]/.Thread[
{\[Omega]val,gval,e0val,e1val,cXSq00,cXSq11,cX01}->
{\[Omega]val1,gval1,e0val1,e1val1,cXSq00val,cXSq11val,cX01val}
]//Evaluate
];
<|"sdp"->sdp,"dsdp"->dsdp|>

]
