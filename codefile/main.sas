/**************************************************************************************************************************************************/
                                               /** MAIN SCRIPT **/
/**************************************************************************************************************************************************/

/** Database import : S&P500 index price **/
proc import datafile="C:\Lucas_M1\M1_Eco_Stats\M1_S1\SAS_DP\Memoire\PTF_2008\final\SP500_return.xlsx"
	out=indiceSP500
	dbms=xlsx
	replace;
	getname=yes;
run;

/** Database import : VIX Index **/
proc import datafile="C:\Lucas_M1\M1_Eco_Stats\M1_S1\SAS_DP\Memoire\PTF_2008\final\VIX.xlsx"
	out=vix
	dbms=xlsx
	replace;
	getname=yes;
run;

/** Database import : CISS US index **/
proc import datafile="C:\Lucas_M1\M1_Eco_Stats\M1_S1\SAS_DP\Memoire\PTF_2008\final\CISS.xlsx"
	out=CISS_US
	dbms=xlsx
	replace;
	getname=yes;
run;



/** Concatenation of the three portfolios to draw a stacked graph. The goal is to transform the dates into a good format **/
data graph;
	merge SP_500 SP_400 SP_600 VIX indiceSP500;
	by Date;
run;
data graph2;
	merge graph indiceSP500;
	by Date2;
run;

/** Modify the number of observations according to the rolling window used **/
data graph3; 
	set graph2 (firstobs=3686 obs=4147); 
run;


/** Example: procedure to make the graph by hand **/
/*** Remember to change the title for the selected rolling window ***/
proc template;
define statgraph sgdesign;
dynamic _DATE _SP500A2 _DATE4 _SP400A _DATE6 _SP600A _DATE2 _SP400A2 _DATE3 _SP500A _DATE5 _SP600A2;
begingraph / designwidth=777 designheight=540;
   layout lattice / rowdatarange=data columndatarange=data rows=2 columns=2 rowgutter=10 columngutter=10 rowweights=(1.0 1.0) columnweights=(1.0 1.0);
      layout overlay / yaxisopts=( display=(TICKS TICKVALUES LINE LABEL ) label=('Eigenvalues'));
         seriesplot x=_DATE y=_SP500A2 / name='series' connectorder=xaxis lineattrs=(color=CXFF0000 );
         discretelegend 'series' 'series4' 'series6' / opaque=false border=true halign=right valign=top displayclipped=true across=1 order=rowmajor location=inside autoalign=(topright topleft bottomright bottomleft top bottom right left);
         seriesplot x=_DATE4 y=_SP400A / name='series4' connectorder=xaxis lineattrs=(color=CX00FF00 );
         seriesplot x=_DATE6 y=_SP600A / name='series6' connectorder=xaxis lineattrs=(color=CX0000FF );
      endlayout;
      layout overlay / yaxisopts=( label=('Eigenvalues ( S&P400)'));
         seriesplot x=_DATE2 y=_SP400A2 / name='series2' connectorder=xaxis lineattrs=(color=CX00FF00 );
      endlayout;
      layout overlay / yaxisopts=( display=(TICKS TICKVALUES LINE LABEL ) label=('Eigenvalues (S&P 500)'));
         seriesplot x=_DATE3 y=_SP500A / name='series3' connectorder=xaxis lineattrs=(color=CXFF0000 );
      endlayout;
      layout overlay / yaxisopts=( display=(TICKS TICKVALUES LINE LABEL ) label=('Eigenvalues (S&P 600)'));
         seriesplot x=_DATE5 y=_SP600A2 / name='series5' connectorder=xaxis lineattrs=(color=CX0000FF );
      endlayout;
   endlayout;
endgraph;
end;
run;
proc sgrender data=WORK.GRAPH2 template=sgdesign;
	dynamic _DATE="DATE2" _SP500A2="SP500" _DATE4="DATE2" _SP400A="SP400" _DATE6="DATE2" _SP600A="SP600" _DATE2="DATE2" _SP400A2="SP400" _DATE3="DATE2" _SP500A="SP500" _DATE5="DATE2" _SP600A2="SP600";
run;


/**************************************************************************************************************************************************/
                                                /** DECISION CRITERIA **/
/**************************************************************************************************************************************************/

/** Distribution of Marchenko Pastur **/
proc iml;

	/* Portfolio Size */
	N=15;
	T=22;
	/* Ratio of matrix dimensions */
	c=N/T;
	/* Sample */
	x=normal(j(N,T,0)); /* Normal distribution */
	free lambda ;
	do i=1 to ncol(x);
		xvect=xvect//x[,i];
	end;
	s=std(xvect);


	/* Spectral matrix */
	r=x*x`/T;

	/* Eigenvalues */
	call eigen(val, rvec, r) vecl="lvec";
	l=val;

	/* Boundaries */
	a=(s**2)*(1-sqrt(c))**2;
	b=(s**2)*(1+sqrt(c))**2;

	start linspace(a, b, numPts=50);
		n = floor(numPts);               /* if n is not an integer, truncate */
		if n < 1 then return( {} );      /* return empty matrix */
		else if n=1 then return( b );    /* return upper endpoint */
		return( do(a, b, (b-a)/(n-1)) ); /* return n equally spaced points */
	finish;
	lambda=linspace(a,b);
	print (lambda);

	/* Theoretical pdf */
	do i=1 to ncol(lambda);	
		lambdat=lambda[,i];	
		ft=ft//(1/(2*3.14*lambdat*c*s**(2)))*sqrt((b-lambdat)*(lambdat-a));
	end;

	title "Marchenko Pastur Distribution";
	call histogram(l) density={'kernel'}
		scale="count";
	call series(lambda,ft);
	call QNTL(q,lambdat,0.95);
	print q;

/**************************************************************************************************************************************************/

/** Random Matrix Theory : Normal and Student multivariate law **/
 
/** Normal multivariate law **/
proc iml;

	free lambda;
	do rep=1 to 5000;
		N=15;
		wind=6*22;
		Mean=j(1,N,0);
		R=j(wind,N,0);
		call randgen(R, 'NORMAL', 0.5, 1);
		Cov=cov(R);
		call randseed(4321);               
		X=RandNormal(wind, Mean, Cov);
		lambda=lambda//eigval(corr(X));
		C=corr(X);
		do i=1 to ncol(C);
			corrcoef = corrcoef // C[,i];
		end;
	end;

	title "Random Matrix Theory (Normal)";
	call histogram(lambda);
	call QNTL(q,lambda,0.95);
	print q;
	call histogram(corrcoef) density={'kernel'}
		scale="count";

	mx=mean(X[,1]);
	stdX=std(X);
	print stdX;

	Create lambda var {lambda};
		append;
		close lambda;

/** Student multivariate law **/
proc iml;

	free lambda;
	do rep=1 to 5000;
		N=15;
		wind=6*22;
		Mean=j(1,N,0);
		R=j(wind,N,0);
		call randgen(R, 'T',7);
		Cov=cov(R);
		call randseed(4321);               
		X=RandMVT(wind,7, Mean, Cov);
		lambda=lambda//eigval(corr(X));
		C=corr(X);
		do i=1 to ncol(C);
			corrcoef = corrcoef // C[,i];
		end; 
	end;

	title "Random Matrix Theory (Student)";
	call histogram(lambda);
	call QNTL(q,lambda,0.95);
	print q;
	call histogram(corrcoef) density={'kernel'}
		scale="count";

	mx=mean(X[,1]);
	stdX=std(X);
	print stdX;

	Create lambda var {lambda};
		append;
		close lambda;

/**************************************************************************************************************************************************/
