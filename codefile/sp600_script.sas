/**************************************************************************************************************************************************/
                                               /** SCRIPT S&P600 **/
/**************************************************************************************************************************************************/


/** Database import : S&P600 assets **/
proc import datafile="C:\Lucas_M1\M1_Eco_Stats\M1_S1\SAS_DP\Memoire\PTF_2008\final\S&P600_6.xlsx"
	out=ptf600
	dbms=xlsx
	replace;
	getname=yes;
run;

/**************************************************************************************************************************************************/

/** Data standardization: GARCH Studentized **/
%macro garch (table, resultsp, var=, condvar=); 
 
	proc autoreg data= ptf600;
		model &var.= / garch=(q=1, p=1) dist = t; 
		output out = &resultsp. r=epsilon cev=&condvar. p=prevision alphacli=0.05
		lcl = lower ucl = upper;
	 
	proc iml;
	 
		varNames = {&var. &condvar.};
		use &resultsp.; read all var varNames into X; close &resultsp.;
		 
		do i=1 to nrow(X);
			Z = Z // X[i,1];
			A = X[:];
			B = B // X[i,2];
		end;

		varNames = {"Date"};
		use ptf600; read all var varNames into Date; close ptf600;

		free norm;
		norm = (Z-A)/ sqrt(B);
		M =  B || Z || norm || Date;
	 
	Create &table. var {Date norm};
	append;
	close &table.;
	 
	proc means data=&table.;
	var norm;
	run; 
 
%mend garch;

%garch(tab1,resultsp1,   var=AFG,  condvar=condvar1);
%garch(tab2,resultsp2,   var=AMED, condvar=condvar2);
%garch(tab3,resultsp3,   var=ANF,  condvar=condvar3);
%garch(tab4,resultsp4,   var=ASH,  condvar=condvar4);
%garch(tab5,resultsp5,   var=CRS,  condvar=condvar5);
%garch(tab6,resultsp6,   var=DIN,  condvar=condvar6);
%garch(tab7,resultsp7,   var=GHC,  condvar=condvar7);
%garch(tab8,resultsp8,   var=IDA,  condvar=condvar8);
%garch(tab9,resultsp9,   var=JLL,  condvar=condvar9);
%garch(tab10,resultsp10, var=LAD,  condvar=condvar10);
%garch(tab11,resultsp11, var=LDL,  condvar=condvar11);
%garch(tab12,resultsp12, var=MGPI, condvar=condvar12);
%garch(tab13,resultsp13, var=PTC,  condvar=condvar13);
%garch(tab14,resultsp14, var=UTHR, condvar=condvar14);
%garch(tab15,resultsp15, var=WDFC, condvar=condvar15);

/** Construction of the final portfolio with all data centered and reduced Studentized **/
data ptf_final600;
	merge tab1 (rename=(NORM=AFG)) tab2 (rename=(NORM=AMED)) tab3 (rename=(NORM=ANF)) tab4 (rename=(NORM=ASH)) tab5 (rename=(NORM=CRS))
	tab6 (rename=(NORM=DIN)) tab7 (rename=(NORM=GHC)) tab8 (rename=(NORM=IDA)) tab9 (rename=(NORM=JLL)) tab10 (rename=(NORM=LAD))
	tab11 (rename=(NORM=LDL)) tab12 (rename=(NORM=MGPI)) tab13 (rename=(NORM=PTC)) tab14 (rename=(NORM=UTHR)) tab15 (rename=(NORM=WDFC));
	by Date;
run;

/**************************************************************************************************************************************************/

/** Printing Heatmaps **/
%Macro macro_heatmap(tab, start, end);
	/* Number of trading days in a month */
	%let date1=1;
	%let date2=22;
	
	%do n=&start. %to &end.; /* You can choose the rolling time window */

	    Data &tab.;
	    set ptf_final600(firstobs=&date1. obs=&date2.);
	    run;

	    proc iml;

	    /* Covariance */
	    varNames = {"AFG" "AMED" "ANF" "ASH" "CRS" "DIN" "GHC" "IDA" "JLL" "LAD" "LDL" "MGPI" "PTC" "UTHR" "WDFC"};
	    use &tab.; read all var varNames into X; close &tab.;

		call HeatmapCont(corr(X)) xvalues=varNames yvalues=varNames 
	        colorramp="threeColor" range={-1.01 1.01} 
			title='Correlation Matrix ';
		
		/* Recovering the lambda max */
		lambda=(&date1.||&date2.||max(eigval(corr(X))));
		print lambda;

	 	%let date1=%eval(&date1+1);
		%let date2=%eval(&date2+1);

	%end;
	run;

%mend macro_heatmap;

%macro_heatmap(tab, 1, 981);

/**************************************************************************************************************************************************/

/** Printing of the empirical distribution of eigenvalues **/
proc iml;

	/* Correlations and rolling windows: taken from a normal law. In practice, if you are doing RMT, normalize each variable E[x]=0 et V[x]=1 */  
	varNames = {"AFG" "AMED" "ANF" "ASH" "CRS" "DIN" "GHC" "IDA" "JLL" "LAD" "LDL" "MGPI" "PTC" "UTHR" "WDFC"};
	use ptf_final600; read all var varNames into X; close ptf_final600;

	free lambda;
	wind=6*22;/* Rolling time window */ 
	do t=1 to nrow(X)-wind  ;
		X1=X[t:t+wind,];/* Matrix partition selection */
		C=corr(X1);
		do i=1 to ncol(C);
			corrcoef600 = corrcoef600 // C[,i];
		end;
		lambda=lambda//(t||t+wind||max(eigval(corr(X1)))); 
		SP600 = SP600 // max(eigval(corr(X1)));
		lam = lam // eigval(corr(X1));
	end;

	varNames = {"Date"};
	use ptf600; read all var varNames into Date; close ptf600;

	mattrib Lambda[colname={'Start','End','Lambda max'}];

	call series(Lambda[,2],Lambda[,3]);
	call histogram (lam);
	call QNTL(q,lam,0.95);
	print q;
	call histogram(SP600);
	call QNTL(q,SP600,0.95);
	print q;

	Create Coeffcorr600 var {corrcoef600};
		append;
		close;
	Create SP_600 var {Date SP600};
		append;
		close SP_600;

/**************************************************************************************************************************************************/

/** Impression des distributions des coefficients de corrélation **/
data correlation600; set Coeffcorr600;
	if CORRCOEF600 = 1 then delete;
run;

proc iml;
	varNames = {"CORRCOEF"};
	use correlation; read all var varNames into correlation; close correlation;

	call histogram(correlation) density={'kernel'}
		scale="count";

/**************************************************************************************************************************************************/

