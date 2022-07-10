/**************************************************************************************************************************************************/
                                               /** S&P400 SCRIPT **/
/**************************************************************************************************************************************************/


/** Database import : S&P400 assets **/
proc import datafile="C:\Lucas_M1\M1_Eco_Stats\M1_S1\SAS_DP\Memoire\PTF_2008\final\S&P400_6.xlsx"
	out=ptf400
	dbms=xlsx
	replace;
	getname=yes;
run;

/**************************************************************************************************************************************************/

/** Data standardization: GARCH Studentized **/
%macro garch (table, resultsp, var=, condvar=); 
 
	proc autoreg data= ptf400;
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
		use ptf400; read all var varNames into Date; close ptf400;

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

%garch(tab1,resultsp1,   var=ACIW, condvar=condvar1);
%garch(tab2,resultsp2,   var=ADS,  condvar=condvar2);
%garch(tab3,resultsp3,   var=BOH,  condvar=condvar3);
%garch(tab4,resultsp4,   var=CBT,  condvar=condvar4);
%garch(tab5,resultsp5,   var=CLGX, condvar=condvar5);
%garch(tab6,resultsp6,   var=CREE, condvar=condvar6);
%garch(tab7,resultsp7,   var=FL,   condvar=condvar7);
%garch(tab8,resultsp8,   var=HOG,  condvar=condvar8);
%garch(tab9,resultsp9,   var=HXL,  condvar=condvar9);
%garch(tab10,resultsp10, var=ICUI, condvar=condvar10);
%garch(tab11,resultsp11, var=KBH,  condvar=condvar11);
%garch(tab12,resultsp12, var=OSK,  condvar=condvar12);
%garch(tab13,resultsp13, var=PNM,  condvar=condvar13);
%garch(tab14,resultsp14, var=RS,   condvar=condvar14);
%garch(tab15,resultsp15, var=PZZA, condvar=condvar15);

/** Construction of the final portfolio with all data centered and reduced Studentized **/
data ptf_final400;
	merge tab1 (rename=(NORM=ACIW)) tab2 (rename=(NORM=ADS)) tab3 (rename=(NORM=BOH)) tab4 (rename=(NORM=CBT)) tab5 (rename=(NORM=CLGX))
	tab6 (rename=(NORM=CREE)) tab7 (rename=(NORM=FL)) tab8 (rename=(NORM=HOG)) tab9 (rename=(NORM=HXL)) tab10 (rename=(NORM=ICUI))
	tab11 (rename=(NORM=KBH)) tab12 (rename=(NORM=OSK)) tab13 (rename=(NORM=PNM)) tab14 (rename=(NORM=RS)) tab15 (rename=(NORM=PZZA));
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
		    set ptf_final400(firstobs=&date1. obs=&date2.);
	    run;

	    proc iml;

	    /* Covariance */
	    varNames = {"ACIW" "ADS" "BOH" "CBT" "CLGX" "CREE" "FL" "HOG" "HXL" "ICUI" "KBH" "OSK" "PNM" "RS" "PZZA"};
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
	varNames = {"ACIW" "ADS" "BOH" "CBT" "CLGX" "CREE" "FL" "HOG" "HXL" "ICUI" "KBH" "OSK" "PNM" "RS" "PZZA"};
	use ptf_final400; read all var varNames into X; close ptf_final400;

	free lambda;
	wind=6*22;/* Rolling time window */ 
	do t=1 to nrow(X)-wind ;
		X1=X[t:t+wind,];/* Matrix partition selection */
		C=corr(X1);
		do i=1 to ncol(C);
			corrcoef400 = corrcoef400 // C[,i];
		end;
		lambda=lambda//(t||t+wind||max(eigval(corr(X1)))); 
		SP400 = SP400 // max(eigval(corr(X1)));
		lam = lam // eigval(corr(X1));
	end;

	varNames = {"Date"};
	use ptf400; read all var varNames into Date; close ptf400;

	mattrib Lambda[colname={'Start','End','Lambda max'}];

	call series(Lambda[,2],Lambda[,3]);

	call histogram (lam);
	call QNTL(q,lam,0.95);
	print q;
	call histogram(SP400);
	call QNTL(q,SP400,0.95);
	print q;

	Create Coeffcorr400 var {corrcoef400};
		append;
		close;
	Create SP_400 var {Date SP400};
		append;
		close SP_400;

/**************************************************************************************************************************************************/

/** Printing of empirical distributions of correlation coefficients **/
data correlation400; set Coeffcorr400;
	if CORRCOEF400 = 1 then delete;
run;

proc iml;
	varNames = {"CORRCOEF"};
	use correlation; read all var varNames into correlation; close correlation;

	call histogram(correlation) density={'kernel'}
		scale="count";

/**************************************************************************************************************************************************/




