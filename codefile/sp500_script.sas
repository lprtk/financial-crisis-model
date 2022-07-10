/**************************************************************************************************************************************************/
                                               /** S&P500 SCRIPT **/
/**************************************************************************************************************************************************/


/** Database import : S&P500 assets **/
proc import datafile="C:\Lucas_M1\M1_Eco_Stats\M1_S1\SAS_DP\Memoire\PTF_2008\final\S&P500_6.xlsx"
	out=ptf500
	dbms=xlsx
	replace;
	getname=yes;
run;


/**************************************************************************************************************************************************/

/** Data standardization: GARCH Studentized **/
%macro garch (table, resultsp, var=, condvar=); 
 
	proc autoreg data= ptf500;
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
		use ptf500; read all var varNames into Date; close ptf500;
		 
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

%garch(tab1,resultsp1,   var=AAPL, condvar=condvar1);
%garch(tab2,resultsp2,   var=AMGN, condvar=condvar2);
%garch(tab3,resultsp3,   var=BA,   condvar=condvar3);
%garch(tab4,resultsp4,   var=CAT,  condvar=condvar4);
%garch(tab5,resultsp5,   var=DIS,  condvar=condvar5);
%garch(tab6,resultsp6,   var=GS,   condvar=condvar6);
%garch(tab7,resultsp7,   var=HD,   condvar=condvar7);
%garch(tab8,resultsp8,   var=KO,   condvar=condvar8);
%garch(tab9,resultsp9,   var=MCD,  condvar=condvar9);
%garch(tab10,resultsp10, var=MMM,  condvar=condvar10);
%garch(tab11,resultsp11, var=MSFT, condvar=condvar11);
%garch(tab12,resultsp12, var=TRV,  condvar=condvar12);
%garch(tab13,resultsp13, var=UNH,  condvar=condvar13);
%garch(tab14,resultsp14, var=VZ,   condvar=condvar14);
%garch(tab15,resultsp15, var=WMT,  condvar=condvar15);

/** Construction of the final portfolio with all data centered and reduced Studentized **/
data ptf_final500;
	merge tab1 (rename=(NORM=AAPL)) tab2 (rename=(NORM=AMGN)) tab3 (rename=(NORM=BA)) tab4 (rename=(NORM=CAT)) tab5 (rename=(NORM=DIS))
	tab6 (rename=(NORM=GS)) tab7 (rename=(NORM=HD)) tab8 (rename=(NORM=KO)) tab9 (rename=(NORM=MCD)) tab10 (rename=(NORM=MMM))
	tab11 (rename=(NORM=MSFT)) tab12 (rename=(NORM=TRV)) tab13 (rename=(NORM=UNH)) tab14 (rename=(NORM=VZ)) tab15 (rename=(NORM=WMT));
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
	    set ptf_final500(firstobs=&date1. obs=&date2.);
	    run;

	    proc iml;

	    /* Covariance */
	    varNames = {"AAPL" "AMGN" "BA" "CAT" "DIS" "GS" "HD" "KO" "MCD" "MMM" "MSFT" "TRV" "UNH" "VZ" "WMT"};
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
	varNames = {"AAPL" "AMGN" "BA" "CAT" "DIS" "GS" "HD" "KO" "MCD" "MMM" "MSFT" "TRV" "UNH" "VZ" "WMT"};
	use ptf_final500; read all var varNames into X; close ptf_final500;

	free lambda;
	wind=6*22;/* Rolling time window */ 
	do t=1 to nrow(X)-wind;
		X1=X[t:t+wind,];/* Matrix partition selection */
		C=corr(X1);
		do i=1 to ncol(C);
			corrcoef500 = corrcoef500 // C[,i];
		end;
		lambda=lambda //(t||t+wind||max(eigval(corr(X1)))); 
		SP500 = SP500 // max(eigval(corr(X1)));
		lam = lam // eigval(corr(X1));
	end;

	varNames = {"Date"};
	use ptf500; read all var varNames into Date; close ptf500;

	mattrib Lambda[colname={'Start','End','Lambda max'}];

	call series(Lambda[,2],Lambda[,3]);
	call histogram(corrcoef) density={'kernel'}
		scale="count";
	call histogram (lam);
	call QNTL(q,lam,0.95);
	print q;
	call histogram(SP500);
	call QNTL(q,SP500,0.95);
	print q;

	Create Coeffcorr500 var {corrcoef500};
		append;
		close;
	Create SP_500 var {Date SP500};
		append;
		close SP_500;

/**************************************************************************************************************************************************/

/** Printing of empirical distributions of correlation coefficients **/
data correlation500; set Coeffcorr500;
	if CORRCOEF500 = 1 then delete;
run;

proc iml;
	varNames = {"CORRCOEF"};
	use correlation; read all var varNames into correlation; close correlation;

	call histogram(correlation) density={'kernel'}
		scale="count";

/**************************************************************************************************************************************************/
