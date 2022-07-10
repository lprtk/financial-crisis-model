/**************************************************************************************************************************************************/
                                               /** S&P456 SCRIPT **/
/**************************************************************************************************************************************************/


/** Database import : S&P500 400 and 600 assets **/
proc import datafile="C:\Lucas_M1\M1_Eco_Stats\M1_S1\SAS_DP\Memoire\PTF_2008\S&P456.xlsx"
	out=ptf456
	dbms=xlsx
	replace;
	getname=yes;
run;

/**************************************************************************************************************************************************/

/** Data standardization: GARCH Studentized **/
%macro garch (table, resultsp, var=, condvar=); 
 
	proc autoreg data= ptf456;
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
		use ptf456; read all var varNames into Date; close ptf456;

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

%garch(tab1,resultsp1,   var=ACIW,  condvar=condvar1);
%garch(tab2,resultsp2,   var=ADS,   condvar=condvar2);
%garch(tab3,resultsp3,   var=BOH,   condvar=condvar3);
%garch(tab4,resultsp4,   var=CBT,   condvar=condvar4);
%garch(tab5,resultsp5,   var=CLGX,  condvar=condvar5);
%garch(tab6,resultsp6,   var=CREE,  condvar=condvar6);
%garch(tab7,resultsp7,   var=FL,    condvar=condvar7);
%garch(tab8,resultsp8,   var=HOG,   condvar=condvar8);
%garch(tab9,resultsp9,   var=HXL,   condvar=condvar9);
%garch(tab10,resultsp10, var=ICUI,  condvar=condvar10);
%garch(tab11,resultsp11, var=KBH,   condvar=condvar11);
%garch(tab12,resultsp12, var=OSK,   condvar=condvar12);
%garch(tab13,resultsp13, var=PNM,   condvar=condvar13);
%garch(tab14,resultsp14, var=RS,    condvar=condvar14);
%garch(tab15,resultsp15, var=PZZA,  condvar=condvar15);

%garch(tab16,resultsp16, var=AAPL,  condvar=condvar16);
%garch(tab17,resultsp17, var=AMGN,  condvar=condvar17);
%garch(tab18,resultsp18, var=BA,    condvar=condvar18);
%garch(tab19,resultsp19, var=CAT,   condvar=condvar19);
%garch(tab20,resultsp20, var=DIS,   condvar=condvar20);
%garch(tab21,resultsp21, var=GS,    condvar=condvar21);
%garch(tab22,resultsp22, var=HD,    condvar=condvar22);
%garch(tab23,resultsp23, var=KO,    condvar=condvar23);
%garch(tab24,resultsp24, var=MCD,   condvar=condvar24);
%garch(tab25,resultsp25, var=MMM,   condvar=condvar25);
%garch(tab26,resultsp26, var=MSFT,  condvar=condvar26);
%garch(tab27,resultsp27, var=TRV,   condvar=condvar27);
%garch(tab28,resultsp28, var=UNH,   condvar=condvar28);
%garch(tab29,resultsp29, var=VZ,    condvar=condvar29);
%garch(tab30,resultsp30, var=WMT,   condvar=condvar30);

%garch(tab31,resultsp31, var=AFG,   condvar=condvar31);
%garch(tab32,resultsp32, var=AMED,  condvar=condvar32);
%garch(tab33,resultsp33, var=ANF,   condvar=condvar33);
%garch(tab34,resultsp34, var=ASH,   condvar=condvar34);
%garch(tab35,resultsp35, var=CRS,   condvar=condvar35);
%garch(tab36,resultsp36, var=DIN,   condvar=condvar36);
%garch(tab37,resultsp37, var=GHC,   condvar=condvar37);
%garch(tab38,resultsp38, var=IDA,   condvar=condvar38);
%garch(tab39,resultsp39, var=JLL,   condvar=condvar39);
%garch(tab40,resultsp40, var=LAD,   condvar=condvar40);
%garch(tab41,resultsp41, var=LDL,   condvar=condvar41);
%garch(tab42,resultsp42, var=MGPI,  condvar=condvar42);
%garch(tab43,resultsp43, var=PTC,   condvar=condvar43);
%garch(tab44,resultsp44, var=UTHR,  condvar=condvar44);
%garch(tab45,resultsp45, var=WDFC,  condvar=condvar45);

/** Construction of the final portfolio with all data centered and reduced Studentized **/
data ptf_final456;
	merge tab1 (rename=(NORM=ACIW)) tab2 (rename=(NORM=ADS)) tab3 (rename=(NORM=BOH)) tab4 (rename=(NORM=CBT)) tab5 (rename=(NORM=CLGX))
	tab6 (rename=(NORM=CREE)) tab7 (rename=(NORM=FL)) tab8 (rename=(NORM=HOG)) tab9 (rename=(NORM=HXL)) tab10 (rename=(NORM=ICUI))
	tab11 (rename=(NORM=KBH)) tab12 (rename=(NORM=OSK)) tab13 (rename=(NORM=PNM)) tab14 (rename=(NORM=RS)) tab15 (rename=(NORM=PZZA))
	tab16 (rename=(NORM=AAPL)) tab17 (rename=(NORM=AMGN)) tab18 (rename=(NORM=BA)) tab19 (rename=(NORM=CAT)) tab20 (rename=(NORM=DIS))
	tab21 (rename=(NORM=GS)) tab22 (rename=(NORM=HD)) tab23 (rename=(NORM=KO)) tab24 (rename=(NORM=MCD)) tab25 (rename=(NORM=MMM))
	tab26 (rename=(NORM=MSFT)) tab27 (rename=(NORM=TRV)) tab28 (rename=(NORM=UNH)) tab29 (rename=(NORM=VZ)) tab30 (rename=(NORM=WMT))
	tab31 (rename=(NORM=AFG)) tab32 (rename=(NORM=AMED)) tab33 (rename=(NORM=ANF)) tab34 (rename=(NORM=ASH)) tab35 (rename=(NORM=CRS))
	tab36 (rename=(NORM=DIN)) tab37 (rename=(NORM=GHC)) tab38 (rename=(NORM=IDA)) tab39 (rename=(NORM=JLL)) tab40 (rename=(NORM=LAD))
	tab41 (rename=(NORM=LDL)) tab42 (rename=(NORM=MGPI)) tab43 (rename=(NORM=PTC)) tab44 (rename=(NORM=UTHR)) tab45 (rename=(NORM=WDFC));
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
	    set ptf_final456(firstobs=&date1. obs=&date2.);
	    run;

	    proc iml;

	    /* Covariance */
	    varNames = {"ACIW" "ADS" "BOH" "CBT" "CLGX" "CREE" "FL" "HOG" "HXL" "ICUI" "KBH" "OSK" "PNM" "RS" "PZZA" "AAPL" "AMGN" "BA" "CAT" "DIS" "GS" "HD" "KO" "MCD" "MMM" "MSFT" "TRV" "UNH" "VZ" "WMT" "AFG" "AMED" "ANF" "ASH" "CRS" "DIN" "GHC" "IDA" "JLL" "LAD" "LDL" "MGPI" "PTC" "UTHR" "WDFC"};
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

%macro_heatmap(tab, 1, 897);

/**************************************************************************************************************************************************/

/** Printing of the empirical distribution of eigenvalues **/
proc iml;

	/* Correlations and rolling windows: taken from a normal law. In practice, if you are doing RMT, normalize each variable E[x]=0 et V[x]=1 */  
	varNames = {"ACIW" "ADS" "BOH" "CBT" "CLGX" "CREE" "FL" "HOG" "HXL" "ICUI" "KBH" "OSK" "PNM" "RS" "PZZA" "AAPL" "AMGN" "BA" "CAT" "DIS" "GS" "HD" "KO" "MCD" "MMM" "MSFT" "TRV" "UNH" "VZ" "WMT" "AFG" "AMED" "ANF" "ASH" "CRS" "DIN" "GHC" "IDA" "JLL" "LAD" "LDL" "MGPI" "PTC" "UTHR" "WDFC"};
	use ptf_final456; read all var varNames into X; close ptf_final456;

	free lambda;
	wind=6*22;/* Rolling time window */ 
	do t=1 to nrow(X)-wind ;
		X1=X[t:t+wind,];/* Matrix partition selection */
		C=corr(X1);
		do i=1 to ncol(C);
			corrcoef456 = corrcoef456 // C[,i];
		end;
		lambda=lambda//(t||t+wind||max(eigval(corr(X1)))); 
		SP456 = SP456 // max(eigval(corr(X1)));
		lam = lam // eigval(corr(X1));
	end;

	varNames = {"Date"};
	use ptf456; read all var varNames into Date; close ptf456;

	mattrib Lambda[colname={'Start','End','Lambda max'}];

	call series(Lambda[,2],Lambda[,3]);

	call histogram (lam);
	call QNTL(q,lam,0.95);
	print q;
	call histogram(SP400);
	call QNTL(q,SP400,0.95);
	print q;

	Create Coeffcorr456 var {corrcoef456};
		append;
		close;
	Create SP_456 var {Date SP456};
		append;
		close SP_456;

/**************************************************************************************************************************************************/

/** Printing of empirical distributions of correlation coefficients **/
data correlation456; set Coeffcorr456;
	if CORRCOEF456 = 1 then delete;
run;

proc iml;
	varNames = {"CORRCOEF"};
	use correlation; read all var varNames into correlation; close correlation;

	call histogram(correlation) density={'kernel'}
		scale="count";

/**************************************************************************************************************************************************/

/** Preparation of the tables to print the plots **/
data graph;
	merge SP_456 VIX indiceSP500;
	by Date;
run;
data graph2;
	merge graph indiceSP500;
	by Date2;
run;

/**************************************************************************************************************************************************/


