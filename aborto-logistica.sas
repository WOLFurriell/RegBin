proc import out = dados dbms = csv replace
			datafile = "C:\Users\User\Dropbox\5° Série\Análise de Dados Categóricos\Regressão Logística\scripts\dados-aborto.csv";
			delimiter = ";";
run;

proc logistic data = dados plots=roc(id=obs);
	class freqmissa(ref = "More than once a week") / param = ref;
	model aborto(event = "Sim") = escolaridade freqmissa / link = logit 
			expb ctable lackfit aggregate = (freqmissa escolaridade) scale = none;
quit; 

proc genmod data = dados descending;
	class freqmissa(ref = "More than once a week") / param = ref;
	model aborto = escolaridade freqmissa / dist = bin link = logit lrci;
quit;

/*cuidado com a referencia para a covariável*/
proc catmod data = dados;
	direct escolaridade;
	model aborto = escolaridade freqmissa / freq covb corrb itprint design;
quit; 
