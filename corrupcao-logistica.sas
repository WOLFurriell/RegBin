proc import out = dados dbms = dlm replace
			datafile = "C:\Users\User\Dropbox\5° Série\Análise de Dados Categóricos\Regressão Logística\scripts\corrupcao.csv";
			delimiter = ",";
			getnames = yes;
run;

proc logistic  data = dados plots = roc;
	class sexo2(ref = "Masculino") Ljato(ref = "Ira_diminuir") denunciaTemer(ref = "Sim_deveria") / param = ref;
	model corrupcao(event = "Corrupcao") = sexo2 ljato denunciatemer idade1 rendaf escola partido2 avaltemer / 
			link = logit ctable lackfit;
quit;

/*Modelo final selecionado pelo Wesley*/
proc logistic  data = dados plots = roc;
	class Ljato(ref = "Continuara_igual") denunciaTemer(ref = "Nao_deveria") / param = ref;
	model corrupcao(event = "Corrupcao") = idade1 escola ljato denunciatemer /	
			link = logit ctable lackfit aggregate = (idade1 escola ljato denunciatemer) scale = none;
quit;
