libname fic "Z:\Projets\IsabelleAuger\PackagesR_divers\OutilsDRF\tests\testthat\fixtures\volume_utilisable";

data param;
  set fic.tiges_vol_ut5;
run;
proc freq data=param; table essence; run;
data param2;
  set param;
  if essence in ("BOG", "BOJ", "BOP", "CET", "CHR", "ERR", "ERS", "FRA", "FRN", "HEG", "ORA", "OSV", "PEB", "PEG", "PET", "TIL",
                 "EPB", "EPN", "EPR", "MEL", "PIB", "PIG", "PIR", "PRU", "SAB", "THO",
                 "CHB", "PED");
  if etat='10';
  if dhp_cm>9;
  VMU_dm3 = VMUtig*1000;
  VM_dm3 = VMtig*1000;
  keep id_pe no_arbre essence etat dhp_cm ht_complet ratio_ht_dhp est_vol_se dhp_se Def_theo_se Hm_theo_se VMU_dm3 VM_dm3;
run;
* 1302546;
proc freq data=param2; table essence; run;

proc sort data=param2 out=param3 nodupkey; by essence dhp_cm; run;
* 9532;

proc univariate data=param3; var VMU_dm3 VM_dm3; run;

proc export data=param3 replace dbms=xlsx file="Z:\Projets\IsabelleAuger\PackagesR_divers\OutilsDRF\tests\testthat\fixtures\volume_utilisable\tiges_vol_ut5.xlsx" ; run;

data verif;
  set param;
  if essence='EPN' and dhp_cm=30;
run;