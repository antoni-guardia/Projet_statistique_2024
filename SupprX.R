rm(list=ls())

data <- readRDS("base_PC_Var_X_Var_Y_ENSAI_Respi_FINALE.RData",  "rb")

names_suppr <- c(
  "T16_BS_Pres.EssuiMain", "T21_Elev_ContPo",
  "T21_Elev_Cot", "X04_AUTRESP",
  "X13x2_LAVMain_Cad", "X13x2_BOTTSpecEqua",
  "LIT_PS", "A03_Pos10sTs",
  "A03_sd10sSt", "A03_sd22sSt",
  "A03_My10sAs", "A03_My22sAs",
  "A03_Md10sAs", "A03_Md22sAs",
  "A03_sd10sAs", "A03_sd22sAs",
  "A03_TxPos10sTs", "A03_My10sTs",
  "A03_Md10sTs", "A03_sd10sTs",
  "A03_sd22sTs", "A03_sdSero22sTgPP",
  "A08_Classe_1TO10_10s", "A01_TxPos22sHAPTO",
  "T11_PS_NoteIndPoQue_0", "T14_ENG_NoteIndPoBoit_2",
  "T14_ENG_NoteIndPoQue_0", "T14_ENG_NoteIndPoQue_BIN",
  "T11_PS_NoteIndPoDiarr_2", "T11_PS_NoteIndPoDys",
  "T11_PS_NoteIndPoGroinD", "T14_ENG_NoteIndPoDiarr_0",
  "T14_ENG_NoteIndPoDiarr_1", "T14_ENG_NoteIndPoDiarr_2",
  "T14_ENG_NoteIndPoDiarr_BIN", "T14_ENG_NoteIndPoDys",
  "T14_ENG_NoteIndPoGroinD", "T14_ENG_NoteIndPoAnémi",
  "T10_PS_EauNbPopPo_1", "T10_PS_EauNbPopPo_3",
  "T13_ENG_EauNbPo_1", "Label",
  "A06_TxPos22sSDRPreel", "A01_TxPos10sHAPTO",
  "A01_TxPos22sHAPTO", "A01_TxDtx22sHAPTO",
  "T11_PS_NoteIndPoBles_2", "T14_ENG_NoteIndPoBoit_2",
  "T14_ENG_NoteIndPoBoit_2", "T14_ENG_NoteIndPoHOmb"
)

colonnes_a_conserver <- setdiff(names(data), names_suppr)

# Mise à jour du dataframe
data <- data[, colonnes_a_conserver]
