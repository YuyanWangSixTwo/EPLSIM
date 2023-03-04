#' This is updated data from original data based on NHANES 2003–2004 survey
#'
#' A data set containing outcome triglyceride, re-named ten exposures,
#'  and transformed confounders.
#'
#' \describe{
#'   \item{triglyceride}{outcome triglyceride level, unite mg/dl}
#'   \item{X1_trans.b.carotene}{renamed exposure: trans-b-carotene (ug/dL)}
#'   \item{X2_retinol}{renamed exposure: retinol (ug/dL)}
#'   \item{X3_g.tocopherol}{renamed exposure: g-tocopherol (ug/dL)}
#'   \item{X4_a.tocopherol}{renamed exposure: a-tocopherol (ug/dL)}
#'   \item{X5_PCB99}{renamed exposure: Polychlorinated Biphenyl (PCB) 99 Lipid Adj (ng/g)}
#'   \item{X6_PCB156}{renamed exposure: Polychlorinated Biphenyl (PCB) 156 Lipid Adj (ng/g)}
#'   \item{X7_PCB206}{renamed exposure: Polychlorinated Biphenyl (PCB) 206 Lipid Adj (ng/g)}
#'   \item{X8_3.3.4.4.5.pncb}{renamed exposure: 3,3,4,4,5-Pentachlorobiphenyl (pncb) Lipid Adj (pg/g)}
#'   \item{X9_1.2.3.4.7.8.hxcdf}{renamed exposure: 1,2,3,4,7,8-hxcdf Lipid Adj (pg/g)}
#'   \item{X10_2.3.4.6.7.8.hxcdf}{renamed exposure: 2,3,4,6,7,8-hxcdf Lipid Adj (pg/g)}
#'   \item{AGE.c}{rescaled continuous confounder: subject age at measurement}
#'   \item{SEX.Female}{categorical confounder dummy variable: subject sex as Female}
#'   \item{RACE.NH.Black}{categorical dummy variable: subject race as Non-Hispanic Black}
#'   \item{RACE.MexicanAmerican}{categorical dummy variable: subject race as Mexican American}
#'   \item{RACE.OtherRace}{categorical dummy variable: subject race as Other Races}
#'   \item{RACE.Hispanic}{categorical dummy variable: subject race as Hispanic}
#' }
#' @author Yuyan Wang \email{yuyan.wang@@nyumc.org}
"nhanes.new"
