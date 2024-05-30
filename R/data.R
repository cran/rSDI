#' Türkiye migration network flows data
#'
#' This data frame contains the data on migration of people between Türkiye's provinces
#' in the period 2016-2017-2018.
#' This is a consolidated version of raw data from Turkish Statistical Institute.
#'
#' Each row contains the number of people migrated in the "weight" column. 'from' and 'to'
#' columns include the codes of provinces as used in the Türkiye statistical coding system.
#'
#' The familiar names of provinces and their locations are to be found in a separate data frame
#' named TurkiyeMigration.nodes
#' @format ## `TurkiyeMigration.flows`
#' A data frame with 6480 rows and 3 columns:
#' \describe{
#'   \item{from, to}{codes of origin and arrival province}
#'   \item{weight}{number of people migrated}
#' }
#' @source <https://data.tuik.gov.tr/Kategori/GetKategori?p=Nufus-ve-Demografi-109>
"TurkiyeMigration.flows"

#' Türkiye migration network provinces/nodes data
#'
#' This data frame contains the data on Türkiye's provinces as used in the migration flows
#' data frame (TurkiyeMigration.flow)
#'
#' Each row contains the code of the province as used in TurkiyeMigration.flows data frame,
#' it is known name/label, and latitude/longitude of the province central.
#'
#' @format ## `TurkiyeMigration.nodes`
#' A data frame with 6480 rows and 3 columns:
#' \describe{
#'   \item{id}{codes of province as used in the TurkiyeMigration.flows data frame}
#'   \item{label}{Name of the province capital city}
#'   \item{longitude, latitude}{spatial coordinates of the province capital}
#' }
#' @source The latitude/longitude of Turkish province capitals was shared as a courtesy of Başarsoft LLC
"TurkiyeMigration.nodes"
