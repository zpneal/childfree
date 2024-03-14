#' Read and recode UN Demographic and Health Surveys (DHS) individual data
#'
#' @param files vector: a character vector containing the paths for one or more Individual Recode DSH data files (see details)
#' @param extra.vars vector: a character vector containing the names of variables to be retained from the raw data
#' @param progress boolean: display a progress bar
#'
#' @details
#' The United Nations Demographic and Health Surveys (DHS) program regularly collects health data from
#'    population-representative samples in many countries using standardized surveys. The "individual
#'    recode" data files contain women's responses, and are available in SPSS, SAS, and Stata formats
#'    from \href{https://www.dhsprogram.com/}{https://www.dhsprogram.com/}. Access to these data requires
#'    a free application, however a sample data file can be obtained \href{https://dhsprogram.com/data/Download-Model-Datasets.cfm}{here}
#'    without an application. The `dhs()` function reads one or more of these files, extracts and
#'    recodes selected variables useful for studying childfree adults and other family statuses, then returns
#'    a single data frame.
#'
#' **Known DHS data file issues**
#'   * The SPSS-formatted files containing data from Gabon Recode 4 (GAIR41FL.SAV) and Turkey Recode 4 (TRIR41FL.SAV)
#'     contain encoding errors. Use the SAS-formatted files (GAIR41FL.SAS7BDAT and TRIR41FL.SAS7BDAT) instead.
#'   * In some cases, DHS makes available individual recode data files for specific states. For example, data from Ondo
#'     State in Nigeria from Wave 1 is contained in OSIR01FL.SAV, data from states in India from 1999 are contained in
#'     files named XXIR42FL.SAV, where the "XX" is a two-letter state code. This function only accepts whole-country
#'     individual recode data files, and not these state-specific data files.
#'
#' @return A data frame containing:
#' * *Family Status Variables* (based on \href{https://doi.org/10.1177/10664807231198869}{Neal and Neal's (2024)} framework)
#'   * `cf_want` (factor) - Is the respondent childfree according to a "want" variable
#'   * `cf_ideal` (factor) - Is the respondent childfree according to an "ideal" variable
#'   * `famstat` (factor) - Respondent's family status based on all available information:
#'      * A "Parent - unclassified" has children
#'      * A "Parent - Fulfilled" has exactly the number of children that is ideal
#'      * A "Parent - Unfulfilled" has fewer children than is ideal
#'      * A "Parent - Reluctant" has more children than is ideal
#'      * A "Parent - Ambivalent" has children but does not know how many is ideal
#'      * A "Not yet parent" does not have children but wants children
#'      * A "Childless" respondent does not have children and is infecund but ideally would have liked to have children
#'      * An "Ambivalent non-parent" does not have children and is infecund but does not know if they ideally would have liked to have children
#'      * An "Undecided" respondent does not have children and is undecided whether they want children, or provided
#'         inconsistent responses to the want and ideal questions (e.g., want = no, ideal > 0; want = yes, ideal = 0).
#'      * A "Childfree" respondent does not have children and does not want children or ideally would like zero children. If the respondent
#'         provided responses to *both* the want and ideal questions, these responses are consistent (i.e., want = no *and* ideal = 0).
#' * *Demographic Variables*
#'   * `sex` (factor) - Respondent's sex (*The DHS data include only female respondents*)
#'   * `age` (numeric) - Respondent's age in years
#'   * `education` (numeric) - Respondent's years of education
#'   * `partnered` (factor) - Respondent's partnership status (*Partnership includes both marriage and cohabitation*)
#'   * `residence` (factor) - Urbanicity of respondent's place of residence
#' * *Design Variables*
#'   * `id` (string) - Unique respondent ID
#'   * `country` (string) - Respondent's country of residence
#'   * `weight` (numeric) - Sampling weight (*Exercise caution using weights when data are pooled from multiple countries or waves*)
#'   * `file` (string) - Source data file
#'   * `survey` (string) - Source survey (*Equal to "DHS" for all data generated using the `dhs()` function*)
#'   * `wave` (numeric) - Wave of data collection (*In the DHS, "waves" are called "recodes"*)
#'   * `year` (numeric) - Year of data collection
#'   * `month` (numeric) - Month of data collection
#'
#' @export
#'
#' @references {Neal, Z. P. and Neal, J. W. (2024). A framework for studying adults who neither have nor want children. *The Family Journal, 32*, 121-130. \href{https://doi.org/10.1177/10664807231198869}{https://doi.org/10.1177/10664807231198869}}
#'
#' @examples
#' \dontrun{data <- dhs(files = c("AFIR71FL.SAV", "ALIR51FL.SAV"), extra.vars = c("v201")}
dhs <- function(files, extra.vars = NULL, progress = TRUE) {

  if (!is.null(extra.vars)) {extra.vars <- tolower(extra.vars)}  #Make requested extra variables lowercase

  if (progress) {message("Processing DHS data files -")}
  if (progress) {pb <- utils::txtProgressBar(min = 0, max = length(files), initial = 0, style = 3)} #Initialize progress bar

  #Loop over each supplied data file
  for (file in 1:length(files)) {

    #Increment progress bar
    if (progress) {utils::setTxtProgressBar(pb,file)}

    #Import raw data
    dat <- rio::import(files[file])
    colnames(dat) <- tolower(colnames(dat))  #Make all variables lowercase

    #### Family Status ####
    #Number of children
    dat$numkid <- dat$v201

    #Want children
    dat$want <- dat$v602
    dat$want[which(dat$want==4)] <- 5  #Combine 4-Sterilized with 5-Infecund
    dat$want[which(dat$want>=6)] <- NA  #Various labels, none about wants
    dat$want <- factor(dat$want, levels = c(1,2,3,5), labels = c("Have (another)", "Undecided", "No (more)", "Infecund"))

    #Ideal number of children
    dat$ideal <- dat$v613
    dat$ideal[which(dat$ideal==98)] <- -1  #Special code for "Don't Know"
    dat$ideal[which(dat$ideal>30)] <- NA  #Treat all values above 30 as missing (includes some undocumented country-specific special codes)

    #Childfree (want)
    dat$cf_want <- NA
    dat$cf_want[which(!is.na(dat$numkid) & dat$numkid==0 &
                      !is.na(dat$want) & dat$want=="No (more)")] <- 2  #Childfree if (a) have no children and (b) want no children
    dat$cf_want[which(!is.na(dat$numkid) & dat$numkid>0)] <- 1  #Not childfree if have children
    dat$cf_want[which(!is.na(dat$want) & (dat$want=="Have (another)" | dat$want=="Undecided"))] <- 1  #Not childfree if want or may want children
    dat$cf_want <- factor(dat$cf_want, levels = c(1,2), labels = c("No", "Yes"))

    #Childfree (ideal)
    dat$cf_ideal <- NA
    dat$cf_ideal[which(!is.na(dat$numkid) & dat$numkid==0 &
                       !is.na(dat$ideal) & dat$ideal==0)] <- 2  #Childfree if (a) have no children and (b) zero children is ideal
    dat$cf_ideal[which(!is.na(dat$numkid) & dat$numkid>0)] <- 1  #Not childfree if have children
    dat$cf_ideal[which(!is.na(dat$ideal) & (dat$ideal==-1 | dat$ideal>0))] <- 1  #Not childfree if it is ideal to have some number of children, or the ideal number of children is unknown
    dat$cf_ideal <- factor(dat$cf_ideal, levels = c(1,2), labels = c("No", "Yes"))

    #Family status
    dat$famstat <- NA
    dat$famstat[which(!is.na(dat$numkid) & dat$numkid>0)] <- 1  #Parent - Unclassified (known number of children greater than zero)
    dat$famstat[which(!is.na(dat$numkid) & dat$numkid>0 &
                      !is.na(dat$ideal) & dat$numkid==dat$ideal)] <- 2  #Parent - Fulfilled (has ideal number of children)
    dat$famstat[which(!is.na(dat$numkid) & dat$numkid>0 &
                      !is.na(dat$ideal) & dat$numkid<dat$ideal)] <- 3  #Parent - Unfulfilled (has less than ideal number of children)
    dat$famstat[which(!is.na(dat$numkid) & dat$numkid>0 &
                      !is.na(dat$ideal) & dat$numkid>dat$ideal & dat$ideal!=-1)] <- 4  #Parent - Reluctant (has more than ideal number of children)
    dat$famstat[which(!is.na(dat$numkid) & dat$numkid>0 &
                      !is.na(dat$ideal) & dat$ideal==-1)] <- 5  #Parent - Ambivalent (unsure how many children is ideal)

    dat$famstat[which(!is.na(dat$numkid) & dat$numkid==0 &
                      !is.na(dat$want) & dat$want=="Have (another)")] <- 6  #Not yet parent (wants child(ren), regardless of how many is ideal)

    dat$famstat[which(!is.na(dat$numkid) & dat$numkid==0 &
                      !is.na(dat$want) & dat$want=="Infecund" &
                      !is.na(dat$ideal) & dat$ideal>0)] <- 7  #Childless (cannot have children, but a specific number would have been ideal)

    dat$famstat[which(!is.na(dat$numkid) & dat$numkid==0 &
                      !is.na(dat$want) & dat$want=="Infecund" &
                      !is.na(dat$ideal) & dat$ideal==-1)] <- 8  #Ambivalent non-parent (cannot have children, ideal number is unknown)

    dat$famstat[which(!is.na(dat$numkid) & dat$numkid==0 &
                        !is.na(dat$want) & dat$want=="Undecided")] <- 9  #Undecided (unsure if want children)

    dat$famstat[which(!is.na(dat$numkid) & dat$numkid==0 &
                      !is.na(dat$want) & dat$want=="No (more)")] <- 10  #Childfree (do not want children)
    dat$famstat[which(!is.na(dat$numkid) & dat$numkid==0 &
                      !is.na(dat$ideal) & dat$ideal==0)] <- 10  #Childfree (zero children is ideal)

    dat$famstat[which(!is.na(dat$numkid) & dat$numkid==0 &
                      !is.na(dat$ideal) & dat$ideal==0) &
                      !is.na(dat$want) & dat$want!="No (more)"] <- 9  #Undecided, ideal and want responses are inconsistent
    dat$famstat[which(!is.na(dat$numkid) & dat$numkid==0 &
                      !is.na(dat$ideal) & dat$ideal>0) &
                      !is.na(dat$want) & dat$want=="No (more)"] <- 9  #Undecided, ideal and want responses are inconsistent

    dat$famstat <- factor(dat$famstat, levels = c(1:10),
                          labels = c("Parent - Unclassified", "Parent - Fulfilled", "Parent - Unfulfilled", "Parent - Reluctant", "Parent - Ambivalent",
                                     "Not yet parent", "Childless", "Ambivalent non-parent", "Undecided", "Childfree"))

    #### Demographics ####
    #Sex
    dat$sex <- 1
    dat$sex <- factor(dat$sex, levels = c(1,2,3), labels = c("Female", "Male", "Other"))

    #Age in years
    dat$age <- dat$v012

    #Education in years
    dat$education <- dat$v133
    dat$education[dat$education>=40] <- NA

    #Partnership status
    dat$partnered <- dat$v502 + 1
    dat$partnered <- factor(dat$partnered, levels = c(1,2,3), labels = c("Never", "Currently", "Formerly"))

    #Residence
    dat$residence <- dat$v102
    if (dat$v000[1]=="MX" & dat$v007[1]==87) {  #Mexico Wave 1 used a different coding
      dat$residence[which(dat$residence<4)] <- 2  #Code as rural (1) Less than 2500, (2) 2500-19999, and (3) 20000+
      dat$residence[which(dat$residence==4)] <- 1  #Code as urban (4) Areas Metropolitanas
    }
    dat$residence <- factor(dat$residence, levels = c(2,1), labels = c("Rural", "Urban"), ordered = TRUE)

    #### Design ####
    #Identifier (non-standard variable name in Egypt 1988-89)
    if (dat$v000[1]=="EG" & (dat$v007[1]==88 | dat$v007[1]==89)) {dat$id <- dat$`case$id`} else {dat$id <- dat$caseid}
    dat$id <- as.character(dat$id)

    #Country
    country.codes <- c("AF", "AL", "AO", "AM", "AZ", "BD", "BJ", "BO", "BT", "BR", "BF", "BU", "KH", "CM", "CV", "CF", "TD", "CO", "KM", "CG",
                       "CD", "CI", "DR", "EC", "EG", "ES", "EK", "ER", "ET", "GA", "GM", "GH", "GU", "GN", "GY", "HT", "HN", "IA", "ID", "JO",
                       "KK", "KE", "KY", "LA", "LS", "LB", "MD", "MW", "MV", "ML", "MR", "MX", "MB", "MA", "MZ", "MM", "NM", "NP", "NC", "NI",
                       "NG", "OS", "PK", "PY", "PE", "PH", "RW", "WS", "ST", "SN", "SL", "ZA", "LK", "SD", "SZ", "TJ", "TZ", "TH", "TL", "TG",
                       "TT", "TN", "TR", "TM", "UG", "UA", "UZ", "VN", "YE", "ZM", "ZW", "PG")
    country.names <- c("Afghanistan", "Albania", "Angola", "Armenia", "Azerbaijan", "Bangladesh", "Benin", "Bolivia", "Botswana", "Brazil", "Burkina Faso", "Burundi", "Cambodia", "Cameroon", "Cape Verde", "Central African Republic", "Chad", "Columbia", "Comoros", "Congo",
                       "Congo Democratic Republic", "Cote d'Ivoire", "Dominican Republic", "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guatamala", "Guinea", "Guyana", "Haiti", "Honduras", "India", "Indonesia", "Jordan",
                       "Kazakhstan", "Kenya", "Kyrgyz Republic", "Lao People's Democratic Republic", "Lesotho", "Liberia", "Madagascar", "Malawi", "Maldives", "Mali", "Mauritania", "Mexico", "Moldova", "Morocco", "Mozambique", "Myanmar", "Namibia", "Nepal", "Nicaragua", "Niger",
                       "Nigeria", "Nigeria (Ondo State)", "Pakistan", "Paraguay", "Peru", "Philippines", "Rwanda", "Samoa", "Sao Tome and Principe", "Senegal", "Sierra Leone", "South Africa", "Sri Lanka", "Sudan", "Swaziland", "Tajikstan", "Tanzania", "Thailand", "Timor-Leste", "Togo",
                       "Trinidad and Tobago", "Tunisia", "Turkey", "Turkministan", "Uganda", "Ukraine", "Uzbekistan", "Vietnam", "Yemen", "Zambia", "Zimbabwe", "Papua New Guinea")
    dat$country <- country.names[match(substr(dat$v000,1,2), country.codes)]

    #Sampling weight
    dat$weight <- dat$v005/1000000  #Sampling weight

    #Wave (called "Recode" in the DHS)
    dat$wave <- as.numeric(substr(dat$v000,3,3))  #Recode
    dat$wave[which(is.na(dat$wave))] <- 1  #In recode 1, v000 only contained the country code
    dat$wave[which(dat$country=="Vietnam" & dat$v007==97)] <- 3  #Recode was labeled as "T" for Vietnam 1997
    dat$wave[which(dat$country=="Vietnam" & dat$v007==2)] <- 4  #Recode was labeled as "T" for Vietnam 2002

    #Year of data collection
    dat$year <- dat$v007
    dat$year[which(dat$year>=85 & dat$year<=99 & dat$country!="Nepal")] <- dat$year[which(dat$year>=85 & dat$year<=99 & dat$country!="Nepal")] + 1900  #Fix two-digit years
    dat$year[which(dat$year>=0 & dat$year<=10 & dat$country!="Nepal")] <- dat$year[which(dat$year>=0 & dat$year<=10 & dat$country!="Nepal")] + 2000  #Fix two-digit years
    dat$year[which(dat$year>1900 & dat$country=="Nepal")] <- dat$year[which(dat$year>1900 & dat$country=="Nepal")] - 57  #Fix Nepali years
    dat$year[which(dat$year<100 & dat$country=="Nepal")] <- dat$year[which(dat$year<100 & dat$country=="Nepal")] + 1943
    dat$year[which(dat$country=="Afghanistan" & dat$wave==7)] <- 2015  #The 2015 Afghanistan used year from Afghan calendar

    #Month of data collection
    dat$month <- dat$v006
    dat$month <- factor(dat$month, levels = c(1:12), labels = c("January", "February", "March", "April", "May", "June",
                                                                "July", "August", "September", "October", "November", "December"),
                        ordered = TRUE)

    #Source file
    dat$file <- files[file]

    #Source survey
    dat$survey <- "DHS"

    #### Clean up ####
    #Check for extra variables; if not present, add them
    for (var in extra.vars) {if (!(var %in% colnames(dat))) {  #For each extra variable that is not present:
      dat$v8675309 <- NA   #(1) Add a temporary missing variable called `v8675309`
      colnames(dat)[length(colnames(dat))] <- var  #(2) Rename this new missing variable with the correct name
      }
    }

    #Reduce data
    if (!is.null(extra.vars)) {
      dat <- dat[,c("cf_want", "cf_ideal", "famstat",  #Family status
                    "sex", "age", "education", "partnered", "residence",  #Demographics
                    "id", "country", "weight", "file", "survey", "wave", "year", "month",  #Design
                    extra.vars)]
    } else {
      dat <- dat[,c("cf_want", "cf_ideal", "famstat",  #Family status
                    "sex", "age", "education", "partnered", "residence",  #Demographics
                    "id", "country", "weight", "file", "survey", "wave", "year", "month")]  #Design
    }

    #Start data file, or append to existing data file
    if (file==1) {data <- dat} else {data <- rbind(data, dat)}

  }

  #Finalize
  if (progress) {close(pb)}  #Close progress bar
  class(data) <- c("data.frame", "childfree")
  return(data)  #Export data
}
