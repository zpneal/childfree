#' Read and recode Michigan State of the State (SOSS) data
#'
#' @param waves vector: a numeric vector containing the SOSS waves to include (currently available: 79, 82, 84, 85, 86)
#' @param extra.vars vector: a character vector containing the names of variables to be retained from the raw data
#' @param progress boolean: display a progress bar
#'
#' @details
#' The State of the State Survey (SOSS) is regularly collected by the Institute for Public Policy and
#'    Social Research (IPPSR) at Michigan State University (MSU). Each wave is collected from a sample
#'    of 1000 adults in the US state of Michigan, and includes sampling weights to obtain a sample that
#'    is representative of the state's population with respect to age, gender, race, and education. The
#'    `soss()` function reads the raw data from IPPSR's website, extracts and recodes selected variables
#'    useful for studying childfree adults and other family statuses, then returns a single data frame. Questions
#'    necessary for identifying childfree adults were asked in five waves, which each include unique questions
#'    that may be of interest:
#'    * \href{http://ippsr.msu.edu/survey-research/state-state-survey-soss/soss-data/soss-79b-spring-2020}{Wave 79} (May 2020) - Neighborhoods, Health care, COVID, Personality
#'    * \href{http://ippsr.msu.edu/survey-research/state-state-survey-soss/soss-data/soss-82-fall-2021}{Wave 82} (September 2021) - Trust in government, Critical Race Theory
#'    * \href{http://ippsr.msu.edu/survey-research/state-state-survey-soss/soss-data/soss-84-spring-2022}{Wave 84} (April 2022) - Trust in scientists, Autonomous vehicles, Morality
#'    * \href{http://ippsr.msu.edu/survey-research/state-state-survey-soss/soss-data/soss-85-fall-2022}{Wave 85} (September 2022) - Reproductive rights, Race equity
#'    * \href{http://ippsr.msu.edu/survey-research/state-state-survey-soss/soss-data/soss-86-winter-2022}{Wave 86} (December 2022) - Education, Infrastructure
#'
#' **Known SOSS data file issues**
#'   * Wave 79 did not include a "do not know" option for selected questions. Therefore, it is not possible to identify
#'     "undecided" or "ambivalent non-parent" respondents. This may lead other family status categories to be inflated.
#'   * Wave 82 originally included a 500 person oversample of parents. These respondents are omitted if `wave == 82`.
#'
#' @return A data frame containing:
#' * *Family Status Variables* (based on \href{https://doi.org/10.1177/10664807231198869}{Neal and Neal's (2024)} framework)
#'   * `cf_want` (factor) - Is the respondent childfree according to a "want" variable
#'   * `famstat` (factor) - Respondent's family status based on all available information:
#'      * A "Parent - unclassified" has children
#'      * A "Not yet parent" does not have children but wants children
#'      * A "Childless" respondent does not have children, are not planning to have children, but wished they had children
#'      * An "Ambivalent non-parent" does not have children, are not planning to have children, and do not know if they wished they had children
#'      * An "Undecided" respondent does not have children and is undecided whether they want children
#'      * A "Childfree" respondent does not have children and does not want children
#' * *Demographic Variables*
#'   * `sex` (factor) - Respondent's sex
#'   * `age` (numeric) - Respondent's age in years
#'   * `education` (numeric) - Respondent's education on a 10-point ordinal scale
#'   * `partnered` (factor) - Respondent's partnership status (*Partnership includes both marriage and cohabitation*)
#'   * `residence` (factor) - Urbanicity of respondent's place of residence
#' * *Design Variables*
#'   * `id` (string) - Unique respondent ID
#'   * `country` (string) - Respondent's country of residence (*The SOSS is collected only in the United States, specifically, Michigan*)
#'   * `weight` (numeric) - Sampling weight (*Exercise caution using weights when data are pooled from multiple waves*)
#'   * `file` (string) - Source data file
#'   * `survey` (string) - Source survey (*Equal to "SOSS" for all data generated using the `soss()` function*)
#'   * `wave` (numeric) - Wave of data collection
#'   * `year` (numeric) - Year of data collection
#'   * `month` (numeric) - Month of data collection
#'
#' @export
#'
#' @references {Neal, Z. P. and Neal, J. W. (2024). A framework for studying adults who neither have nor want children. *The Family Journal, 32*, 121-130. \href{https://doi.org/10.1177/10664807231198869}{https://doi.org/10.1177/10664807231198869}}
#'
#' @examples
#' data <- soss(waves = 84, extra.vars = c("inc"))
soss <- function(waves, extra.vars = NULL, progress = TRUE) {

  if (!all(waves %in%c(79,82,84,85,86))) {stop("Only the following SOSS waves are available: 79, 82, 84, 85, 86")}  #Check for valid waves
  waves <- sort(waves)  #Put waves in order

  if (!is.null(extra.vars)) {extra.vars <- tolower(extra.vars)}  #Make requested extra variables lowercase

  if (progress) {message("Processing SOSS data files -")}
  if (progress) {pb = txtProgressBar(min = 0, max = length(waves), initial = 0, style = 3)} #Initialize progress bar
  wave.num <- 1

  #Loop over each supplied data file
  for (wave in waves) {

    #Increment progress bar
    if (progress) {setTxtProgressBar(pb,wave.num)}

    #Import raw data
    if (wave==79) {dat <- rio::import("http://ippsr.msu.edu/sites/default/files/soss79b.sav")}
    if (wave==82) {dat <- rio::import("https://ippsr.msu.edu/sites/default/files/soss/soss82.sav")}
    if (wave==84) {dat <- rio::import("http://ippsr.msu.edu/sites/default/files/SOSS%2084%20WEIGHTED%20DATASET_5.4.22.sav")}
    if (wave==85) {dat <- rio::import("http://ippsr.msu.edu/sites/default/files/SOSS%2085_WEIGHTED_OUTPUT.sav")}
    if (wave==86) {dat <- rio::import("http://ippsr.msu.edu/sites/default/files/SOSS86_weighted_OUTPUT.sav")}
    colnames(dat) <- tolower(colnames(dat))  #Make all variables lowercase

    #### Family Status ####
    #Do you have children?
    if (wave==79) {
      dat$havekid <- NA
      dat$havekid[which(dat$neal01==1)] <- 1  #Yes
      dat$havekid[which(dat$neal01==2)] <- 2  #No (don't know was not an option)
    }
    if (wave==82) {
      dat$havekid <- NA
      dat$havekid[which(dat$neal1==1)] <- 1  #Yes
      dat$havekid[which(dat$neal1==2)] <- 2  #No
      dat$havekid[which(dat$neal1==6)] <- 3  #Don't know
    }
    if (wave==84 | wave==85) {
      dat$havekid <- NA
      dat$havekid[which(dat$neal1==1)] <- 1  #Yes
      dat$havekid[which(dat$neal1==2)] <- 2  #No
      dat$havekid[which(dat$neal1==7)] <- 3  #Don't know
    }
    if (wave==86) {
      dat$havekid <- NA
      dat$havekid[which(dat$neal1==1)] <- 1  #Yes
      dat$havekid[which(dat$neal1==2)] <- 2  #No
      dat$havekid[which(dat$neal1==3)] <- 3  #Don't know
    }
    dat$havekid <- factor(dat$havekid, levels = c(1,2,3), labels = c("Yes", "No", "DK"))

    #Are you planning to have children?
    if (wave==79) {
      dat$plankid <- NA
      dat$plankid[which(dat$neal02==1)] <- 1  #Yes
      dat$plankid[which(dat$neal02==2)] <- 2  #No (don't know was not an option)
    }
    if (wave==82) {
      dat$plankid <- NA
      dat$plankid[which(dat$neal2==1)] <- 1  #Yes
      dat$plankid[which(dat$neal2==2)] <- 2  #No
      dat$plankid[which(dat$neal2==6)] <- 3  #Don't know
    }
    if (wave==84 | wave==85) {
      dat$plankid <- NA
      dat$plankid[which(dat$neal2==1)] <- 1  #Yes
      dat$plankid[which(dat$neal2==2)] <- 2  #No
      dat$plankid[which(dat$neal2==7)] <- 3  #Don't know
    }
    if (wave==86) {
      dat$plankid <- NA
      dat$plankid[which(dat$neal2==1)] <- 1  #Yes
      dat$plankid[which(dat$neal2==2)] <- 2  #No
      dat$plankid[which(dat$neal2==3)] <- 3  #Don't know
    }
    dat$plankid <- factor(dat$plankid, levels = c(1,2,3), labels = c("Yes", "No", "DK"))

    #Do you wish you could have had children?
    if (wave==79) {
      dat$wishkid <- NA
      dat$wishkid[which(dat$neal03==1)] <- 1  #Yes
      dat$wishkid[which(dat$neal03==2)] <- 2  #No (don't know was not an option)
    }
    if (wave==82) {
      dat$wishkid <- NA
      dat$wishkid[which(dat$neal3==1)] <- 1  #Yes
      dat$wishkid[which(dat$neal3==2)] <- 2  #No
      dat$wishkid[which(dat$neal3==6)] <- 3  #Don't know
    }
    if (wave==84 | wave==85) {
      dat$wishkid <- NA
      dat$wishkid[which(dat$neal3==1)] <- 1  #Yes
      dat$wishkid[which(dat$neal3==2)] <- 2  #No
      dat$wishkid[which(dat$neal3==7)] <- 3  #Don't know
    }
    if (wave==86) {
      dat$wishkid <- NA
      dat$wishkid[which(dat$neal3==1)] <- 1  #Yes
      dat$wishkid[which(dat$neal3==2)] <- 2  #No
      dat$wishkid[which(dat$neal3==3)] <- 3  #Don't know
    }
    dat$wishkid <- factor(dat$wishkid, levels = c(1,2,3), labels = c("Yes", "No", "DK"))

    #Family status
    dat$famstat <- NA
    dat$famstat[which(dat$havekid=="Yes")] <- 0  #Parent - Unclassified
    dat$famstat[which(dat$havekid=="No" & dat$plankid=="Yes")] <- 5  #Not yet parent
    dat$famstat[which(dat$havekid=="No" & dat$plankid=="DK")] <- 8  #Undecided
    dat$famstat[which(dat$havekid=="No" & dat$plankid=="No" & dat$wishkid=="Yes")] <- 6  #Childless
    dat$famstat[which(dat$havekid=="No" & dat$plankid=="No" & dat$wishkid=="DK")] <- 7  #Ambivalent non-parent
    dat$famstat[which(dat$havekid=="No" & dat$plankid=="No" & dat$wishkid=="No")] <- 9  #Childfree
    dat$famstat <- factor(dat$famstat, levels = c(0:9),
                          labels = c("Parent - Unclassified", "Parent - Fulfilled", "Parent - Unfulfilled", "Parent - Reluctant", "Parent - Ambivalent",
                                     "Not yet parent", "Childless", "Ambivalent non-parent", "Undecided", "Childfree"))

    #Childfree (want)
    dat$cf_want <- NA
    dat$cf_want[which(dat$famstat=="Childfree")] <- 1
    dat$cf_want[which(dat$famstat!="Childfree")] <- 0
    dat$cf_want <- factor(dat$cf_want, levels = c(0,1), labels = c("No", "Yes"))

    #### Demographics ####
    #Sex
    dat$sex <- NA
    dat$sex[which(dat$cd1==2)] <- 0  #Female
    dat$sex[which(dat$cd1==1)] <- 1  #Male
    dat$sex[which(dat$cd1==3)] <- 2  #Intersex
    dat$sex <- factor(dat$sex, levels = c(0,1,2), labels = c("female", "male", "intersex"))

    #Age in years
    if (wave==79) {dat$age <- 2020 - dat$cd2}
    if (wave==82) {dat$age <- 2021 - dat$cd2}
    if (wave==84) {dat$age <- 2022 - dat$cd2}
    if (wave==85) {dat$age <- 2022 - dat$cd2}
    if (wave==86) {dat$age <- 2022 - dat$cd2}

    #Education in years
    dat$education <- dat$cd3
    dat$education[which(dat$education>0 & dat$education<12)] <- 19
    dat$education <- factor(dat$education,
                            levels = c(0, 19, 12, 13, 14, 20, 15, 16, 17, 18),
                            labels = c("No education", "Did not graduate high school", "High School graduate",
                                       "1st year college", "2nd year college", "Junior college graduate",
                                       "3rd year college", "College graduate", "Some post-graduate", "Graduate degree"),
                            ordered = TRUE)

    #Partnership status
    dat$partnered <- NA
    dat$partnered[which(dat$cd8==6)] <- 0  #Single, never married
    dat$partnered[which(dat$cd8==1 | dat$cd8==5)] <- 1  #Currently partnered
    dat$partnered[which(dat$cd8==2 | dat$cd8==3 | dat$cd8==4)] <- 2  #Formerly partnered
    dat$partnered <- factor(dat$partnered, levels = c(0,1,2), labels = c("Never", "Currently", "Formerly"))

    #Residence
    dat$residence <- dat$x1
    dat$residence[which(dat$residence==5 | dat$residence==8)] <- NA
    dat$residence <- factor(dat$residence, levels = c(1,2,3,4), labels = c("Rural", "Town", "Suburb", "Urban"))

    #### Design ####
    #Identifier
    dat$id <- dat$caseid

    #Country
    dat$country <- "United States"

    #Sampling weight
    if (wave==82) {dat <- dat[which(!is.na(dat$weight)),]}  #In wave 82, only use base sample; drop parent oversample
    dat$weight <- dat$weight

    #Wave
    dat$wave <- wave

    #Year of data collection
    if (wave==79) {dat$year <- 2020}
    if (wave==82) {dat$year <- 2021}
    if (wave==84) {dat$year <- 2022}
    if (wave==85) {dat$year <- 2022}
    if (wave==86) {dat$year <- 2022}

    #Month of data collection
    if (wave==79) {dat$month <- 5}
    if (wave==82) {dat$month <- 9}
    if (wave==84) {dat$month <- 4}
    if (wave==85) {dat$month <- 9}
    if (wave==86) {dat$month <- 12}

    #Source file
    if (wave==79) {dat$file <- "soss79b.sav"}
    if (wave==82) {dat$file <- "soss82.sav"}
    if (wave==84) {dat$file <- "SOSS%2084%20WEIGHTED%20DATASET_5.4.22.sav"}
    if (wave==85) {dat$file <- "SOSS%2085_WEIGHTED_OUTPUT.sav"}
    if (wave==86) {dat$file <- "SOSS86_weighted_OUTPUT.sav"}

    #Source survey
    dat$survey <- "SOSS"

    #### Clean up ####
    #Check for extra variables; if not present, add them
    if (!is.null(extra.vars)) {
      for (var in extra.vars) {if (!(var %in% colnames(dat))) {  #For each extra variable that is not present:
        dat$v8675309 <- NA   #(1) Add a temporary missing variable called `v8675309`
        colnames(dat)[length(colnames(dat))] <- var  #(2) Rename this new missing variable with the correct name
        }
      }
    }

    #Reduce data
    if (!is.null(extra.vars)) {
      dat <- dat[,c("cf_want", "famstat",  #Family status
                    "sex", "age", "education", "partnered", "residence",  #Demographics
                    "id", "country", "weight", "file", "survey", "wave", "year", "month",  #Design
                    extra.vars)]
    } else {
      dat <- dat[,c("cf_want", "famstat",  #Family status
                    "sex", "age", "education", "partnered", "residence",  #Demographics
                    "id", "country", "weight", "file", "survey", "wave", "year", "month")]  #Design
    }

    #Start data file, or append to existing data file
    if (wave==min(waves)) {data <- dat} else {data <- rbind(data, dat)}
    wave.num <- wave.num + 1

  }

  #Finalize
  if (progress) {close(pb)}  #Close progress bar
  class(data) <- c("data.frame", "childfree")
  return(data)  #Export data
}