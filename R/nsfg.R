#' Read and recode National Survey of Family Growth (NSFG) data
#'
#' @param years vector: a numeric vector containing the starting year of NSFG waves to include (2002, 2006, 2011, 2013, 2015, 2017)
#' @param progress boolean: display a progress bar
#'
#' @details
#' The U.S. Centers for Disease Control \href{https://www.cdc.gov/nchs/nsfg/index.htm}{National Survey of Family Growth} (NSFG)
#'    regularly collects fertility and other health information from a population-representative sample of adults in the
#'    United States. Between 1973 and 2002, the NSFG was conducted periodically. Starting in 2002, the NSFG transitioned to
#'    continuous data collection, releasing data in three-year waves (e.g., the 2013-2015, 2015-2017). The `nsfg()` function reads
#'    the raw data from CDC's website, extracts and recodes selected variables useful for studying childfree adults and other family
#'    statuses, then returns a single data frame.
#'
#' **Known issues**
#'   * Starting in 2006, "hispanic" was a response option for race, however "hispanic" is not a racial category, but an ethnicity.
#'     When a respondent chose this option, their actual race is unknown.
#'
#' @return A data frame containing variables described in the codebook available using \code{vignette("codebooks")}
#'
#' @export
#'
#' @examples
#' \donttest{data <- nsfg(years = 2017)}
nsfg <- function(years, progress = TRUE) {

  if (!all(years %in%c(2002, 2006, 2011, 2013, 2015, 2017))) {stop("Only the following NSFG years are available: 2002, 2006, 2011, 2013, 2015, 2017")}  #Check for valid years
  years <- sort(years)  #Put years in order

  if (progress) {message("Processing NSFG data files -")}
  if (progress) {pb <- utils::txtProgressBar(min = 0, max = length(years), initial = 0, style = 3)} #Initialize progress bar
  year.num <- 1

  #Loop over each supplied data file
  for (year in years) {

    #Increment progress bar
    if (progress) {utils::setTxtProgressBar(pb,year.num)}

    #Import raw data
    if (year==2002) {raw <- readLines("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/2002FemResp.dat")}
    if (year==2006) {raw <- readLines("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/2006_2010_FemResp.dat")}
    if (year==2011) {raw <- readLines("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/2011_2013_FemRespData.dat")}
    if (year==2013) {raw <- readLines("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/2013_2015_FemRespData.dat")}
    if (year==2015) {raw <- readLines("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/2015_2017_FemRespData.dat")}
    if (year==2017) {raw <- readLines("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/2017_2019_FemRespData.dat")}

    #Initialize dataframe with id variable
    if (year==2002) {dat <- data.frame(id = as.character(substring(raw,1,12)))}
    if (year==2006 | year==2011 | year==2013 | year==2015 | year==2017) {dat <- data.frame(id = as.character(substring(raw,1,5)))}

    #### Family Status ####
    #Source variables
    if (year==2002) {
      dat$hasbabes <- as.numeric(substring(raw,79,79)) #Any live births: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$everadpt <- as.numeric(substring(raw,306,306)) #Adoption experience: 1 = Yes, 3 = Trying, 5 = No
      dat$seekadpt <- as.numeric(substring(raw,307,307))  #Are you seeking to adopt: 1 = Yes, 5 = No, 9 = Don't know
      dat$rwant <- as.numeric(substring(raw,3512,3512)) #Wants a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$rstrstat <- as.numeric(substring(raw,1463,1463)) #Respondent's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$pstrstat <- as.numeric(substring(raw,1464,1464)) #Partner's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$intend <- as.numeric(substring(raw,3522,3522)) #Not partnered & fertile, Intends to have a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$jintend <- as.numeric(substring(raw,3515,3515)) #Partnered & fertile, Intends to have a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
    }
    if (year==2006) {
      dat$hasbabes <- as.numeric(substring(raw,118,118)) #Any live births: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$everadpt <- as.numeric(substring(raw,696,696)) #Adoption experience: 1 = Yes, 3 = Trying, 5 = No
      dat$seekadpt <- as.numeric(substring(raw,697,697))  #Are you seeking to adopt: 1 = Yes, 5 = No, 9 = Don't know
      dat$rwant <- as.numeric(substring(raw,4539,4539)) #Wants a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$rstrstat <- as.numeric(substring(raw,1902,1902)) #Respondent's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$pstrstat <- as.numeric(substring(raw,1903,1903)) #Partner's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$intend <- as.numeric(substring(raw,4550,4550)) #Not partnered & fertile, Intends to have a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$jintend <- as.numeric(substring(raw,4542,4542)) #Partnered & fertile, Intends to have a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
    }
    if (year==2011) {
      dat$hasbabes <- as.numeric(substring(raw,123,123)) #Any live births: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$everadpt <- as.numeric(substring(raw,623,623)) #Adoption experience: 1 = Yes, 3 = Trying, 5 = No
      dat$seekadpt <- as.numeric(substring(raw,624,624))  #Are you seeking to adopt: 1 = Yes, 5 = No, 9 = Don't know
      dat$rwant <- as.numeric(substring(raw,3282,3282)) #Wants a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$rstrstat <- as.numeric(substring(raw,1754,1754)) #Respondent's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$pstrstat <- as.numeric(substring(raw,1755,1755)) #Partner's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$intend <- as.numeric(substring(raw,3293,3293)) #Not partnered & fertile, Intends to have a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$jintend <- as.numeric(substring(raw,3285,3285)) #Partnered & fertile, Intends to have a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
    }
    if (year==2013) {
      dat$hasbabes <- as.numeric(substring(raw,118,118)) #Any live births: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$everadpt <- as.numeric(substring(raw,551,551)) #Adoption experience: 1 = Yes, 3 = Trying, 5 = No
      dat$seekadpt <- as.numeric(substring(raw,552,552))  #Are you seeking to adopt: 1 = Yes, 5 = No, 9 = Don't know
      dat$rwant <- as.numeric(substring(raw,3420,3420)) #Wants a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$rstrstat <- as.numeric(substring(raw,1683,1683)) #Respondent's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$pstrstat <- as.numeric(substring(raw,1684,1684)) #Partner's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$intend <- as.numeric(substring(raw,3252,3252)) #Not partnered & fertile, Intends to have a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$jintend <- as.numeric(substring(raw,3243,3243)) #Partnered & fertile, Intends to have a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      }
    if (year==2015) {
      dat$hasbabes <- as.numeric(substring(raw,96,96)) #Any live births: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$everadpt <- as.numeric(substring(raw,400,400)) #Adoption experience: 1 = Yes, 3 = Trying, 5 = No
      dat$seekadpt <- as.numeric(substring(raw,401,401))  #Are you seeking to adopt: 1 = Yes, 5 = No, 9 = Don't know
      dat$rwant <- as.numeric(substring(raw,2785,2785)) #Wants a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$rstrstat <- as.numeric(substring(raw,1237,1237)) #Respondent's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$pstrstat <- as.numeric(substring(raw,1238,1238)) #Partner's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$intend <- as.numeric(substring(raw,2796,2796)) #Not partnered & fertile, Intends to have a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$jintend <- as.numeric(substring(raw,2788,2788)) #Partnered & fertile, Intends to have a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      }
    if (year==2017) {
      dat$hasbabes <- as.numeric(substring(raw,89,89)) #Any live births: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$everadpt <- as.numeric(substring(raw,213,213)) #Adoption experience: 1 = Yes, 3 = Trying, 5 = No
      dat$seekadpt <- as.numeric(substring(raw,214,214))  #Are you seeking to adopt: 1 = Yes, 5 = No, 9 = Don't know
      dat$rwant <- as.numeric(substring(raw,2410,2410)) #Wants a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$rstrstat <- as.numeric(substring(raw,836,836)) #Respondent's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$pstrstat <- as.numeric(substring(raw,837,837)) #Partner's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$intend <- as.numeric(substring(raw,2421,2421)) #Not partnered & fertile, Intends to have a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$jintend <- as.numeric(substring(raw,2413,2413)) #Partnered & fertile, Intends to have a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
    }

    #Constructed variables
    dat$anykids <- NA  #Does the respondent have biological or adopted children?
    dat$anykids[which(dat$hasbabes==5 & (dat$everadpt!=1 | is.na(dat$everadpt)))] <- 0  #No
    dat$anykids[which(dat$hasbabes==1 | dat$everadpt==1)] <- 1  #Yes

    dat$planadpt <- NA  #Is the respondent trying (currently), or seeking (plans in the future), to adopt?
    dat$planadpt[which((dat$everadpt==5 | is.na(dat$everadpt)) & (dat$seekadpt==5 | is.na(dat$seekadpt)))] <- 0  #No (Have not adopted & don't plan to)
    dat$planadpt[which(dat$everadpt==1 | dat$seekadpt==5)] <- 0  #No (Have adopted, but don't plan to again)
    dat$planadpt[which(dat$everadpt==3 | dat$seekadpt==1)] <- 1  #Yes (May adopt in the future)
    dat$planadpt[which(dat$seekadpt==9 & dat$everadpt!=3)] <- 9  #Don't know (Not currently trying to adopt, don't know about future)

    dat$wantbio <- NA  #Does the respondent want a(nother) biological child?
    dat$wantbio[which(dat$rwant==5)] <- 0  #No
    dat$wantbio[which(dat$rwant==1)] <- 1  #Yes
    dat$wantbio[which(dat$rwant==9)] <- 9  #Don't know

    #Childfree (want)
    dat$cf_want <- NA
    dat$cf_want[which(dat$anykids==0 & dat$wantbio==0 & dat$planadpt==0)] <- 1  #Childfree
    dat$cf_want[which(dat$anykids==1 | dat$wantbio==1 | dat$wantbio==9 | dat$planadpt==1 | dat$planadpt==9)] <- 0  #Not childfree

    #Childfree (expect) - Unknown because intention question only asked of single respondents if they wanted children

    #Family status
    dat$famstat <- NA
    dat$famstat[which(dat$anykids==1)] <- 1  #Parent - Unclassified
    #Parent - Fulfilled: Unknown because parents who do not want another child could also be reluctant
    dat$famstat[which(dat$anykids==1 & (dat$wantbio==1 | dat$planadpt==1))] <- 3 #Parent - Unfulfilled
    #Parent - Reluctant: Unknown because parents who do not want another child could also be fulfilled
    dat$famstat[which(dat$anykids==1 & (dat$wantbio==9 | dat$planadpt==9))] <- 5 #Parent - Ambivalent

    dat$famstat[which(dat$anykids==0 & (dat$wantbio==1 | dat$planadpt==1))] <- 6  #Not yet parent

    #Childless - Unclassified: Not used because all can be classified

    dat$famstat[which(dat$anykids==0 & dat$planadpt==0 & dat$wantbio==1 & dat$intend==5)] <- 8  #Childless - Social: Single respondent who wanted, but do not intend, to have children
    dat$famstat[which(dat$anykids==0 & dat$planadpt==0 & dat$wantbio==1 & dat$jintend==5)] <- 8  #Childless - Social: Partnered respondent who wanted, but do not intend, to have children

    dat$famstat[which(dat$anykids==0 & dat$planadpt==0 & dat$wantbio==1 & (dat$rstrstat==1 | dat$rstrstat==2))] <- 9  #Childless - Biological: Respondent who wanted, but is sterile
    dat$famstat[which(dat$anykids==0 & dat$planadpt==0 & dat$wantbio==1 & (dat$pstrstat==1 | dat$pstrstat==2))] <- 9  #Childless - Biological: Respondent who wanted, but who's partner is sterile

    dat$famstat[which(dat$anykids==0 & (dat$wantbio==9 | dat$planadpt==9))] <- 11 #Undecided

    dat$famstat[which(dat$anykids==0 & dat$intend==5 & (dat$wantbio==9 | dat$planadpt==9))] <- 10  #Ambivalent non-parent: Single respondent who does not intend, but does not know if wanted
    dat$famstat[which(dat$anykids==0 & dat$jintend==5 & (dat$wantbio==9 | dat$planadpt==9))] <- 10  #Ambivalent non-parent: Partnered respondent who does not intend, but does not know if wanted

    dat$famstat[which(dat$anykids==0 & dat$wantbio==0 & dat$planadpt==0)] <- 12 #Childfree

    dat$famstat <- factor(dat$famstat, levels = c(1:12),
                          labels = c("Parent - Unclassified", "Parent - Fulfilled", "Parent - Unfulfilled", "Parent - Reluctant", "Parent - Ambivalent",
                                     "Not yet parent", "Childless - Unclassified", "Childless - Social", "Childless - Biological", "Ambivalent non-parent", "Undecided", "Childfree"))

    #### Demographics ####
    #Sex
    dat$sex <- 1  #All NSFG respondents are female
    dat$sex <- factor(dat$sex, levels = c(1,2,3), labels = c("Female", "Male", "Other"))

    #Race
    if (year==2002) {
      dat$race <- as.numeric(substring(raw,17,17))
      dat$race <- factor(dat$race, levels = c(5,4,3,2,1,99,98), labels = c("White", "Black", "Hawaiian", "Asian", "American Indian", "Other", "Multi-racial"))
    }
    if (year==2006 | year==2011 | year==2013) {  #Coding assumes that 1/2/3 mean the same here as in 2002, despite not being labeled in the provided Stata .do code
      dat$race <- as.numeric(substring(raw,10,10))
      dat$race <- factor(dat$race, levels = c(5,4,3,2,1,96,99), labels = c("White", "Black", "Hawaiian", "Asian", "American Indian", "Other", "Multi-racial"))
    }
    if (year==2015 | year==2017) {
      dat$race <- as.numeric(substring(raw,10,10))
      dat$race[which(dat$race==4)] <- NA  #Hispanic, unknown race
      dat$race[which(dat$race==7)] <- NA  #Not asked
      dat$race[which(dat$race==8)] <- NA  #Refused
      dat$race[which(dat$race==9)] <- NA  #Don't know
      dat$race <- factor(dat$race, levels = c(3,2,99,98,97,1,99), labels = c("White", "Black", "Hawaiian", "Asian", "American Indian", "Other", "Multi-racial"))
    }

    #Hispanic
    if (year==2002) {dat$hispanic <- as.numeric(substring(raw,16,16))}
    if (year==2006 | year==2011 | year==2013 | year==2015 | year==2017) {dat$hispanic <- as.numeric(substring(raw,9,9))}
    dat$hispanic[which(dat$hispanic==7)] <- NA  #Not asked
    dat$hispanic[which(dat$hispanic==8)] <- NA  #Refused
    dat$hispanic[which(dat$hispanic==9)] <- NA  #Don't know
    dat$hispanic[which(dat$hispanic==5)] <- 0  #Not hispanic
    dat$hispanic[which(dat$hispanic==1)] <- 1  #Hispanic

    #Age in years
    if (year==2002) {dat$age <- as.numeric(substring(raw,20,21))}
    if (year==2006 | year==2011 | year==2013 | year==2015 | year==2017) {dat$age <- as.numeric(substring(raw,13,14))}
    dat$age[which(dat$age==98)] <- NA  #Refused
    dat$age[which(dat$age==99)] <- NA  #Don't know

    #Education in years
    if (year==2006) {dat$higrade <- as.numeric(substring(raw,39,40))}
    if (year==2002 | year==2011) {dat$higrade <- as.numeric(substring(raw,43,44))}
    if (year==2013) {dat$higrade <- as.numeric(substring(raw,40,41))}
    if (year==2015 | year==2017) {dat$higrade <- as.numeric(substring(raw,36,37))}
    dat$higrade[which(dat$higrade==98)] <- NA  #Refused
    dat$higrade[which(dat$higrade==99)] <- NA  #Don't know

    if (year==2002) {dat$dipged <- as.numeric(substring(raw,47,47))}
    if (year==2011) {dat$dipged <- as.numeric(substring(raw,46,46))}
    if (year==2006 | year==2013) {dat$dipged <- as.numeric(substring(raw,43,43))}
    if (year==2015 | year==2017) {dat$dipged <- as.numeric(substring(raw,39,39))}
    dat$dipged[which(dat$dipged==8)] <- NA  #Refused
    dat$dipged[which(dat$dipged==9)] <- NA  #Don't know

    if (year==2011) {dat$havedeg <- as.numeric(substring(raw,71,71))}
    if (year==2006 | year==2013) {dat$havedeg <- as.numeric(substring(raw,68,68))}
    if (year==2002 | year==2015 | year==2017) {dat$havedeg <- as.numeric(substring(raw,52,52))}
    dat$havedeg[which(dat$havedeg==7)] <- NA  #Not asked
    dat$havedeg[which(dat$havedeg==8)] <- NA  #Refused
    dat$havedeg[which(dat$havedeg==9)] <- NA  #Don't know

    if (year==2011) {dat$degrees <- as.numeric(substring(raw,72,72))}
    if (year==2006 | year==2013) {dat$degrees <- as.numeric(substring(raw,69,69))}
    if (year==2002 | year==2015 | year==2017) {dat$degrees <- as.numeric(substring(raw,53,53))}
    dat$degrees[which(dat$degrees==8)] <- NA  #Refused
    dat$degrees[which(dat$degrees==9)] <- NA  #Don't know

    dat$education <- NA
    dat$education[which(dat$higrade<=12 & dat$dipged!=1 & dat$dipged!=2 & dat$dipged!=3)] <- 2  #Did not finish high school (yet)
    dat$education[which(dat$dipged==1 | dat$dipged==2 | dat$dipged==3)] <- 3  #High school graduate
    dat$education[which(dat$higrade>12 & (dat$havedeg==5 | dat$degrees==1))] <- 4  #Some college (no degree or associates degree)
    dat$education[which(dat$degrees==2)] <- 5  #College degree
    dat$education[which(dat$degrees==3 | dat$degrees==4 | dat$degrees==5)] <- 7  #Graduate degree
    dat$education <- factor(dat$education,
                            levels = c(1:7),
                            labels = c("No education", "Did not graduate high school", "High School graduate",
                                       "Some college", "College graduate", "Some post-graduate", "Graduate degree"),
                            ordered = TRUE)

    #Partnership status
    if (year==2006 | year==2011 | year==2013) {dat$marstat <- as.numeric(substring(raw,21,21))}
    if (year==2002 | year==2015 | year==2017) {dat$marstat <- as.numeric(substring(raw,28,28))}
    dat$partnered <- NA
    dat$partnered[which(dat$marstat==6)] <- 1  #Single, never married
    dat$partnered[which(dat$marstat==1 | dat$marstat==2)] <- 2  #Currently partnered
    dat$partnered[which(dat$marstat==3 | dat$marstat==4 | dat$marstat==5)] <- 3  #Formerly partnered
    dat$partnered <- factor(dat$partnered, levels = c(1,2,3), labels = c("Never", "Currently", "Formerly"))

    #Residence
    if (year==2002) {dat$metro <- as.numeric(substring(raw,4821,4821))}
    if (year==2006) {dat$metro <- as.numeric(substring(raw,6116,6116))}
    if (year==2011) {dat$metro <- as.numeric(substring(raw,4890,4890))}
    if (year==2013) {dat$metro <- as.numeric(substring(raw,5016,5016))}
    if (year==2015) {dat$metro <- as.numeric(substring(raw,4454,4454))}
    if (year==2017) {dat$metro <- as.numeric(substring(raw,3772,3772))}
    dat$residence <- NA
    dat$residence[which(dat$metro==1)] <- 4  #Principal city of MSA = Urban
    dat$residence[which(dat$metro==2)] <- 3  #Other part of MSA = Urban
    dat$residence[which(dat$metro==3)] <- 1  #Not in MSA = Rural
    dat$residence <- factor(dat$residence, levels = c(1,2,3,4), labels = c("Rural", "Town", "Suburb", "Urban"), ordered = TRUE)

    #Employed
    if (year==2002) {dat$rwrkst <- as.numeric(substring(raw,3674,3674))}
    if (year==2006) {dat$rwrkst <- as.numeric(substring(raw,4758,4758))}
    if (year==2011) {dat$rwrkst <- as.numeric(substring(raw,3511,3511))}
    if (year==2013) {dat$rwrkst <- as.numeric(substring(raw,3461,3461))}
    if (year==2015) {dat$rwrkst <- as.numeric(substring(raw,3008,3008))}
    if (year==2017) {dat$rwrkst <- as.numeric(substring(raw,2663,2663))}
    dat$employed <- NA
    dat$employed[which(dat$rwrkst==1)] <- 1  #Employed
    dat$employed[which(dat$rwrkst==5)] <- 0  #Not employed

    #In school
    if (year==2006) {dat$goschol <- as.numeric(substring(raw,37,37))}
    if (year==2002 | year==2011) {dat$goschol <- as.numeric(substring(raw,41,41))}
    if (year==2013) {dat$goschol <- as.numeric(substring(raw,38,38))}
    if (year==2015 | year==2017) {dat$goschol <- as.numeric(substring(raw,34,34))}
    dat$inschool <- NA
    dat$inschool[which(dat$goschol==1)] <- 1  #In school
    dat$inschool[which(dat$goschol==5)] <- 0  #Not in school

    #### Attitude ####
    #Religion
    if (year==2002) {dat$relcurr <- as.numeric(substring(raw,3653,3654))}
    if (year==2006) {dat$relcurr <- as.numeric(substring(raw,4728,4729))}
    if (year==2011) {dat$relcurr <- as.numeric(substring(raw,3493,3494))}
    if (year==2013) {dat$relcurr <- as.numeric(substring(raw,3444,3445))}
    if (year==2015) {dat$relcurr <- as.numeric(substring(raw,2990,2991))}
    if (year==2017) {dat$relcurr <- as.numeric(substring(raw,2644,2645))}
    dat$religion <- NA
    dat$religion[which(dat$relcurr==1)] <- 1  #None
    dat$religion[which(dat$relcurr==2)] <- 2  #Catholic
    dat$religion[which(dat$relcurr==3)] <- 5  #Baptist/Southern Baptist ==> Protestant
    dat$religion[which(dat$relcurr==4)] <- 5  #Methodist, Lutheran, Presbyterian, Episcopal ==> Protestant
    dat$religion[which(dat$relcurr==5)] <- 5  #Fundamentalist Protestant ==> Protestant
    dat$religion[which(dat$relcurr==6)] <- 5  #Other Protestant denomination ==> Protestant
    dat$religion[which(dat$relcurr==7)] <- 5  #Protestant - No specific denomination ==> Protestant
    dat$religion[which(dat$relcurr==8)] <- 6  #Other
    dat$religion <- factor(dat$religion, levels = c(1:6), labels = c("None", "Catholic / Orthodox", "Muslim", "Jewish", "Protestant / Christian", "Other"))

    #### Design ####
    #Identifier - This step is performed above, when initializing the data frame

    #Country
    dat$country <- "United States"

    #Sampling weight
    if (year==2002) {dat$weight <- as.numeric(substring(raw,4873,4891))}
    if (year==2006) {dat$weight <- as.numeric(substring(raw,6150,6168))}
    if (year==2011) {dat$weight <- as.numeric(substring(raw,4906,4922))}
    if (year==2013) {dat$weight <- as.numeric(substring(raw,5032,5048))}
    if (year==2015) {dat$weight <- as.numeric(substring(raw,4470,4486))}
    if (year==2017) {dat$weight <- as.numeric(substring(raw,3787,3803))}

    #Wave
    if (year==2002) {dat$wave <- "2002"}
    if (year==2006) {dat$wave <- "2006-2010"}
    if (year==2011) {dat$wave <- "2011-2013"}
    if (year==2013) {dat$wave <- "2013-2015"}
    if (year==2015) {dat$wave <- "2015-2017"}
    if (year==2017) {dat$wave <- "2017-2019"}

    #Year of data collection
    if (year==2002) {dat$cmintvw <- as.numeric(substring(raw,4894,4897))}
    if (year==2006) {dat$cmintvw <- as.numeric(substring(raw,6226,6229))}
    if (year==2011) {dat$cmintvw <- as.numeric(substring(raw,4926,4929))}
    if (year==2013) {dat$cmintvw <- as.numeric(substring(raw,5052,5055))}
    if (year==2015) {dat$cmintvw <- as.numeric(substring(raw,4490,4493))}
    if (year==2017) {dat$cmintvw <- as.numeric(substring(raw,3807,3810))}
    dat$year <- 1900+floor((dat$cmintvw-1)/12)

    #Month of data collection
    dat$month <- dat$cmintvw - (12 * (dat$year - 1900))
    dat$month <- factor(dat$month, levels = c(1:12), labels = c("January", "February", "March", "April", "May", "June",
                                                                "July", "August", "September", "October", "November", "December"),
                        ordered = TRUE)

    #Source file
    if (year==2002) {dat$file <- "2002FemResp.dat"}
    if (year==2006) {dat$file <- "2006_2010_FemRespData.dat"}
    if (year==2011) {dat$file <- "2011_2013_FemRespData.dat"}
    if (year==2013) {dat$file <- "2013_2015_FemRespData.dat"}
    if (year==2015) {dat$file <- "2015_2017_FemRespData.dat"}
    if (year==2017) {dat$file <- "2017_2019_FemRespData.dat"}

    #Source survey
    dat$survey <- "NSFG"

    #### Clean up ####
    #Reduce data
    dat <- dat[,c("cf_want", "famstat",  #Family status
                  "sex", "race", "hispanic", "age", "education", "partnered", "residence", "employed", "inschool",  #Demographics
                  "religion",  #Attitude
                  "id", "country", "weight", "file", "survey", "wave", "year", "month")]  #Design

    #Start data file, or append to existing data file
    if (year==min(years)) {data <- dat} else {data <- rbind(data, dat)}
    year.num <- year.num + 1

  }

  #Finalize
  if (progress) {close(pb)}  #Close progress bar
  class(data) <- c("data.frame", "childfree")
  return(data)  #Export data
}
