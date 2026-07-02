## Objectives 🎯

This project aims to manage data quality by automating several workflows:

- **Get data.** For both the demand- and supply-side surveys:
  - Download all data
  - Combine data from all versions
- **Validate interviews.** 
  - Check logical data for errors and statistical outliers.
  - Recommend actions to take
  - Create propose interviews to reject
  - Create a report to monitor both validations done by this program and validations done by Survey Solutions.
- **Reject flagged interviews.**
  - Collect interviews to be rejected
  - Reject them on the server, posting interview- and question-level comments
- **Create monitoring report.**
  - Computes statistics for monitoring indicators
  - Creates a report of those statistics

## Installation 🔌

- [Install prerequisites 🧰](#install-prerequisites-)
- [Download the project ⬇️](#download-the-project-️)
- [Provide parameters ⚙️](#provide-parameters-️)

### Install prerequisites 🧰

- [R](#r) version 4.5.3
- [RTools](#rtools) version 4.5
- [RStudio](#rstudio) any version from 2024 onward

<details>

<summary>
Open to see more details 👁️
</summary>

#### R

- Follow this [link](https://cran.r-project.org/)
- Click on your operating system
- Click on `base`
- Find version 4.5.3
- Download and install (e.g.,
  [this](https://cran.r-project.org/bin/windows/base/old/4.5.3/R-4.5.3-win.exe) for Windows)

#### RTools

Necessary when run on a Windows operaterating system

- Follow this [link](https://cran.r-project.org/)
- Click on `Windows`
- Click on `RTools`
- Find version 4.5
- Download
  (e.g.,[this](https://cran.r-project.org/bin/windows/Rtools/rtools45/files/rtools45-6768-6492.exe) for a 64bit  architecture)
- Install in the default location suggested by the installer
(e.g., `C:\rtools4'`)

This program allows R to compile source code written in C++ and and that used by certain packages to be more performant (e.g., `{dplyr}`).

#### RStudio

- Follow this [link](https://posit.co/download/rstudio-desktop/)
- Click on the `DOWNLOAD RSTUDIO` button
- Select the right file for your operating system
- Download and install (e.g.,
  [this](https://download1.rstudio.org/electron/windows/RStudio-2024.09.1-394.exe) for Windows)

RStudio is required for a few reasons:

1. Provides a good interface for using R
2. Ships with [Quarto](https://quarto.org/), a program that this project will use for creating reports

</details>

### Download the project ⬇️

- Click on the `Code` button
- Select `Download zip` and download
- Unzip the project on your device

### Provide parameters ⚙️

Before running the program, one needs to do some one-time setup by:

- Opening `_parameters.R`
- Inputting a few details described below

#### Server details

For the program to act on your behalf, it needs you to:

1. Create an API account
2. Provide server connection details

##### Create an API account

On the target Survey Solutions server, create an API user account (see process [here](https://docs.mysurvey.solutions/headquarters/accounts/teams-and-roles-tab-creating-user-accounts/)) and give it access to the workspace that contains your questionnaire (see process [here](https://docs.mysurvey.solutions/headquarters/accounts/adding-users-to-workspaces/))

##### Provide server connection details

Since the program will be connecting to the server as an API user, one must give it the following informations:

```r
# =============================================================================
# 1. Server details
# =============================================================================

server    <- "" # full URL of the server
workspace <- "" # use the `name` attribute rather than the `display name`.
user      <- "" # user name for the API account
password  <- "" # password for the API user account

```

#### Questionnaires

For each target questionnaire, provide two or three pieces of information:

1. Some text that identifies it. See details in the comment string below. See more about regular expressions [here](https://regexlearn.com/)
2. Questionnaire variable. See more [here](https://docs.mysurvey.solutions/questionnaire-designer/components/questionnaire-variable/) about where to find that information.
3. Data and variable names.

These pieces of information will permit the program to download the data from the questionnaire and correctly identify which data file is the "main" one.

```r
# =============================================================================
# 2. Questionnaires
# =============================================================================

# For each questionniare, provide:
#
# 1. text that identifies the questionnaire(s), which could be:
#   - a full name/title
#   - a sub-text
#   - a regular expression
#
# 2. the "questionnaire variable" value as it appears in Designer. To find it:
#   - log into Designer
#   - open the questionnaire
#   - click on `SETTINGS`
#   - copy the values in the `questionnaire variable` filed
#
# For more information, please see:
# https://docs.mysurvey.solutions/questionnaire-designer/components/questionnaire-variable/

# -----------------------------------------------------------------------------
# Household questionnaire
# -----------------------------------------------------------------------------

# text that identifies the questionnaire(s)
household_qnr_expr <- ""
# value of the questionnaire variable
household_qnr_var  <- ""

# name of the member-level file, as it appears in Designer and without `.dta`
members_roster_var <- ""

# name of variables used for outlier groups: admin1 and urban/rural
admin1_var <- ""
urb_rur_var <- ""

# -----------------------------------------------------------------------------
# Community questionnaire
# -----------------------------------------------------------------------------

# text that identifies the questionnaire(s)
community_qnr_expr <- ""
# value of the questionnaire variable
community_qnr_var  <- ""

```

#### Validation behavior

For the validation workflow to work, it needs to know:

1. **Which interview statuses to review.** In effect, at what point(s) in the interview approval process should the program intervene: immediately once the interviewer posts an interview, after a supervisor approves an interview, and/or after the head office approves an interview?
2. **What types of issues trigger rejection.** Only issues that merit rejection (default) or others as well?

Unlike other sections of `_parameters.R`, this section contains default values.

See the comments of the file for more details.

```r
# =============================================================================
# 3. Validation behavior
# =============================================================================

# Provide a comma-delimted vector of interview statuses to review
# See the values here:
# https://docs.mysurvey.solutions/headquarters/export/system-generated-export-file-anatomy/#coding_status
# Status values supported by this project:
# - Completed: 100
# - ApprovedBySupervisor: 120
# - ApprovedByHeadquarters: 130
suso_statuses_to_reject <- c(100, 120)

# Provide a comma-delimited vector of issue types to reject
# {susoreview} uses the following codes:
# - 1 = Reject
# - 2 = Comment a variable
# - 3 = Survey Soluttions validation error
# - 4 = Review
issue_codes_to_reject <- c(1)
```

#### Report period

For each report indicator, the report provides two sets of information:

- Statistics for the report period
- Trends covering the full data collection period (including beyond the period indicated)

If no values are provided, the program will assume that the report should cover the last completed week and determine that period using your device's current date.

If one chooses to indicate start and end dates, provide the dates as character strings that follow the [ISO 8601 date format](https://en.wikipedia.org/wiki/ISO_8601).

```r
# =============================================================================
# 4. Report period
# =============================================================================

report_start  <- ""
report_end    <- ""
```
## Usage 👩‍💻

- [Open 📂](#open-)
- [Launch 🚀](#launch-)
- [Choose 👉](#choose-)
- [Consume 👀](#consume-)

#### Open 📂

For the program to work properly, it should be opened as a project.

To do this, double-click on the project file: `lsms2_dms.Rproj`.

This will do two useful things:

1. Open RStudio
2. Open this program in RStudio as a project and activate its project environment (e.g., install required packages at the project level). (To learn more, read [here](https://rstats.wtf/projects#rstudio-projects) and [here](https://support.posit.co/hc/en-us/articles/200526207-Using-RStudio-Projects).)

#### Launch 🚀

To run the program:

- Open `run_workflow.R`
- Source the script

#### Choose 👉

Once this script is run, you will be asked to choose a workflow:

```
── Data Quality Workflows ───────────────────────────────────
Which workflow would you like to run? 

1: Get data
2: Validate interviews
3: Reject flagged interviews
4: Create monitoring report

Selection: 
```

To execute a workflow, enter the number of your choice and press `Enter`.

Here is what each workflow entails:

- **Get data.** For both the household and commmunity surveys:
  - Download all data
  - Combine all versions
- **Validate interviews.** 
  - Check logical data for errors and statistical outliers.
  - Recommend actions to take
  - Create propose interviews to reject
  - Create a report to monitor both validations done by this program and validations done by Survey Solutions.
- **Reject flagged interviews.**
  - Collect interviews to be rejected
  - Reject them on the server, posting interview- and question-level comments
- **Create monitoring report.**
  - Computes statistics for monitoring indicators
  - Creates a report of those statistics

In addition, there is another way to get the data. If the data can only be obtained as a zip file (e.g., emailed by the data collection partner), the zip file should be placed in `01_data/{survey}/{}/00_inbox`. From there, the validation workflow can run, and that workflow will take care of unpacking and preparing the emailed data for use.

Note: the emailed, zipped data can currently follow any of these formats:

- Zip file of data files (e.g., containing `households.dta`, `members.dta`, etc.)
- Zip file of folders (e.g., `folder1/`, `folder2`, etc. that each contain data files inside).
- Zip file of zip files (e.g. `data_v1.zip`, `data_v2.zip`, etc that are each the data downloaded from Survey Solutions for a particular questionnaire version).

#### Consume 👀

##### Data

Find the combined data.

For the household survey: `01_data/01_household/02_combined`

For the community survey: `01_data/02_community/02_combined`

##### Validation errors

The validation workflow produces two sets of outputs

1. Recommendations
2. Rejections
3. Reports

Both of these are currently only available for the household survey.

###### Recommendations

The validation workflow reviews interviews, identifies issues, and recommends actions accordingly:

- **Reject.** If there 1+ validation issue and no potentially explanatory comment, neither for the interview overall nor for questions involved in the issue(s) found. These interviews can be rejected.
- **Review.** If would have been rejected except for potentially explanatory comments. These interviews require human review, at the very least to read the comments.
- **Follow-up.** If there is 1+ validation that has already been the basis for prior rejection. Since rejection has not lead to remedying the issue, survey managers need to follow-up to understand why not.

The results can be found in `02_validation/01/demand/01_recommendations`.

There are several files present that can be understood as follows:

- Intermediary files:
  - `attribs`. Files of data attributes used to create issues.
  - `issues`. File of issue-level observations.
  - `interviews_validated`. Interview ID of interviews validated by the system.
- Recommendation files.
  - Actions:
    - `to_reject_*`. Interviews that contains 1 or more issue that warrants rejection, without any potentially explanatory comments.
    - `to_review_*`. `Interviews that would otherwise be rejected except that there are potentially explanatory comments to be reviewed, whether comments on the interview as a whole or comments on the questions involved in the issues.
    - `to_follow_up_*`. Interviews that were rejected once already and contain unresolved issues.
  - Contents:
    - `*_ids`. Interview-level file containing the interview__id and interview__key of cases that fall into this category.
    - `*_issues`. Issue-level file of issues for the interviews in this category.
    - `*_api`. Contains the information that the API needs to reject the interview: interview__id, reject_comment (a concatenation of all issue messages), and interview__status.

###### Rejections

To help with rejections, the program copies recommended rejections to `02_validation/01/demand/01_decisions`.

The `to_reject_api.xlsx` contains recommendations and can be edited by the user, whether that editing mean removing interviews, adding interviews, or editing the reasons for rejection.

The contents of this file will be used to reject interviews with the rejection workflow.

###### Reports

The validation workflow produces two sets of reports on the issues found by the validation workflow and by Survey Solutions:

- Headquarters report
- Team-level report (🚧 Not yet implemented 🚧)

The Headquarters report provides an overview of the top issues overall and the number of issues by team.

##### Rejected interviews

See [rejections](#rejections) above.

##### Monitoring report

🚧 Not yet implemented 🚧

<!-- ## Troubleshooting 🔨 -->