############
# This is to run after lines 215-230 from script '2_subset.R':
# # Handle date conditions separately
# df_recode <- df_recode %>%
#   mutate(
#     d_time_cuts_DEF = case_when(
#       !is.na(d_time_cuts_DEF) ~ d_time_cuts_DEF,  # Retain 'censored' and true NA's
#       # is.na(d_death) ~ NA_character_,  # Handle true NAs
#       is.na(d_time_cuts_DEF) & FECHA_DEF < T0_start ~ 'pre_T0',
#       is.na(d_time_cuts_DEF) & FECHA_DEF >= T0_start & FECHA_DEF <= T0_end ~ 'T0',
#       is.na(d_time_cuts_DEF) & FECHA_DEF > T0_end & FECHA_DEF < T1_start ~ 'gap_T0_T1',
#       is.na(d_time_cuts_DEF) & FECHA_DEF >= T1_start & FECHA_DEF <= T1_end ~ 'T1',
#       is.na(d_time_cuts_DEF) & FECHA_DEF > T1_end & FECHA_DEF < T2_start ~ 'gap_T1_T2',
#       is.na(d_time_cuts_DEF) & FECHA_DEF >= T2_start & FECHA_DEF <= T2_end ~ 'T2',
#       is.na(d_time_cuts_DEF) & FECHA_DEF > T2_end ~ 'post_T2',
#       TRUE ~ 'check'  # Default case if none of the above match
#     )
#   )

# Return 'True_NAs' placeholder back to actual NA:
# True NA's are from non-sensical dates coded in original (max follow-up) outcome var (d_death)
# This and other tries re-introduce rows of NA's. They seem to be additional rows that couldn't be handled though.
# This appears to solely be when subsetting and not in the actual dataframe.
# Check separate script trying to debug this 'subset_NA_rows_issue.R'
# So, converted to single vector, recoded, reinserted in to dataframe, jeeeez.
df <- data.frame()
df <- df_recode
df$d_time_cuts_DEF <- trimws(df$d_time_cuts_DEF)
typeof(df$d_time_cuts_DEF)
str(df$d_time_cuts_DEF)
class(df$d_time_cuts_DEF)
nrow(df)
length(which(df$d_time_cuts_DEF == 'true_NA'))
# View((df[df$d_time_cuts_DEF == 'true_NA', ]))
# View(df[which(df$d_time_cuts_DEF == 'true_NA'), 'd_time_cuts_DEF'])

vec_test <- character()
vec_test <- as.character(df$d_time_cuts_DEF)
typeof(vec_test)
class(vec_test)
str(vec_test)
length(vec_test)
vec_test[which(vec_test == 'true_NA')]
length(which(vec_test == 'true_NA')) == length(which(df$d_time_cuts_DEF == 'true_NA'))

vec_test <- ifelse(vec_test == 'true_NA', NA_character_, vec_test)
!any(vec_test[which(vec_test == 'true_NA')]) # should be TRUE, zero
vec_NAs <- which(is.na(vec_test))
vec_test[vec_NAs]

typeof(vec_test)
class(vec_test)
str(vec_test)
summary(factor(vec_test))
length(vec_test) == nrow(df)
length(which(is.na(vec_test))) == length(which(df$d_time_cuts_DEF == 'true_NA')) # should be TRUE, NA == 'true_NA'
# Fine up to here


# Create a dataframe to check if recoding worked and then join, etc. Massive workaround.
# Stil not working, can't id pb. 
test_df <- data.frame(id = as.character(df_recode$ID_REGISTRO),
                      d_time_cuts_DEF_pre = as.character(df_recode$d_time_cuts_DEF),
                      d_time_cuts_DEF_post = as.character(vec_test)
                      )
nrow(test_df) == nrow(data_f)

test_df$check <- ifelse(test_df$d_time_cuts_DEF_pre == test_df$d_time_cuts_DEF_post, "match", "no match")
nrow(test_df) == nrow(data_f)

which(test_df$check == "no match") # zero
# All match
test_df[which(is.na(test_df$d_time_cuts_DEF_post)), ]
test_df[which(test_df$d_time_cuts_DEF_pre == "true_NA"), ]

epi_head_and_tail(test_df, cols = 4)

# TO DO: continue here
# Somehow pre and post are not the same when subsetting but there are zero 'no match':
epi_head_and_tail(test_df[test_df$d_time_cuts_DEF_pre == 'pre_T0', ], cols = 4)
epi_head_and_tail(test_df[test_df$d_time_cuts_DEF_post == 'pre_T0', ], cols = 4)
# but there are no empty(NA) rows:
test_df[rowSums(is.na(test_df)) == ncol(test_df), ] # should be empty
subset_preT0 <- test_df[test_df$d_time_cuts_DEF_post == 'pre_T0', ]
# But not when subsetting:
epi_head_and_tail(subset_preT0, cols = 4)
subset_preT0[rowSums(is.na(subset_preT0)) == ncol(subset_preT0), ] # should be empty but isn't
# Clean the subset by removing rows that are completely NA
subset_preT0_clean <- subset_preT0[!is.na(subset_preT0$id), ]
subset_preT0_clean[rowSums(is.na(subset_preT0_clean)) == ncol(subset_preT0_clean), ] # should be empty
# Check the head and tail of the cleaned subset
epi_head_and_tail(subset_preT0_clean, cols = 4)


# Re-insert into dataframe:
df_j <- dplyr::left_join(df, test_df,
                         by = c('ID_REGISTRO' = 'id')
                        )
# Does not error after removing duplicates


epi_head_and_tail(df_j, last_cols = TRUE)
colnames(df_j)

# df$d_time_cuts_DEF <- as.character(vec_test)
# df$d_time_cuts_DEF <- as.character(df$d_time_cuts_DEF)
nrow(df_j) == nrow(data_f)
summary(factor(df_j$d_time_cuts_DEF))
summary(factor(df_j$d_time_cuts_DEF_pre))
summary(factor(df_j$d_time_cuts_DEF_post))

epi_head_and_tail(df_j[df_j$d_time_cuts_DEF == 'pre_T0', ], cols = 6)
epi_head_and_tail(df_j[df_j$d_time_cuts_DEF_pre == 'pre_T0', ], cols = 6)
epi_head_and_tail(df_j[df_j$d_time_cuts_DEF_post == 'pre_T0', ], cols = 6)
epi_head_and_tail(df_j[df_j$d_time_cuts_DEF_post == 'pre_T0', ], last_cols = TRUE)


epi_head_and_tail(df_recode[df_recode$d_time_cuts_DEF == 'pre_T0', ], cols = 6)

View(tail(df[df$d_time_cuts_DEF == 'pre_T0', ], n = 100))
View(tail(df_recode[df_recode$d_time_cuts_DEF == 'pre_T0', ], n = 100))
# sink()
############


############
# This was a duplicate:
data_f[which(data_f$ID_REGISTRO == 'g1feb61'), ]
############



############
dim(df_recode)
summary(factor(df_recode$d_time_cuts_DEF))
str(df_recode)

# Not OK:
epi_head_and_tail(df_recode[df_recode$d_time_cuts_DEF == 'pre_T0', ], cols = 6)
epi_head_and_tail(df_recode[df_recode$d_time_cuts_DEF == 'T0', ], cols = 6)
epi_head_and_tail(df_recode[df_recode$d_time_cuts_DEF == 'T1', ], cols = 6)


df_recode$d_time_cuts_DEF <- factor(df_recode$d_time_cuts_DEF,
                                 levels = c('pre_T0', 'T0',
                                            'gap_T0_T1', 'T1',
                                            'gap_T1_T2', 'T2',
                                            'post_T2', 'censored'),
                                 ordered = TRUE
                                 )

summary(df_recode$d_time_cuts_DEF)
summary(df_recode$d_time_cuts_INGRESO)
df_recode$d_time_cuts_DEF
############



############
###
# Sanity check as issues with NAs rows introduced:
df_recode <- data_f

colnames(df_recode)

cols_to_check <- c('ID_REGISTRO',
                   "d_death",
                   "d_days_to_death",
                   "d_time_cuts_INGRESO",
                   "d_time_cuts_DEF",
                   "d_time_cuts_prev"
                   )

dim(data_f)
dim(df_recode)
summary(df_recode[, cols_to_check])
epi_head_and_tail(df_recode[, cols_to_check], cols = 6)
# Looks fine

lapply(cols_to_check, function(x) head(df_recode[, x]))
lapply(cols_to_check, function(x) tail(df_recode[, x]))
# Fine up to here too


# But not when subsetting:
# Looks OK:
epi_head_and_tail(df_recode[df_recode$d_time_cuts_INGRESO == 'pre_T0', cols_to_check], cols = 6)
epi_head_and_tail(df_recode[df_recode$d_time_cuts_INGRESO == 'T0', cols_to_check], cols = 6)
epi_head_and_tail(df_recode[df_recode$d_time_cuts_INGRESO == 'T1', cols_to_check], cols = 6)

# Not OK:
epi_head_and_tail(df_recode[df_recode$d_time_cuts_DEF == 'pre_T0', cols_to_check], cols = 6)
epi_head_and_tail(df_recode[df_recode$d_time_cuts_DEF == 'T0', cols_to_check], cols = 6)
epi_head_and_tail(df_recode[df_recode$d_time_cuts_DEF == 'T1', cols_to_check], cols = 6)


# Not OK:
epi_head_and_tail(df_recode[df_recode$d_time_cuts_prev == 'pre_T0', cols_to_check], cols = 6)
epi_head_and_tail(df_recode[df_recode$d_time_cuts_prev == 'T0', cols_to_check], cols = 6)
epi_head_and_tail(df_recode[df_recode$d_time_cuts_prev == 'T1', cols_to_check], cols = 6)


# TO DO: needs checking, same issue as above:
df_recode[df_recode$d_time_cuts_DEF == 'pre_T0', ]
tail(df_recode[df_recode$d_time_cuts_DEF == 'pre_T0', ])

tail(df_recode[df_recode$d_time_cuts_DEF == 'pre_T0', ], n = 10)
# Last 7 rows are NAs
tail(df_recode[df_recode$d_time_cuts_DEF == 'T0', ], n = 10)
tail(df_recode[df_recode$d_time_cuts_DEF == 'T1', ], n = 10)
# Different IDs for those without NAs but last 7 rows in each period are NAs

# TO DO: check
# for (i in levels(df_recode$d_time_cuts_prev)) {
#     print(sprintf('Period: %s', i))
#     # 
#     print("everybody for the period:")
#     prev_cut <- nrow(df_recode[df_recode$d_time_cuts_prev == i, ])
#     print(prev_cut)
#     cat('\n')
#     # 
#     print("everybody who died in the period:")
#     outcome_Y <- nrow(df_recode[df_recode$d_time_cuts_DEF == i, ])
#     print(outcome_Y)
#     cat('\n')
#     # 
#     print("everybody who was alive in the period:")
#     outcome_N <- nrow(df_recode[df_recode$d_time_cuts_DEF == 'censored' &
#                                     df_recode$d_time_cuts_prev == i, ]
#                       )
#     print(outcome_N)
#     cat('\n')
#     # This should match:
#     print(prev_cut == outcome_Y + outcome_N)
#     
#     # 
#     print("everybody who died in the period should match with overall outcome (end of study follow-up):")
#     print(nrow(df_recode[df_recode$d_time_cuts_DEF == i & df_recode$d_death == 1, ]))
#     cat('\n')
#     # 
#     print("but should be different to those who were still alive at the particular period:")
#     print(nrow(df_recode[df_recode$d_time_cuts_DEF == 'censored' & df_recode$d_death == 0, ]))
#     cat('\n')
#     cat('\n')
# }
# 
# 
# 
# 
# summary(df_recode[df_recode$d_time_cuts_DEF == 'T0', ])
# dim(df_recode[df_recode$d_time_cuts_DEF == 'T0', ]) # equals number of people who died in this period
# dim(df_recode[df_recode$d_time_cuts_INGRESO == 'T0', ]) # equals number of people at risk (survivors and non-survivors) in this period
# 
# epi_head_and_tail(df_recode[, c('d_time_cuts_INGRESO','d_time_cuts_DEF')], cols = 2)
# summary(df_recode[, c('d_time_cuts_INGRESO','d_time_cuts_DEF')])
# table(df_recode$d_time_cuts_INGRESO, df_recode$d_time_cuts_DEF)
# # d_time_cuts_DEF includes those who were admitted before e.g. T0 start date as it's those who died during specified period.
# 
# # but need an outcome variable for each period for regression with 0's and 1's:
###
############
