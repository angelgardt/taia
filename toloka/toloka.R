library(tidyverse)

# load('toloka.RData')
# save.image('toloka.RData')

## directory content
dir()


### TOLOKA PREPROCESS ----- 

## import toloka data
toloka <- read_tsv("assignments_from_pool_23846483__05-05-2021.tsv")

str(toloka)
length(toloka$`OUTPUT:nickname`) ## 567
length(unique(toloka$`OUTPUT:nickname`)) ## 567

length(toloka$`ASSIGNMENT:worker_id`) ## 567
length(unique(toloka$`ASSIGNMENT:worker_id`)) ## 560

sum(toloka$`ASSIGNMENT:status` == "SUBMITTED") ## 500 ## great!

## subset submitted tasks
toloka %>% filter(`ASSIGNMENT:status` == 'SUBMITTED') -> toloka_submitted

toloka_submitted$`ASSIGNMENT:worker_id` %>% unique() %>% length() ## 500 ## great!

toloka_submitted %>% distinct(`OUTPUT:nickname`, `OUTPUT:gender`, `OUTPUT:age`) %>% nrow()
## 497...
## that's fuckup...

## so it means i need time column to merge toloka data with 1ka data

toloka_submitted$`OUTPUT:code` %>% unique() ## validation worked right

toloka_submitted %>% View

## frequencies by age, sex and nicknames
table(toloka$`OUTPUT:age`)
table(toloka$`OUTPUT:gender`) ## 268 males
sort(table(toloka$`OUTPUT:nickname`))


toloka_submitted %>% select(`OUTPUT:nickname`,
                  `OUTPUT:age`,
                  `OUTPUT:gender`,
                  `ASSIGNMENT:assignment_id`,
                  `ASSIGNMENT:worker_id`,
                  `ASSIGNMENT:started`,
                  `ASSIGNMENT:submitted`,
                  `ASSIGNMENT:reward`) %>% 
  rename(nickname = `OUTPUT:nickname`,
         age = `OUTPUT:age`,
         sex = `OUTPUT:gender`,
         a_id = `ASSIGNMENT:assignment_id`,
         w_id = `ASSIGNMENT:worker_id`,
         started = `ASSIGNMENT:started`,
         submitted = `ASSIGNMENT:submitted`,
         reward = `ASSIGNMENT:reward`) %>% 
  mutate(sex = ifelse(str_detect(sex, regex("м", ignore_case = TRUE)) |
                        str_detect(sex, regex("^M$")) |
                        str_detect(sex, regex("^male$")), "m", "f")) -> tlk_sbm
  # group_by(sex) %>% summarise(n = n()) ## check sex values

# sum(str_detect(tolower(toloka_submitted$`OUTPUT:gender`), 'м'))

tlk_sbm %>% distinct(nickname,
                     age,
                     sex,
                     started) %>% nrow()
## success!!!





### 1KA PREPROCESS ----- 

## import 1ka data
enka <- read_csv2("anketa340828-2021-05-05.csv")

## import aux items keys
aux_items <- readxl::read_xlsx("TAIA.xlsx", "aux_items")

str(enka)
View(enka)
nrow(enka)

str(aux_items)

## check NA
apply(sapply(enka, is.na), 2, sum)

## remove extra row and col
## add unique IDs
set.seed(115)
enka %>% 
  slice(-1) %>% 
  select(-X145) %>% 
  mutate(id = stringi::stri_rand_strings(505, 6)) -> enka

## select nickname, age, sex cols

enka %>% 
  select(id, nickname, age, sex) -> enka_toloka_linked

## select TAIA items
readxl::read_xlsx("TAIA.xlsx", 1) %>% 
  select(code) %>% 
  slice(-66) %>% 
  filter(code != "code" & !is.na(code)) %>% .[[1]] -> taia_cols

enka %>% 
  select(id, all_of(taia_cols)) -> taia



## fix enka_toloka_linked

enka_toloka_linked %>% 
  mutate_at(vars(nickname, age, sex),
            function(x) {
              str_replace_all(x, "[=\"]", "")
            }) %>% 
  mutate(age = as.numeric(age)) -> enka_toloka_linked


## fix TAIA dataset
taia %>%
  mutate_at(all_of(taia_cols),
            function(x) {
              as.numeric(unlist(str_extract_all(x, '[:digit:]')))
            }) -> taia



### check TAIA filling quality -----

taia %>% 
  select(id, all_of(taia_cols[66:80])) %>% 
  pivot_longer(all_of(taia_cols[66:80]),
               names_to = 'code', values_to = 'score') %>% 
  full_join(
    aux_items %>% 
      select(code, key)
  ) %>% 
  group_by(id, code) %>% 
  summarise(difference = abs(score - key)) -> aux_items_diff

## descriptive stats for differences between aux items scores and keys
psych::describe(aux_items_diff$difference)
## wow! nice results!

## sums for each participant

aux_items_diff %>% 
  summarise(n_miss = sum(difference != 0),
            p_miss = n_miss / 15,
            min = min(difference),
            max = max(difference),
            sum = sum(difference),
            p_sum = sum / 15) -> aux_items_stat

## descriptive stats for sums
psych::describe(aux_items_stat$sum)


## merging with toloka data

aux_items_stat %>% 
  full_join(enka_toloka_linked) %>% 
  full_join(tlk_sbm) -> toloka_quality


## subset low quality answers
## criterion is 'more than 5 misclicks in aux items'
toloka_quality %>% 
  filter(n_miss > 5) %>% 
  write_csv("non-rewarded_toloka.csv")

## check participants which had troubles
toloka_quality %>% 
  filter(nickname == 'gala' | nickname == 'Марго556')

toloka_quality %>% 
  filter(nickname == 'вано')
