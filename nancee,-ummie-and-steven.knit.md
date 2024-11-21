---
title: "research sprint"
output:
  pdf_document: default
  html_document: default
date: "2024-11-21"
editor_options:
  chunk_output_type: console
---



## R Markdown

## The deliverable
Create a new Rmd document in this repo that you can use to do your analysis and show the results. This document should have the following format:

* Research question (and explanation for why it matters)
* Methods and data, especially if using other data sources
* Analysis
* Discussion and conclusion

The final report should use both descriptive and inferential statistics, as well as at least two visualizations. Including at least one map is strongly encouraged but not essential.

You should be prepared to present your findings at our finals class--you'll have about 5 minutes to talk about your project and results. No need for slides--you can just knit your final document and walk us through it.

"Racial Distribution in Healthcare vs Agriculture Employment: A Statistical Analysis" author: "Data Analysis Report"

Primary Question: Is there a significant association between race and employment distribution in healthcare versus agriculture sectors?

Significance: Understanding racial representation in essential sectors informs equity policies
Healthcare and agriculture are vital sectors with different skill requirements and barriers to entry
Findings can help identify potential systematic barriers in these industries
Results can guide targeted workforce development programs
Methods and Data
Data Source
Using the Pulse Survey data, focusing on:
RRACE (Race categories)
SETTING (Industry sectors)
REGION (Geographic regions)
Assumption is that employment is normally distributed among all races.

``` r
files<-list.files("data",recursive = TRUE,full.names = TRUE,pattern="puf")

pulse<-map_df(files,read_csv)
```

```
## Rows: 70685 Columns: 243
## -- Column specification --------------------------------------------------------
## Delimiter: ","
## chr   (2): SCRAM, EST_ST
## dbl (241): WEEK, EST_MSA, REGION, HWEIGHT, PWEIGHT, TBIRTH_YEAR, ABIRTH_YEAR...
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 68504 Columns: 243
## -- Column specification --------------------------------------------------------
## Delimiter: ","
## chr   (2): SCRAM, EST_ST
## dbl (241): WEEK, EST_MSA, REGION, HWEIGHT, PWEIGHT, TBIRTH_YEAR, ABIRTH_YEAR...
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 75709 Columns: 243
## -- Column specification --------------------------------------------------------
## Delimiter: ","
## chr   (2): SCRAM, EST_ST
## dbl (241): WEEK, EST_MSA, REGION, HWEIGHT, PWEIGHT, TBIRTH_YEAR, ABIRTH_YEAR...
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 72738 Columns: 244
## -- Column specification --------------------------------------------------------
## Delimiter: ","
## chr   (2): SCRAM, EST_ST
## dbl (242): WEEK, EST_MSA, REGION, HWEIGHT, PWEIGHT, TBIRTH_YEAR, ABIRTH_YEAR...
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 61927 Columns: 244
## -- Column specification --------------------------------------------------------
## Delimiter: ","
## chr   (2): SCRAM, EST_ST
## dbl (242): WEEK, EST_MSA, REGION, HWEIGHT, PWEIGHT, TBIRTH_YEAR, ABIRTH_YEAR...
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 59290 Columns: 244
## -- Column specification --------------------------------------------------------
## Delimiter: ","
## chr   (2): SCRAM, EST_ST
## dbl (242): WEEK, EST_MSA, REGION, HWEIGHT, PWEIGHT, TBIRTH_YEAR, ABIRTH_YEAR...
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 64792 Columns: 221
## -- Column specification --------------------------------------------------------
## Delimiter: ","
## chr   (2): SCRAM, EST_ST
## dbl (219): WEEK, EST_MSA, REGION, HWEIGHT, PWEIGHT, TBIRTH_YEAR, ABIRTH_YEAR...
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 63802 Columns: 221
## -- Column specification --------------------------------------------------------
## Delimiter: ","
## chr   (2): SCRAM, EST_ST
## dbl (219): WEEK, EST_MSA, REGION, HWEIGHT, PWEIGHT, TBIRTH_YEAR, ABIRTH_YEAR...
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 68830 Columns: 221
## -- Column specification --------------------------------------------------------
## Delimiter: ","
## chr   (2): SCRAM, EST_ST
## dbl (219): WEEK, EST_MSA, REGION, HWEIGHT, PWEIGHT, TBIRTH_YEAR, ABIRTH_YEAR...
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 68454 Columns: 247
## -- Column specification --------------------------------------------------------
## Delimiter: ","
## chr   (2): SCRAM, EST_ST
## dbl (245): WEEK, EST_MSA, REGION, HWEIGHT, PWEIGHT, TBIRTH_YEAR, ABIRTH_YEAR...
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 72839 Columns: 247
## -- Column specification --------------------------------------------------------
## Delimiter: ","
## chr   (2): SCRAM, EST_ST
## dbl (245): WEEK, EST_MSA, REGION, HWEIGHT, PWEIGHT, TBIRTH_YEAR, ABIRTH_YEAR...
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 79371 Columns: 247
## -- Column specification --------------------------------------------------------
## Delimiter: ","
## chr   (2): SCRAM, EST_ST
## dbl (245): WEEK, EST_MSA, REGION, HWEIGHT, PWEIGHT, TBIRTH_YEAR, ABIRTH_YEAR...
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
data_subset <- pulse %>%
  select(RRACE, SETTING, REGION)
```



``` r
# Clean and transform data
analyzed_data <- data_subset %>%
  mutate(
    race = case_when(
      RRACE == 1 ~ "White",
      RRACE == 2 ~ "Black",
      RRACE == 3 ~ "Asian",
      RRACE == 4 ~ "Other",
      TRUE ~ "Not Specified"
    ),
    region = case_when(
      REGION == 1 ~ "Northeast",
      REGION == 2 ~ "South",
      REGION == 3 ~ "Midwest",
      REGION == 4 ~ "West",
      TRUE ~ "Not Specified"
    ),
    industry = case_when(
      SETTING == 1 ~ "Agriculture",
      SETTING == 17 ~ "Healthcare",
      TRUE ~ "Other"
    )
  ) %>%
  filter(industry != "Other")
```

## Including Plots

``` r
# Create industry distribution visualization
ggplot(analyzed_data, aes(x = race, fill = industry)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(
    title = "Distribution of Healthcare vs Agriculture Employment by Race",
    x = "Race",
    y = "Proportion",
    fill = "Industry"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )
```

![](nancee,-ummie-and-steven_files/figure-latex/unnamed-chunk-2-1.pdf)<!-- --> 


``` r
# Create regional distribution map
regional_data <- analyzed_data %>%
  group_by(region, industry) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(region) %>%
  mutate(proportion = count/sum(count))

ggplot(regional_data, aes(x = region, y = industry)) +
  geom_tile(aes(fill = proportion)) +
  scale_fill_viridis_c(labels = scales::percent) +
  theme_minimal() +
  labs(
    title = "Industry Distribution Across Regions",
    x = "Region",
    y = "Industry",
    fill = "Proportion"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )
```

![](nancee,-ummie-and-steven_files/figure-latex/unnamed-chunk-3-1.pdf)<!-- --> 


``` r
# Create contingency table
contingency_table <- table(analyzed_data$race, analyzed_data$industry)

# Perform chi-square test
chi_square_test <- chisq.test(contingency_table)

# Fisher's exact test (for small sample sizes)
fisher_test <- fisher.test(contingency_table, simulate.p.value = TRUE)

# Create results table
test_results <- data.frame(
  Test = c("Chi-square", "Fisher's Exact"),
  "Statistic" = c(chi_square_test$statistic, NA),
  "p-value" = c(chi_square_test$p.value, fisher_test$p.value)
)

kable(test_results, 
      caption = "Statistical Test Results") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
```


\begin{longtable}[t]{llrr}
\caption{\label{tab:unnamed-chunk-4}Statistical Test Results}\\
\toprule
 & Test & Statistic & p.value\\
\midrule
X-squared & Chi-square & 788.0606 & 0.0000000\\
 & Fisher's Exact & NA & 0.0004998\\
\bottomrule
\end{longtable}

``` r
# Calculate and display proportions
prop_table <- prop.table(contingency_table, margin = 1) * 100
kable(prop_table, 
      caption = "Industry Participation Rates by Race (%)") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
```


\begin{longtable}[t]{lrr}
\caption{\label{tab:unnamed-chunk-4}Industry Participation Rates by Race (%)}\\
\toprule
 & Agriculture & Healthcare\\
\midrule
Asian & 2.821870 & 97.17813\\
Black & 2.492212 & 97.50779\\
Other & 9.084821 & 90.91518\\
White & 10.153700 & 89.84630\\
\bottomrule
\end{longtable}


``` r
# Test normality of employment distribution
employment_counts <- analyzed_data %>%
  group_by(race, industry) %>%
  summarise(count = n(), .groups = 'drop')

shapiro_test <- shapiro.test(employment_counts$count)

# Create QQ plot
ggplot(employment_counts, aes(sample = count)) +
  stat_qq() +
  stat_qq_line() +
  theme_minimal() +
  labs(
    title = "Q-Q Plot of Employment Counts",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )
```

![](nancee,-ummie-and-steven_files/figure-latex/unnamed-chunk-5-1.pdf)<!-- --> 

You can also embed plots, for example:



Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
