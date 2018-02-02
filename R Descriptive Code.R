############################# Data #############################

### Libraries

#Data
library(tidyverse)

#Descriptive Analysis
install.packages("devtools")
library(devtools)
install_github("dnzmarcio/ntimes")
library(ntimes)

#Plots
install.packages("ggthemes")
#https://cran.r-project.org/web/packages/ggthemes/vignettes/ggthemes.html
library(ggthemes)

install.packages("cowplot")
library(cowplot)
install.packages("patchwork")
library(patchwork)

### Reading dataset

emergency <- read_csv("emergency.csv")

### Recoding variables

dataset <- emergency %>% mutate(infection =
                                 factor(infection,
                                        levels = 0:1,
                                        labels = c("No", "Yes")),
                               gender = 
                                 factor(gender,
                                        levels = 0:1,
                                        labels = c("Female", "Male")),
                               child =
                                 factor(child,
                                        levels = 0:2,
                                        labels = c("A", "B", "C")),
                               encephalopathy =
                                 factor(encephalopathy,
                                        levels = 0:1,
                                        labels = c("No", "Yes")),
                               ascites =
                                 factor(ascites,
                                        levels = 0:1,
                                        labels = c("No", "Yes")),
                               death = 
                                 factor(death,
                                        levels = 0:1,
                                        labels = c("No", "Yes")))


# Just another option of recoding variables separately

emergency$infection <- factor(emergency$infection, levels = 0:1, labels = c('No', 'Yes'))
emergency$gender <- factor(emergency$gender, levels = 0:1, labels = c('Female', 'Male'))
emergency$child <- factor(emergency$child, levels = 0:2, labels = c("A", "B", "C"))
emergency$encephalopathy <- factor(emergency$encephalopathy, levels = 0:1, labels = c('No', 'Yes'))
emergency$ascites <- factor(emergency$ascites, levels = 0:1, labels = c('No', 'Yes'))
emergency$death <- factor(dataset$death, levels = 0:1, labels = c('No', 'Yes'))

############################# Descriptive Measures

### Location Measures

# mean
mean(dataset$age)

# mean with missing values
mean(dataset$cpr)
mean(dataset$cpr, na.rm = TRUE)

# Median
median(dataset$age)
median(dataset$cpr, na.rm = TRUE)

### Dispersion Measures

# Standard Deviation
sd(dataset$age)

# Standard Deviation with missing values
sd(dataset$cpr)
sd(dataset$cpr, na.rm = TRUE)

# Quantiles
quantile(dataset$age)
quantile(dataset$age, probs = c(0.25, 0.5))

# Quantiles with missing values
quantile(dataset$cpr, probs = c(0.25, 0.5), na.rm = TRUE)

# Minimum
min(dataset$age)
min(dataset$cpr, na.rm = TRUE)

# Maximum
max(dataset$age)
max(dataset$cpr, na.rm = TRUE)

### Calculating some summaries

# Quantitative variables

dataset %>%
  summarise(mean_age = mean(age), sd_age = sd(age))

dataset %>%
  group_by(infection) %>%
  summarise(mean_age = mean(age), sd_age = sd(age))

dataset %>% select(-id) %>% summarize_if(is.numeric, funs(mean))

dataset %>% select(-id) %>% 
  summarize_if(is.numeric, funs(mean), na.rm = TRUE)

dataset %>% select(-id) %>% group_by(infection) %>%
  summarize_if(is.numeric, funs(mean), na.rm = TRUE)

dataset %>% select(-id) %>% group_by(infection) %>%
  summarize_if(is.numeric, funs(mean, sd), na.rm = TRUE) %>%
  gather(key = variable, value = value, -infection) %>%
  separate(variable, into = c("variable", "measure"), sep = "_") %>%
  spread(key = measure, value = value) %>%
  mutate(mean = round(mean, 2), sd = round(sd, 2))

tab <- dataset %>% select(-id) %>% group_by(infection) %>%
  summarize_if(is.numeric, funs(mean, sd), na.rm = TRUE) %>%
  gather(key = variable, value = value, -infection) %>%
  separate(variable, into = c("variable", "measure"), sep = "_") %>%
  spread(key = measure, value = value) %>%
  mutate(mean = round(mean, 2), sd = round(sd, 2))

# Qualitative variables

dataset %>% group_by(infection) %>% summarize(n = n())

dataset %>% group_by(infection, death, child) %>% summarize(n = n())

dataset %>% select_if(is.factor) %>% 
  gather(key = variable, value = group, -infection) %>%
  group_by(infection, variable, group) %>%
  summarise (n = n())

dataset %>% select_if(is.factor) %>% 
  gather(key = variable, value = group, -infection) %>%
  group_by(infection, variable, group) %>%
  summarise (n = n())  %>%
  mutate(perc = 100*n / sum(n)) %>%
  mutate(perc = round(perc, 2))

tab <- dataset %>% select_if(is.factor) %>% 
  gather(key = variable, value = group, -infection) %>%
  group_by(infection, variable, group) %>%
  summarise (n = n())  %>%
  mutate(perc = 100*n / sum(n)) %>%
  mutate(perc = round(perc, 2))

# Creating a table

dataset <- emergency %>% mutate(age = qt_var(age, 
                                             unit = "years",
                                             label = "Age"),
                                infection =
                                  ql_var(infection,
                                         from = 0:1,
                                         to = c("No", "Yes"),
                                         label = "Infection"),
                                gender = 
                                  ql_var(gender,
                                         from = 0:1,
                                         to = c("Female", "Male"),
                                         label = "Infection"),
                                child =
                                  ql_var(child,
                                         from = 0:2,
                                         to = c("A", "B", "C"),
                                         label = "Child"),
                                encephalopathy =
                                  ql_var(encephalopathy,
                                         from = 0:1,
                                         to = c("No", "Yes"),
                                         label = "Encephalopathy"),
                                ascites =
                                  ql_var(ascites,
                                         from = 0:1,
                                         to = c("No", "Yes"),
                                         label = "Ascites"),
                                death = 
                                  ql_var(death,
                                         from = 0:1,
                                         to = c("No", "Yes"),
                                         label = "Death"))

dataset %>% select(-id) %>% nt_describe(group = infection)

dataset %>% select(-id) %>% 
  nt_describe(group = infection, measure = "mean.sd")

############################# Plots
# Visit http://ggplot2.org/

### Dotplot

data_plot <- dataset %>% select(infection, age)

# dotplot using qplot
qplot(x = age, data = data_plot, geom = 'dotplot', method = 'histodot')


# Basic grid
gp <- ggplot(data_plot, aes(x = age))

# Adding a dotplot
gp <- ggplot(data_plot, aes(x = age, y = NA)) +
  geom_dotplot(method = "histodot")

# Decreasing the size of the dots
gp <- ggplot(data_plot, aes(x = age)) +
  geom_dotplot(binwidth = 1, method = "histodot")

# Choosing a theme
gp <- ggplot(data_plot, aes(x = age)) +
  geom_dotplot(binwidth = 1, method = "histodot") +
  theme_bw()

gp <- ggplot(data_plot, aes(x = age)) +
  geom_dotplot(binwidth = 1, method = "histodot") +
  theme_minimal()

gp <- ggplot(data_plot, aes(x = age)) +
  geom_dotplot(binwidth = 1, method = "histodot") +
  theme_classic()

gp <- ggplot(data_plot, aes(x = age)) +
  geom_dotplot(binwidth = 1, method = "histodot") +
  theme_fivethirtyeight()

gp <- ggplot(data_plot, aes(x = age)) +
  geom_dotplot(binwidth = 1, method = "histodot") +
  theme_wsj()

gp <- ggplot(data_plot, aes(x = age)) +
  geom_dotplot(binwidth = 1, method = "histodot") +
  theme_economist()

gp <- ggplot(data_plot, aes(x = age)) +
  geom_dotplot(binwidth = 1, method = "histodot") +
  theme_excel()

# Axis y does not make sense in a dotplot. It should be blank
gp <- ggplot(data_plot, aes(x = age)) +
  geom_dotplot(binwidth = 1, method = "histodot") +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


# Filling the dots with different colours as a function of the groups
gp <- ggplot(data_plot, aes(x = age, fill = infection)) +
  geom_dotplot(stackgroups = TRUE, binwidth = 1, method = "histodot")  +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# Changing the x axis title and legend title
gp <- ggplot(data_plot, aes(x = age, fill = factor(infection))) +
  geom_dotplot(stackgroups = TRUE, binwidth = 1, method = "histodot")  +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_discrete("Infection") +
  labs(x = "Age")

# Change colours
gp <- ggplot(data_plot, aes(x = age, fill = infection)) +
  geom_dotplot(stackgroups = TRUE, binwidth = 1, method = "histodot")  +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_manual("Infection", values = c("red", "blue")) +
  labs(x = "Age")

gp <- ggplot(data_plot, aes(x = age, fill = infection)) +
  geom_dotplot(stackgroups = TRUE, binwidth = 1, method = "histodot")  +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x = "Age")

# Using different panels
gp <- ggplot(data_plot, aes(x = age, fill = infection)) +
  geom_dotplot(stackgroups = TRUE, binwidth = 1, method = "histodot")  +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_brewer("Infection", palette = "Set1") +
  labs(x = "Age") +
  facet_grid(. ~ infection)

gp <- ggplot(data_plot, aes(x = age, fill = infection)) +
  geom_dotplot(stackgroups = TRUE, binwidth = 1, method = "histodot")  +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_brewer("Infection", palette = "Set1") +
  labs(x = "Age") +
  facet_grid(infection ~ .)

# Removing legend
gp <- ggplot(data_plot, aes(x = age, fill = infection)) +
  geom_dotplot(stackgroups = TRUE, binwidth = 1, method = "histodot")  +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  scale_fill_brewer("Infection", palette = "Set1") +
  labs(x = "Age") +
  facet_grid(. ~ infection)

# A different perspective
data_plot <- dataset %>% select(infection, age)
gp01 <- ggplot(data_plot, aes(y = age, x = infection, fill = infection)) +
  geom_dotplot(binaxis = "y", stackdir = "center",
               binwidth = 1, method = "histodot")  +
  stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median,
               geom = "crossbar", width = 0.5) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_fill_brewer("infection", palette = "Set1") +
  labs(y = "Age", x = "Infection")


### Histogram - age

data_plot <- dataset %>% select(infection, age)

# qplot using histogram of age
qplot(x = age, data = data_plot)

# Basic grid
gp <- ggplot(data_plot, aes(x = age))

# Adding a histogram
gp <- ggplot(data_plot, aes(x = age)) +
  geom_histogram()

# Changing the width of the bin
gp <- ggplot(data_plot, aes(x = age)) +
  geom_histogram(binwidth = 5)

# Choosing a theme
gp <- ggplot(data_plot, aes(x = age)) +
  geom_histogram(binwidth = 5) +
  theme_bw()

# A histogram can have two types of y-axis: Frequency and Density
gp <- ggplot(data_plot, aes(x = age, y = ..density..)) +
  geom_histogram(binwidth = 5) +
  theme_bw()

# If it is a density histogram then we have different widths for 
# each interval
breaks <- c(15, seq(30, 85, by = 5))
gp <- ggplot(data_plot, aes(x = age, y = ..density..)) +
  geom_histogram(breaks = breaks) +
  theme_bw()

# Creating histograms for each group
gp <- ggplot(data_plot, aes(x = age, y = ..density.., fill = infection)) +
  geom_histogram(binwidth = 5) +
  theme_bw()

# Changing the axis titles and legend title
gp <- ggplot(data_plot, aes(x = age, y = ..density.., fill = infection)) +
  geom_histogram(binwidth = 5)  +
  theme_bw() +
  scale_fill_discrete("infection") +
  labs(x = "Age", y = "Density")

# Change colours
gp <- ggplot(data_plot, aes(x = age, y = ..density.., fill = infection)) +
  geom_histogram(binwidth = 5)  +
  theme_bw() +
  scale_fill_brewer("infection", palette = "Set1") +
  labs(x = "Age", y = "Density")

# Using different panels
gp <- ggplot(data_plot, aes(x = age, y = ..density.., fill = infection)) +
  geom_histogram(binwidth = 5)  +
  theme_bw() +
  scale_fill_brewer("infection", palette = "Set1") +
  labs(x = "Age", y = "Density") +
  facet_grid(infection ~ .)

# Removing legend
gp02 <- ggplot(data_plot, aes(x = age, y = ..density.., fill = infection)) +
  geom_histogram(binwidth = 5)  +
  theme_bw() +
  scale_fill_brewer("infection", palette = "Set1") +
  labs(x = "Age", y = "Density") +
  facet_grid(infection ~ ., labeller = label_both) +
  theme(legend.position = "none")

### Boxplot - age

data_plot <- dataset %>% select(infection, age)

# qplot boxplot of age 
qplot(y = age, x = infection, geom = 'boxplot', data = data_plot)

# Basic grid
gp <- ggplot(data_plot, aes(y = age))

# Adding a boxplot
gp <- ggplot(data_plot, aes(y = age, x = NA)) +
  geom_boxplot()

# Adding a boxplot
gp <- ggplot(data_plot, aes(y = age, x = NA)) +
  stat_boxplot(geom = "errorbar", width = 0.1) +
  geom_boxplot()

# Choosing a theme
gp <- ggplot(data_plot, aes(y = age, x = NA)) +
  stat_boxplot(geom = "errorbar", width = 0.1) +
  geom_boxplot() +
  theme_bw()

# The x axis does not make sense for a boxplot
gp <- ggplot(data_plot, aes(y = age, x = NA)) +
  stat_boxplot(geom = "errorbar", width = 0.1) +
  geom_boxplot() +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# Filling the boxplot with colour
gp <- ggplot(data_plot, aes(y = age, x = NA)) +
  stat_boxplot(geom = "errorbar", width = 0.1) +
  geom_boxplot(fill = "grey80") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# Creating boxplots for each group
gp <- ggplot(data_plot, aes(y = age, x = infection, fill = infection)) +
  stat_boxplot(geom = "errorbar", width = 0.1) +
  geom_boxplot() +
  theme_bw()

# Changing the axis titles and legend title
gp <- ggplot(data_plot, aes(y = age, x = infection, fill = infection)) +
  stat_boxplot(geom = "errorbar", width = 0.1) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_discrete(infection) +
  labs(x = "Infection", y = "Age")

# Change colours
gp <- ggplot(data_plot, aes(y = age, x = infection, fill = infection)) +
  stat_boxplot(geom = "errorbar", width = 0.1) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_brewer("infection", palette = "Set1") +
  labs(x = "Infection", y = "Age")

# Removing the legend
gp <- ggplot(data_plot, aes(y = age, x = infection, fill = infection)) +
  stat_boxplot(geom = "errorbar", width = 0.1) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_brewer("infection", palette = "Set1") +
  labs(x = "Infection", y = "Age") +
  theme(legend.position = "none")

# Adding points
gp <- ggplot(data_plot, aes(y = age, x = infection, fill = infection)) +
  stat_boxplot(geom = "errorbar", width = 0.1) +
  geom_boxplot() +
  geom_point(position = position_jitter(width = 0.2), colour = "purple") +
  theme_bw() +
  scale_fill_brewer("infection", palette = "Set1") +
  labs(x = "Infection", y = "Age") +
  theme(legend.position = "none")

# Removing outliers

gp03 <- ggplot(data_plot, aes(y = age, x = infection, fill = infection)) +
  stat_boxplot(geom = "errorbar", width = 0.1) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitter(width = 0.4)) +
  theme_bw() +
  scale_fill_brewer("Infection", palette = "Set1") +
  labs(x = "infection", y = "age") +
  theme(legend.position = "none")

### Barplots - age

data_plot <- dataset %>% select(infection, age) %>% group_by(infection) %>%
  summarize(mean = mean(age), sd = sd(age))

gp <- ggplot(data_plot, aes(x = infection, y = mean, fill = infection)) +
  geom_errorbar(aes(ymin = mean, ymax = mean + sd), width = .1) +
  geom_bar(position = position_dodge(), stat = "identity") +
  scale_fill_brewer("infection", palette = "Set1") +
  theme_bw() + labs(x = "infection", y = "age") +
  theme(legend.position = "none")

### Barplot - child
data_plot <- dataset %>% select(infection, child)

# qplot barplot of child
qplot(x = child, data = data_plot, geom = 'bar')

data_plot <- na.omit(data_plot) %>% count(child = child, 
                                          infection = infection) %>% 
  mutate(pct = round(prop.table(n) * 100, 2))

# Basic grid
gp <- ggplot(data_plot, aes(x = child, y = n))

# Adding bars
gp <- ggplot(data_plot, aes(x = child, y = n)) + geom_bar(stat = 'identity')

# Choosing a theme
gp <- ggplot(data_plot, aes(x = child, y = n)) + 
  geom_bar(stat = 'identity') + theme_bw()

# Change y-axis label
gp <- ggplot(data_plot, aes(x = child, y = n)) + 
  geom_bar(stat = 'identity') + theme_bw() +
  labs(y = "Frequency")

# Creating barplots for each group
gp <- ggplot(data_plot, aes(x = child, y = n, fill = infection)) +
  geom_bar(stat = 'identity') + theme_bw() +
  labs(y = "Frequency")

# Changing the position of the bars
gp <- ggplot(data_plot, aes(x = child, y = n, fill = infection)) +
  geom_bar(stat = 'identity', position = position_dodge(width = .9)) + 
  theme_bw() +
  labs(y = "Frequency")

# Changing colours
gp <- ggplot(na.omit(data_plot), aes(x = child, y = n, fill = infection)) +
  geom_bar(stat = 'identity', position = position_dodge(width = .9)) + 
  theme_bw() +
  labs(y = "Frequency") +
  scale_fill_brewer("Infection", palette = "Set1")

# Changing from frequency to percentage
gp <- ggplot(na.omit(data_plot), aes(x = child, y = pct, fill = infection)) +
  geom_bar(stat = 'identity', position = position_dodge(width = .9)) + 
  theme_bw() +
  labs(y = "%", x = "Child") + scale_y_continuous(limits = c(0, 100)) +
  scale_fill_brewer("Infection", palette = "Set1")

# Changing the scale
gp <- ggplot(na.omit(data_plot), aes(x = child, y = pct, fill = infection)) +
  geom_bar(stat = 'identity', position = position_dodge(width = .9)) + 
  theme_bw() +
  labs(y = "%", x = "Child") + scale_y_continuous(limits = c(0, 100)) +
  scale_fill_brewer("Infection", palette = "Set1")

# Adding text on the top of the columns
gp <- ggplot(data_plot, aes(x = child, y = pct, fill = infection)) + 
  geom_bar(stat = 'identity', position = position_dodge(width = .9)) + 
  labs(y = "%", x = "Child") + scale_y_continuous(limits = c(0, 100)) +
  scale_fill_brewer("Infection", palette = "Set1") +
  theme_bw() + 
  geom_text(aes(y = pct + 1,    # nudge above top of bar
                label = paste0(round(pct, 2), '%')),    
            position = position_dodge(width = .9), 
            size = 3)

# Mix between Frequency and Percentage
gp04 <- ggplot(data_plot, aes(x = child, y = n, fill = infection)) + 
  geom_bar(stat = 'identity', position = position_dodge(width = .9)) + 
  labs(y = "Frequency", x = "Child") + 
  scale_fill_brewer("Infection", palette = "Set1") +
  theme_bw() + 
  geom_text(aes(y = n + 1,    # nudge above top of bar
                label = paste0(round(pct, 2), '%')),    
            position = position_dodge(width = .9), 
            size = 3)

# Arranging the plots
plot_grid(gp01, gp02, gp03, gp04, 
          labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2)



