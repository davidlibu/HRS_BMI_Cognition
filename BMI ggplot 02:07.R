library(tidyverse)

# Model 1b BMI Categories----
years <- rep(c("1992/1993", "1994/1995", "1996", "1998", "2000", "2002", "2004", "2006", "2008", "2010"), each = 4)
categories <- rep(c("Normal Weight (Ref.)", "Overweight", "Obese I", "Obese II"), times = 10)

hazard_ratios <- c(
  1.00, 0.85, 1.03, 1.52, # 1992/1993
  1.00, 0.92, 1.24, 1.51, # 1994/1995
  1.00, 0.95, 1.03, 1.85, # 1996
  1.00, 0.68, 0.96, 1.30, # 1998
  1.00, 0.74, 1.00, 1.05, # 2000
  1.00, 0.79, 1.02, 1.23, # 2002
  1.00, 0.71, 1.03, 1.12, # 2004
  1.00, 0.74, 0.86, 1.02, # 2006
  1.00, 0.72, 0.88, 0.93, # 2008
  1.00, 0.71, 0.78, 0.60  # 2010
)

significant <- c(
  FALSE, FALSE, FALSE, FALSE, # 1992/1993
  FALSE, FALSE, FALSE, FALSE, # 1994/1995
  FALSE, FALSE, FALSE, TRUE,  # 1996
  FALSE, TRUE, FALSE, FALSE,  # 1998
  FALSE, TRUE, FALSE, FALSE,  # 2000
  FALSE, FALSE, FALSE, FALSE, # 2002
  FALSE, TRUE, FALSE, FALSE,  # 2004
  FALSE, FALSE, FALSE, FALSE, # 2006
  FALSE, TRUE, FALSE, FALSE,  # 2008
  FALSE, TRUE, FALSE, TRUE    # 2010
)

data <- data.frame(Year = factor(years, levels = c("1992/1993", "1994/1995", "1996", "1998", "2000", "2002", "2004", "2006", "2008", "2010")),
                   Category = factor(categories, levels = c("Normal Weight (Ref.)", "Overweight", "Obese I", "Obese II")),
                   HazardRatio = hazard_ratios,
                   Significant = significant)

p2 <- ggplot(data, aes(x = Category, y = HazardRatio, group = Year, color = Year)) +
  geom_line(alpha = 0.4) +
  geom_point(size = 2) +
  scale_y_continuous(breaks = seq(0.5, 2.35, by = 0.2), limits = c(0.5, 1.95)) +
  geom_text(data = subset(data, Significant), aes(label = "*"), vjust = -0.5, size = 3, show.legend = FALSE) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "BMI Categories", y = "Hazard Ratio", title = "Hazard Ratios by BMI Categories Across Years", color = "Year") +
  scale_color_viridis_d() + # Using a different color palette
  geom_hline(yintercept = 1, color = "black", linetype = "dashed", alpha = 0.5) + 
  annotate("text", x = "Obese II", y = 0.98, label = "Reference Group: Normal Weight", color = "black", size = 2.0, hjust = -0.07) +
  annotate("text", x = 3.4, y = 0.60, label = "*p < 0.05 for difference", hjust = 1, vjust = 1, size = 2.5, color = "black")

print(p2)


#Faceted
p2 <- ggplot(data, aes(x = Category, y = HazardRatio)) +
  geom_line(aes(group = Year), position = position_dodge(0.8)) +
  geom_point(aes(color = Year), position = position_dodge(0.8), size = 2) +
  geom_text(data = subset(data, Significant), aes(label = "*"), vjust = -0.2, size = 4, position = position_dodge(0.8), show.legend = FALSE) +
  facet_wrap(~Year, scales = "free_x") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.background = element_blank()) +
  labs(x = "BMI Categories", y = "Hazard Ratio", title = "Hazard Ratios by BMI Categories Across Years") +
  scale_color_viridis_d() +
  geom_hline(yintercept = 1, color = "black", linetype = "dashed", alpha = 0.5) + 
  guides(color = guide_legend(title = "Year")) +
  scale_y_continuous(limits = c(0.5, 2.00))  # Set the y-axis limits to 0 to 2.3

p2 <- p2 + annotate("text", x = 3.75, y = 1.75, label = "*p < 0.05 for difference",
                    hjust = 1, vjust = 1, size = 1.5, color = "black")

print(p2)




# Model 2b BMI Categories----
years <- rep(c("1992/1993", "1994/1995", "1996", "1998", "2000", "2002", "2004", "2006", "2008", "2010"), each = 4)
categories <- rep(c("Normal Weight (Ref.)", "Overweight", "Obese I", "Obese II"), times = 10)

hazard_ratios <- c(
  1.00, 1.06, 1.22, 1.46, # 1992/1993
  1.00, 1.08, 1.17, 1.41, # 1994/1995
  1.00, 1.10, 1.17, 1.50, # 1996
  1.00, 0.95, 1.17, 1.29, # 1998
  1.00, 0.96, 1.14, 1.38, # 2000
  1.00, 1.08, 1.21, 1.42, # 2002
  1.00, 0.94, 1.19, 1.28, # 2004
  1.00, 0.95, 1.05, 1.23, # 2006
  1.00, 0.94, 0.98, 1.29, # 2008
  1.00, 0.95, 1.01, 1.12  # 2010
)

significant <- c(
  FALSE, FALSE, TRUE, TRUE,  # 1992/1993
  FALSE, FALSE, FALSE, TRUE,  # 1994/1995
  FALSE, FALSE, FALSE, TRUE, # 1996
  FALSE, FALSE, FALSE, FALSE, # 1998
  FALSE, FALSE, FALSE, TRUE, # 2000
  FALSE, FALSE, TRUE, TRUE,  # 2002
  FALSE, FALSE, TRUE, TRUE,  # 2004
  FALSE, FALSE, FALSE, FALSE, # 2006
  FALSE, FALSE, FALSE, TRUE,  # 2008
  FALSE, FALSE, FALSE, FALSE  # 2010
)

data <- data.frame(Year = factor(years, levels = c("1992/1993", "1994/1995", "1996", "1998", "2000", "2002", "2004", "2006", "2008", "2010")),
                   Category = factor(categories, levels = c("Normal Weight (Ref.)", "Overweight", "Obese I", "Obese II")),
                   HazardRatio = hazard_ratios,
                   Significant = significant)

p4 <- ggplot(data, aes(x = Category, y = HazardRatio, group = Year, color = Year)) +
  geom_line(alpha = 0.4) +
  geom_point(size = 2) +
  scale_y_continuous(breaks = seq(0.5, 2.35, by = 0.2), limits = c(0.8, 1.60)) +
  geom_text(data = subset(data, Significant), aes(label = "*"), vjust = -0.5, size = 3, show.legend = FALSE) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "BMI Categories", y = "Hazard Ratio", title = "Hazard Ratios by BMI Categories Across Years", color = "Year") +
  scale_color_viridis_d() +
  geom_hline(yintercept = 1, color = "black", linetype = "dashed", alpha = 0.5) + 
  annotate("text", x = "Obese II", y = 0.98, label = "Reference Group: Normal Weight", color = "black", size = 2.0, hjust = 0.5) +
  annotate("text", x = 3.4, y = 0.87, label = "*p < 0.05 for difference", hjust = 1, vjust = 1, size = 2.5, color = "black")

print(p4)

#Faceted
p4 <- ggplot(data, aes(x = Category, y = HazardRatio)) +
  geom_line(aes(group = Year), position = position_dodge(0.8)) +
  geom_point(aes(color = Year), position = position_dodge(0.8), size = 2) +
  geom_text(data = subset(data, Significant), aes(label = "*"), vjust = -0.2, size = 4, position = position_dodge(0.8), show.legend = FALSE) +
  facet_wrap(~Year, scales = "free_x") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.background = element_blank()) +
  labs(x = "BMI Categories", y = "Hazard Ratio", title = "Hazard Ratios by BMI Categories Across Years") +
  scale_color_viridis_d() +
  geom_hline(yintercept = 1, color = "black", linetype = "dashed", alpha = 0.5) + 
  guides(color = guide_legend(title = "Year")) +
  scale_y_continuous(limits = c(0.80, 1.55))  # Set the y-axis limits to 0 to 2.3

p4 <- p4 + annotate("text", x = 4.00, y = 1.1, label = "*p < 0.05 for difference",
                    hjust = 3.4, vjust = 1, size = 1.5, color = "black")

print(p4)




# Model 5b Dementia Non Smokers Cat. ----
years <- rep(c("1992/1993", "1994/1995", "1996", "1998", "2000", "2002", "2004", "2006", "2008", "2010"), each = 4)
categories <- rep(c("Normal Weight (Ref.)", "Overweight", "Obese I", "Obese II"), times = 10)

hazard_ratios <- c(
  1.00, 0.68, 0.87, 1.26, # 1992/1993
  1.00, 0.68, 1.42, 1.18, # 1994/1995
  1.00, 1.05, 1.00, 2.45, # 1996
  1.00, 0.65, 0.95, 1.24, # 1998
  1.00, 0.78, 0.86, 1.05, # 2000
  1.00, 0.87, 0.76, 1.31, # 2002
  1.00, 0.70, 1.04, 0.89, # 2004
  1.00, 0.66, 0.86, 0.98, # 2006
  1.00, 0.61, 0.64, 0.91, # 2008
  1.00, 0.62, 0.62, 0.62  # 2010
)

significant <- c(
  FALSE, FALSE, FALSE, FALSE, # 1992/1993
  FALSE, FALSE, FALSE, FALSE, # 1994/1995
  FALSE, FALSE, FALSE, TRUE,  # 1996
  FALSE, FALSE, FALSE, FALSE, # 1998
  FALSE, FALSE, FALSE, FALSE, # 2000
  FALSE, FALSE, FALSE, FALSE, # 2002
  FALSE, FALSE, FALSE, FALSE, # 2004
  FALSE, FALSE, FALSE, FALSE, # 2006
  FALSE, TRUE, FALSE, FALSE,  # 2008
  FALSE, TRUE, FALSE, FALSE   # 2010
)

data <- data.frame(Year = factor(years, levels = c("1992/1993", "1994/1995", "1996", "1998", "2000", "2002", "2004", "2006", "2008", "2010")),
                   Category = factor(categories, levels = c("Normal Weight (Ref.)", "Overweight", "Obese I", "Obese II")),
                   HazardRatio = hazard_ratios,
                   Significant = significant)

p8 <- ggplot(data, aes(x = Category, y = HazardRatio, group = Year, color = Year)) +
  geom_line(alpha = 0.4) +
  geom_point(size = 2) +
  scale_y_continuous(breaks = seq(0.35, 2.61, by = 0.2), limits = c(0.35, 2.60)) +
  geom_text(data = subset(data, Significant), aes(label = "*"), vjust = -0.5, size = 3, show.legend = FALSE) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "BMI Categories", y = "Hazard Ratio", title = "Hazard Ratios by BMI Categories Across Years for Non Smokers", color = "Year") +
  scale_color_viridis_d() +
  geom_hline(yintercept = 1, color = "black", linetype = "dashed", alpha = 0.5) + 
  annotate("text", x = "Obese II", y = 0.97, label = "Ref: Normal Weight", color = "black", size = 2.0, hjust = -0.26) +
  annotate("text", x = 3.4, y = 1.60, label = "*p < 0.05 for difference", hjust = 1, vjust = 1, size = 2.5, color = "black")

print(p8)

#Faceted
p8 <- ggplot(data, aes(x = Category, y = HazardRatio)) +
  geom_line(aes(group = Year), position = position_dodge(0.8)) +
  geom_point(aes(color = Year), position = position_dodge(0.8), size = 2) +
  geom_text(data = subset(data, Significant), aes(label = "*"), vjust = -0.2, size = 4, position = position_dodge(0.8), show.legend = FALSE) +
  facet_wrap(~Year, scales = "free_x") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.background = element_blank()) +
  labs(x = "BMI Categories", y = "Hazard Ratio", title = "Hazard Ratios by BMI Categories Across Years for Non Smokers") +
  scale_color_viridis_d() +
  geom_hline(yintercept = 1, color = "black", linetype = "dashed", alpha = 0.5) + 
  guides(color = guide_legend(title = "Year")) +
  scale_y_continuous(limits = c(0.37, 2.60))  # Set the y-axis limits to 0 to 2.3

p8 <- p8 + annotate("text", x = 3.00, y = 2.0, label = "*p < 0.05 for difference",
                    hjust = 1, vjust = 1, size = 1.5, color = "black")

print(p8)






















# Model 6b Dementia CIND NON Smokers Cat. ----
years <- rep(c("1992/1993", "1994/1995", "1996", "1998", "2000", "2002", "2004", "2006", "2008", "2010"), each = 4)
categories <- rep(c("Normal Weight (Ref.)", "Overweight", "Obese I", "Obese II"), times = 10)

hazard_ratios <- c(
  1.00, 1.02, 1.20, 1.64, # 1992/1993
  1.00, 1.07, 1.23, 1.44, # 1994/1995
  1.00, 1.21, 1.16, 1.81, # 1996
  1.00, 0.94, 1.15, 1.24, # 1998
  1.00, 0.90, 1.08, 1.38, # 2000
  1.00, 1.06, 1.10, 1.37, # 2002
  1.00, 0.92, 1.16, 1.26, # 2004
  1.00, 0.93, 1.02, 1.10, # 2006
  1.00, 0.99, 0.93, 1.22, # 2008
  1.00, 0.90, 0.97, 1.05  # 2010
)

significant <- c(
  FALSE, FALSE, FALSE, TRUE,  # 1992/1993
  FALSE, FALSE, FALSE, FALSE, # 1994/1995
  FALSE, FALSE, FALSE, TRUE,  # 1996
  FALSE, FALSE, FALSE, FALSE, # 1998
  FALSE, FALSE, FALSE, FALSE, # 2000
  FALSE, FALSE, FALSE, FALSE, # 2002
  FALSE, FALSE, FALSE, FALSE, # 2004
  FALSE, FALSE, FALSE, FALSE, # 2006
  FALSE, FALSE, FALSE, FALSE, # 2008
  FALSE, FALSE, FALSE, FALSE  # 2010
)

data <- data.frame(Year = factor(years, levels = c("1992/1993", "1994/1995", "1996", "1998", "2000", "2002", "2004", "2006", "2008", "2010")),
                   Category = factor(categories, levels = c("Normal Weight (Ref.)", "Overweight", "Obese I", "Obese II")),
                   HazardRatio = hazard_ratios,
                   Significant = significant)


p10 <- ggplot(data, aes(x = Category, y = HazardRatio, group = Year, color = Year)) +
  geom_line(alpha = 0.4) +
  geom_point(size = 2) +
  scale_y_continuous(breaks = seq(0.80, 2.90, by = 0.2), limits = c(0.80, 1.85)) +
  geom_text(data = subset(data, Significant), aes(label = "*"), vjust = -0.5, size = 3, show.legend = FALSE) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "BMI Categories", y = "Hazard Ratio", title = "Hazard Ratios by BMI Categories Across Years for Non Smokers", color = "Year") +
  scale_color_viridis_d() +
  geom_hline(yintercept = 1, color = "black", linetype = "dashed", alpha = 0.5) + 
  annotate("text", x = "Obese II", y = 0.98, label = "Ref: Normal Weight", color = "black", size = 2.0, hjust = -0.05) +
  annotate("text", x = 3.4, y = 1.50, label = "*p < 0.05 for difference", hjust = 1, vjust = 1, size = 2.5, color = "black")

print(p10)

#Faceted
p10 <- ggplot(data, aes(x = Category, y = HazardRatio)) +
  geom_line(aes(group = Year), position = position_dodge(0.8)) +
  geom_point(aes(color = Year), position = position_dodge(0.8), size = 2) +
  geom_text(data = subset(data, Significant), aes(label = "*"), vjust = -0.2, size = 4, position = position_dodge(0.8), show.legend = FALSE) +
  facet_wrap(~Year, scales = "free_x") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.background = element_blank()) +
  labs(x = "BMI Categories", y = "Hazard Ratio", title = "Hazard Ratios by BMI Categories Across Years for Non Smokers") +
  scale_color_viridis_d() +
  geom_hline(yintercept = 1, color = "black", linetype = "dashed", alpha = 0.5) + 
  guides(color = guide_legend(title = "Year")) +
  scale_y_continuous(limits = c(0.80, 1.90))  # Set the y-axis limits to 0 to 2.3

p10 <- p10 + annotate("text", x = 3.00, y = 1.50, label = "*p < 0.05 for difference",
                      hjust = 1, vjust = 1, size = 1.5, color = "black")

print(p10)

# Model 7b Dementia Current/Former Smokers Cat ----
years <- rep(c("1992/1993", "1994/1995", "1996", "1998", "2000", "2002", "2004", "2006", "2008", "2010"), each = 4)
categories <- rep(c("Normal Weight (Ref.)", "Overweight", "Obese I", "Obese II"), times = 10)

hazard_ratios <- c(
  1.00, 1.04, 1.31, 1.76, # 1992/1993
  1.00, 1.17, 1.11, 1.85, # 1994/1995
  1.00, 0.92, 1.12, 1.58, # 1996
  1.00, 0.73, 1.02, 1.51, # 1998
  1.00, 0.73, 1.19, 1.10, # 2000
  1.00, 0.75, 1.30, 1.16, # 2002
  1.00, 0.73, 1.07, 1.28, # 2004
  1.00, 0.83, 0.90, 1.08, # 2006
  1.00, 0.84, 1.19, 0.96, # 2008
  1.00, 0.85, 0.98, 0.57  # 2010
)


significant <- rep(FALSE, length(hazard_ratios))

data <- data.frame(Year = factor(years, levels = c("1992/1993", "1994/1995", "1996", "1998", "2000", "2002", "2004", "2006", "2008", "2010")),
                   Category = factor(categories, levels = c("Normal Weight (Ref.)", "Overweight", "Obese I", "Obese II")),
                   HazardRatio = hazard_ratios,
                   Significant = significant)

p4 <- ggplot(data, aes(x = Category, y = HazardRatio, group = Year, color = Year)) +
  geom_line(alpha = 0.4) +
  geom_point(size = 2) +
  scale_y_continuous(breaks = seq(0.5, 2.35, by = 0.2), limits = c(0.50, 1.90)) +
  geom_text(data = subset(data, Significant), aes(label = "*"), vjust = -0.5, size = 3, show.legend = FALSE) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "BMI Categories", y = "Hazard Ratio", title = "Hazard Ratios by BMI Categories Across Years", color = "Year") +
  scale_color_viridis_d() +
  geom_hline(yintercept = 1, color = "black", linetype = "dashed", alpha = 0.5) + 
  annotate("text", x = "Obese II", y = 0.98, label = "Reference Group: Normal Weight", color = "black", size = 2.0, hjust = -0.1) +
  annotate("text", x = 3.4, y = 1.70, label = "*p < 0.05 for difference", hjust = 1, vjust = 1, size = 2.5, color = "black")

print(p4)

#Faceted
p4 <- ggplot(data, aes(x = Category, y = HazardRatio)) +
  geom_line(aes(group = Year), position = position_dodge(0.8)) +
  geom_point(aes(color = Year), position = position_dodge(0.8), size = 2) +
  geom_text(data = subset(data, Significant), aes(label = "*"), vjust = -0.2, size = 4, position = position_dodge(0.8), show.legend = FALSE) +
  facet_wrap(~Year, scales = "free_x") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.background = element_blank()) +
  labs(x = "BMI Categories", y = "Hazard Ratio", title = "Hazard Ratios by BMI Categories Across Years") +
  scale_color_viridis_d() +
  geom_hline(yintercept = 1, color = "black", linetype = "dashed", alpha = 0.5) + 
  guides(color = guide_legend(title = "Year")) +
  scale_y_continuous(limits = c(0.40, 1.90))  # Set the y-axis limits to 0 to 2.3

p4 <- p4 + annotate("text", x = 4.00, y = 1.7, label = "*p < 0.05 for difference",
                    hjust = 3.4, vjust = 1, size = 1.5, color = "black")

print(p4)
# Model 8b Dementia/CIND Current/Former Smokers Cat----
years <- rep(c("1992/1993", "1994/1995", "1996", "1998", "2000", "2002", "2004", "2006", "2008", "2010"), each = 4)
categories <- rep(c("Normal Weight (Ref.)", "Overweight", "Obese I", "Obese II"), times = 10)

hazard_ratios <- c(
  1.00, 1.09, 1.24, 1.34, # 1992/1993
  1.00, 1.09, 1.11, 1.44, # 1994/1995
  1.00, 1.04, 1.19, 1.37, # 1996
  1.00, 0.97, 1.20, 1.39, # 1998
  1.00, 1.01, 1.20, 1.42, # 2000
  1.00, 1.12, 1.31, 1.49, # 2002
  1.00, 0.95, 1.21, 1.32, # 2004
  1.00, 0.96, 1.08, 1.34, # 2006
  1.00, 0.90, 1.03, 1.35, # 2008
  1.00, 0.99, 1.06, 1.20  # 2010
)

significant <- c(
  FALSE, FALSE, FALSE, FALSE,  # 1992/1993
  FALSE, FALSE, FALSE, TRUE,   # 1994/1995
  FALSE, FALSE, FALSE, FALSE,  # 1996
  FALSE, FALSE, FALSE, TRUE,   # 1998
  FALSE, FALSE, FALSE, TRUE,   # 2000
  FALSE, FALSE, TRUE,  TRUE,   # 2002
  FALSE, FALSE, FALSE,  TRUE,   # 2004
  FALSE, FALSE, FALSE,  TRUE,   # 2006
  FALSE, FALSE, FALSE, TRUE,   # 2008
  FALSE, FALSE, FALSE, FALSE   # 2010
)

data <- data.frame(Year = factor(years, levels = c("1992/1993", "1994/1995", "1996", "1998", "2000", "2002", "2004", "2006", "2008", "2010")),
                   Category = factor(categories, levels = c("Normal Weight (Ref.)", "Overweight", "Obese I", "Obese II")),
                   HazardRatio = hazard_ratios,
                   Significant = significant)

p14 <- ggplot(data, aes(x = Category, y = HazardRatio, group = Year, color = Year)) +
  geom_line(alpha = 0.4) +
  geom_point(size = 2) +
  scale_y_continuous(breaks = seq(0.70, 1.75, by = 0.2), limits = c(0.80, 1.50)) +
  geom_text(data = subset(data, Significant), aes(label = "*"), vjust = -0.5, size = 3, show.legend = FALSE) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "BMI Categories", y = "Hazard Ratio", title = "Hazard Ratios by BMI Categories Across Years for Current and Former Smokers", color = "Year") +
  scale_color_viridis_d() +
  geom_hline(yintercept = 1, color = "black", linetype = "dashed", alpha = 0.5) + 
  annotate("text", x = "Obese II", y = 0.98, label = "Reference Group: Normal Weight", color = "black", size = 2.0, hjust = -0.07) +
  annotate("text", x = 3.4, y = 1.50, label = "*p < 0.05 for difference", hjust = 1, vjust = 1, size = 2.5, color = "black")

print(p14)

#Faceted
p14 <- ggplot(data, aes(x = Category, y = HazardRatio)) +
  geom_line(aes(group = Year), position = position_dodge(0.8)) +
  geom_point(aes(color = Year), position = position_dodge(0.8), size = 2) +
  geom_text(data = subset(data, Significant), aes(label = "*"), vjust = -0.2, size = 4, position = position_dodge(0.8), show.legend = FALSE) +
  facet_wrap(~Year, scales = "free_x") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.background = element_blank()) +
  labs(x = "BMI Categories", y = "Hazard Ratio", title = "Hazard Ratios by BMI Categories Across Years for Current and Former Smokers") +
  scale_color_viridis_d() +
  geom_hline(yintercept = 1, color = "black", linetype = "dashed", alpha = 0.5) + 
  guides(color = guide_legend(title = "Year")) +
  scale_y_continuous(limits = c(0.80, 1.50))  

p14 <- p14 + annotate("text", x = 3.00, y = 1.40, label = "*p < 0.05 for difference",
                      hjust = 1, vjust = 1, size = 1.5, color = "black")

print(p14)

#Figure 2 -----
#redo code
data <- data.frame(
  Model = rep(c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6"), each = 8),
  Category = rep(c("Normal Weight (Ref)", "Overweight", "Obese I", "Obese II"), times = 12),
  OutcomeType = rep(c("Max BMI with Age at Max BMI", "Max BMI"), each = 4, times = 6),
  HazardRatio = c(1.00, 0.82, 0.94, 1.28, 1.00, 0.77, 0.82, 1.10, # Model 1b
                  1.00, 1.18, 1.51, 1.72, 1.00, 1.07, 1.26, 1.40, # Model 2b
                  1.00, 0.67, 0.74, 1.19, 1.00, 0.61, 0.64, 0.98, # Model 1b_ns
                  1.00, 1.16, 1.46, 1.68, 1.00, 1.04, 1.23, 1.34, # Model 2b_ns
                  1.00, 1.06, 1.24, 1.50, 1.00, 1.00, 1.10, 1.32, # Model 1b_s
                  1.00, 1.19, 1.57, 1.79, 1.00, 1.10, 1.31, 1.47  # Model 2b_s
  ),
  Significant = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                  FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE,
                  FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE,
                  FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE,
                  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                  FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE)
)

data$Significant <- as.logical(data$Significant) 
data$Category <- factor(data$Category, levels = c("Normal Weight (Ref)", "Overweight", "Obese I", "Obese II"))


data$Asterisk <- ifelse(data$Significant, "*", "") 

p <- ggplot(data, aes(x = Category, y = HazardRatio, group = OutcomeType)) +
  geom_line(aes(color = OutcomeType)) +
  geom_point(aes(color = OutcomeType)) + 
  geom_text(aes(label = Asterisk, vjust = 0.02), color = "black") + 
  facet_wrap(~Model, scales = "free_y") +
  labs(title = "Hazard Ratios Across Models for BMI Categories",
       x = "BMI Category",
       y = "Hazard Ratio",
       color = "Hazard Ratio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") + scale_y_continuous(limits = c(0.60, 1.80))

print(p)

p <- ggplot(data, aes(x = Category, y = HazardRatio, group = OutcomeType, color = OutcomeType)) +
  geom_line() +
  geom_point() + 
  geom_text(aes(label = Asterisk, vjust = 0.02), color = "black") + 
  facet_wrap(~Model, scales = "free_y") +
  labs(title = "Hazard Ratios Across Models for BMI Categories",
       x = "BMI Category",
       y = "Hazard Ratio",
       color = "Hazard Ratio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        strip.text = element_text(face = "bold")) + 
  scale_y_continuous(limits = c(0.60, 1.80))

print(p)



#Figure 3 -----
#redo code
data <- data.frame(
  Model = rep(c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6"), each = 8),
  Category = rep(c("Normal Weight (Ref)", "Overweight", "Obese I", "Obese II"), times = 12),
  OutcomeType = rep(c("Max BMI with Age at Max BMI", "2010 BMI"), each = 4, times = 6),
  HazardRatio = c(1.00, 0.82, 0.94, 1.28, 1.00, 0.71, 0.78, 0.60, # Model 1b
                  1.00, 1.18, 1.51, 1.72, 1.00, 0.94, 1.01, 1.12, # Model 2b
                  1.00, 0.67, 0.74, 1.19, 1.00, 0.62, 0.62, 0.62, # Model 1b_ns
                  1.00, 1.16, 1.46, 1.68, 1.00, 0.90, 0.97, 1.05, # Model 2b_ns
                  1.00, 1.06, 1.24, 1.50, 1.00, 0.85, 0.98, 0.57, # Model 1b_s
                  1.00, 1.19, 1.57, 1.79, 1.00, 0.99, 1.06, 1.20  # Model 2b_s
  ),
  Significant = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE,
                  FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE,
                  FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE,
                  FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE,
                  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                  FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
)

data$Significant <- as.logical(data$Significant) 
data$Category <- factor(data$Category, levels = c("Normal Weight (Ref)", "Overweight", "Obese I", "Obese II"))


data$Asterisk <- ifelse(data$Significant, "*", "") 


p <- ggplot(data, aes(x = Category, y = HazardRatio, group = OutcomeType, color = OutcomeType)) +
  geom_line() +
  geom_point() + 
  geom_text(aes(label = Asterisk, vjust = 0.02), color = "black") + 
  facet_wrap(~Model, scales = "free_y") +
  labs(title = "Hazard Ratios Across Models for BMI Categories",
       x = "BMI Category",
       y = "Hazard Ratio",
       color = "Hazard Ratio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        strip.text = element_text(face = "bold")) + 
  scale_y_continuous(limits = c(0.55, 1.80))

print(p)

p <- ggplot(data, aes(x = Category, y = HazardRatio, group = OutcomeType, color = OutcomeType)) +
  geom_line() +
  geom_point() + 
  geom_text(aes(label = Asterisk, vjust = 0.02), color = "black") + 
  facet_wrap(~Model, scales = "free_y") +
  labs(title = "Hazard Ratios Across Models for BMI Categories",
       x = "BMI Category",
       y = "Hazard Ratio",
       color = "Hazard Ratio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        strip.text = element_text(face = "bold")) + 
  scale_y_continuous(limits = c(0.55, 1.80), breaks = seq(0.50, 1.80, by = 0.20))

print(p)


