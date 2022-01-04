#############################################################################
# Code to recreate figures in McMenamin M, Bond H, Sullivan S, Cowling B. 
# Estimation of Relative Vaccine Effectiveness in Influenza: A Systematic 
# Review of Methodology. J Epidemiol. 2022 (In Press)
# 
# Author: Martina McMenamin
# Date: 2022-01-03
#############################################################################

library('ggplot2')
library('ggExtra')
library('ggpubr')
library('gridExtra')
library('viridis')
library('incidence')
library('forestplot')
library('cowplot')



# Manuscript --------------------------------------------------------------

## Figure 3: VE reported by design


Design <- c("Retrospective \ncohort","Randomised \ncontrolled trial", 
            "Systematic review/ \nmeta-analysis", "Test-negative \ndesign", 
            "Case-control", "Prospective cohort")
Design <- 
  factor(
  Design, 
  levels = 
    rev( 
      c("Retrospective \ncohort","Randomised \ncontrolled trial", 
        "Systematic review/ \nmeta-analysis", "Test-negative \ndesign", 
        "Case-control", "Prospective cohort")
      )
  )

Yes <- c(1, 8, 4, 5, 0, 0) 
No <- c(24, 14, 3, 1, 2, 1)
rep <- c(No, Yes)
group <- c(rep("No",6),rep("Yes",6))
group <- 
  factor(
  group, 
  levels = 
    rev(
      c("No","Yes")
      )
  )

VEreport <- data.frame(Design, rep, group)

Fig3 <-
  ggplot(
    VEreport, 
    aes(x=rep, y=Design, fill=group)
    ) +
  geom_bar(
    stat="identity"
    ) +
  scale_fill_manual(
    values = c("#FDE725FF","#440154FF")
    ) +
  theme_bw() +
  ylab("") +
  xlab("Number of studies") +
  labs(fill = "Absolute \nVE reported") +
  theme(
    text = element_text(size=6)
    ) +
  guides(
    fill = guide_legend(override.aes = list(size = 4))
    )

save_plot("Figure3.pdf", Fig3, base_width = 3.34, base_height = 1.8)


## Figure 4: Bias assessment

### 4A: ROBINS-I

Domain <- 
  c(
    rep("Confounding",5), rep("Selection",5), rep("Classification of intervention", 5),
    rep( "Deviation from intervention",5), rep("Missing data",5),
    rep("Measurement of outcomes",5), rep("Reporting",5)
    )

Domain <- 
  factor(
    Domain, 
    levels = 
      rev(
        c("Confounding", "Selection", "Classification of intervention", 
          "Deviation from intervention", "Missing data",
          "Measurement of outcomes", "Reporting")
        )
    )

Assessment <- rep(
  c("Low", "Moderate", "No Information", "Serious", "Critical"), 7
  )
Assessment <- factor(Assessment,levels = rev(c("Low", "Moderate", "No Information", "Serious", "Critical")))

Proportion <- c(0, 33, 0, 1, 0,
                25, 9, 0, 0, 0,
                32, 2, 0, 0, 0,
                34, 0, 0, 0, 0,
                7, 13, 9, 5, 0,
                23, 10, 0, 1, 0,
                29, 5, 0, 0, 0
                )

biasdata <- data.frame(Domain, Assessment, Proportion)

Fig4A <- 
  ggplot(
    biasdata, 
    aes(x = Domain, y=Proportion)
    ) +
  geom_col(
    aes(fill = Assessment), position = "fill", colour="black", size = 0.2
    ) +
  coord_flip() + 
  scale_fill_manual(
    values=c("#820000", "#BF0000","lemonchiffon1","#E2DF07","#02C100")
    ) +
  theme_minimal() + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        text = element_text(size = 6),
        panel.grid.major = element_blank(),
        axis.line.x = element_line(colour = "black", size = 0.3, linetype = "solid"),
        axis.ticks.y=element_blank(),
        legend.key.size = unit(3, 'mm'), 
        legend.key.height = unit(3, 'mm'), 
        legend.key.width = unit(3, 'mm'), 
        legend.text = element_text(size=4)
  ) +
  scale_y_continuous(
    labels = c("0.00" = "0%", "0.25" = "25%", "0.50" = "50%", "0.75" = "75%", "1.00" = "100%")
    ) +
  labs(fill = "") + 
  ylab("Proportion of studies") + 
  xlab("") 



### 4B: ROB2

Domainrob2 <- c(rep("Randomisation",3), 
                rep( "Deviation from intervention",3), 
                rep("Missing data",3), 
                rep("Measurement of outcomes",3), 
                rep("Reporting",3)
                )
Domainrob2 <- 
  factor(
    Domainrob2, 
    levels = 
      rev(
        c("Randomisation", "Deviation from intervention", "Missing data", 
          "Measurement of outcomes", "Reporting"
          )
        )
    )

Assessmentrob2 <- rep(c("Low", "Some Concerns", "High"), 5)
Assessmentrob2 <- 
  factor(
    Assessmentrob2,
    levels = rev(
      c("Low", "Some Concerns", "High")
      )
    )

Proportionrob2 <- c(13, 6, 0,
                    11, 7, 1,
                    15, 3, 1,
                    14, 2, 3,
                    18, 1, 0)

rob2data <- data.frame(Domainrob2,Assessmentrob2,Proportionrob2)

Fig4B <- ggplot(
  rob2data, aes(x = Domainrob2, y=Proportionrob2)
  ) +
  geom_col(
    aes(fill = Assessmentrob2), position = "fill", colour="black", size = 0.2
    ) +
  coord_flip() + 
  theme_minimal() + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        text = element_text(size = 6),
        panel.grid.major = element_blank(),
        axis.line.x = element_line(colour = "black", size = 0.3, linetype = "solid"),
        axis.ticks.y=element_blank(),
        legend.key.size = unit(3, 'mm'), 
        legend.key.height = unit(3, 'mm'), 
        legend.key.width = unit(3, 'mm'), 
        legend.text = element_text(size=4)
        ) +
  scale_y_continuous(
    labels=c("0.00" = "0%", "0.25" = "25%", "0.50" = "50%", "0.75" = "75%", "1.00" = "100%")
    ) +
  labs(fill="") + 
  ylab("Proportion of studies") +  
  xlab("") + 
  scale_fill_manual(
    values = c("#BF0000", "#E2DF07", "#02C100")
    ) 


# Plot Figures 4 A and B on one grid

ggarrange(
  Fig4A, Fig4B,
  ncol = 1,
  heights = c(1.2,1), 
  labels = c("A", "B")
  )

Fig4 <- plot_grid(Fig4A, Fig4B, ncol = 1, rel_heights = c(1.2, 1), labels = "AUTO")
save_plot("Figure4.pdf", Fig4, base_width = 3.34, base_height = 4.5)



# Figure 5: Proportionality


# HD vs SD

y <- c(24.0, 32.0 )
x <- c(29.0, 6.0)
r <- c(7.0, 27.0 )


# cell vs egg

y <- c(31.7, -2, 46 )
x <- c(20.1, 14, 53 )
r <- c(8.0, 15.0, 0 )


# LAIV vs IIV

y <- c(40, 59.0, 26, 59, 36, 57)
x <- c(51.9, 49.9, 51, 60, 68, 77 )   
r <- c(-46.2, 4.2, -166, -9 )


# MF59 adjuv vs non adjuv

y <- c(86 )
x <- c(43 )
r <- c(75 )


ytot <- c(24.0, 32.0, 31.7, -2, 46, 40, 59.0, 26, 59, 36, 57, 86)
ymintot <- c(5.0, -3, 18.7, -20, 33, 25.2, 47.8, 15, 50, 0, -3, 74)
ymaxtot <- c(39.0, 54.0, 42.6, 14.0, 56, 51.9, 67.9, 36.0, 66, 59, 82, 93)

xtot <- c(29.0, 6.0, 20.1, 14, 53, 51.9, 49.9, 51, 60, 68, 77, 43)
xmintot <- c(10.0, -42, 14.5, -31, 45, 42.0, 39.2, 47, 52, 46, 37, 15)
xmaxtot <- c(44.0, 38, 25.4, 43, 60, 60.1, 58.7, 54, 67, 81, 92, 61)

Comparison <- c(rep("HD",2), rep("cell",3), rep("LAIV", 6), "Adjuv")

rtot <- c(7.0, 27.0, 8.0, 15.0, 0, -46.2, 4.2, -166, -9, -50, -25, 75)

abline <- abline(lm(ytot ~ xtot))
dataprop <- data.frame(xtot,ytot,ymintot, ymaxtot, xmintot, xmaxtot, Comparison)  

Fig5 <- 
  ggplot(
  data = dataprop, aes(x=xtot, y=ytot, color=Comparison)
  ) +
  geom_abline(
    aes(intercept = 18.1511, slope = 0.5295), color = "grey", size = 0.3
  ) +
  geom_point(
    size=0.8
    ) +
  geom_errorbar(
    aes(ymin = ymintot, ymax = ymaxtot), alpha = 0.7, size = 0.3
    ) +
  geom_errorbarh(
    aes(xmin = xmintot, xmax = xmaxtot), alpha = 0.7, size = 0.3
    ) +
  scale_color_viridis(
    discrete=T, labels = c("Adj vs Non-Adj", "Cell vs Egg", "HD vs SD ", "LAIV vs IIV ")
    ) +
  theme_bw()+
  ylim(c(-50,100)) +
  xlim(c(-50,100)) +
  ylab("Absolute VE of vaccine A (%)") +
  xlab("Absolute VE of vaccine B (%)") + 
  theme(
    text = element_text(size=6),
    legend.key.size = unit(3, 'mm'), 
    legend.key.height = unit(3, 'mm'), 
    legend.key.width = unit(3, 'mm'), 
    legend.text = element_text(size=5)
  )

save_plot("Figure5.pdf", Fig5, base_width = 3.34, base_height = 2.05)






# Supplementary Figures ---------------------------------------------------



# Fig S1: Publication year of included studies 

 year.freq <- c(rep("2021",6), rep("2020",14), rep("2019",11), rep("2018",3), 
                rep("2017",3), rep("2016",3), rep("2015",4), rep("2014",6), 
                rep("2013",2), rep("2012",1), rep("2011",2), rep("2010",1), 
                rep("2009",2), "2008", "2007", rep("2006",3))

 year.inc <- as.Date(year.freq, "%Y")
 
 inc.years <- incidence(year.inc, "years")
 
 FigS1 <- 
   plot(
   inc.years, alpha=1, border="white", color="#39568CFF"
   ) +
   theme_bw() + 
   removeGridX() + 
   xlab("Publication year") +
   ylab("Frequency") +
   scale_x_date(
     limits = as.Date(c("2006-01-01", "2022-01-01")), 
     breaks = as.Date(c("2006-01-01", "2011-01-01","2016-01-01","2021-01-01")),
     labels=c("2006", "2011", "2016", "2021")
     )
 



# Fig S2: Variables controlled for in analysis

Vars <- c("Age", "Comorbidities", "Region", "Sex", "Ethnicity", "Healthcare", "Calendar time",
          "Previous \nvaccination", "Time of \nvaccination", "Not reported")
Vars <- 
  factor(
  Vars, 
  levels = 
    rev(
      c("Age", "Comorbidities", "Region", "Sex", "Ethnicity", "Healthcare", "Calendar time",
        "Previous \nvaccination", "Time of \nvaccination", "Not reported")
      )
  )

Number <- c(41, 29, 29, 25, 16, 13, 13, 11, 8, 18)
Prop <- Number/63
Covar <- c(rep("T", 9),"N")

ggadj <- data.frame(Prop, Vars, Covar)

FigS2 <-
  ggplot(
  ggadj, aes(x=Prop, y=Vars, fill=Covar)
  ) +
  geom_col() +
  ylab("") +
  xlab("Proportion of studies (%)") + 
  theme_bw() +
  scale_fill_manual(
    values = c("#56B1F7", "#132B43")
    ) +
  theme(legend.position = "none")

 

#Fig S3: Forest plots for comparisons within infleunza
 

## A: High-dose vs Standard dose influenza

cochrane_from_rmeta <- 
  structure(list(
    mean = c(NA, 6.8, 
             9, 8, 5, -2,
             21.8, 14.8, 16.9, 17.2, # 2011/12, 13/13, 13/14, 14/15
             29, 11, 21,  
             24, 27, 
             23.2, 15.4, 9.0, 5.4, 12.8, 4.9, #2012/13, 13/14, 14/15, 15/16, 16/17, 17/18
             11, 16, 10, 14, 18,
             25, 12.7, 2, 22, 35.4, 12.6),
    lower = c(NA, 3.3, 
              -92, -46, -45, -52,
              -5.9, 9.3, 9.2, 14.5, # 2011/12, 13/13, 13/14, 14/15
              21, -1, 11,
              -46, -8,
              17.7, 7.8, 5.7, -3.8, 6.5, -1.4, #2012/13, 13/14, 14/15, 15/16, 16/17, 17/18
              -2, -5, -3, -13, 4, 
              2, 1.8, -40.0, 15, 12.5, -140.5 ),
    upper = c(NA, 10.1, 
              57, 42, 38, 32,
              42.3, 19.9, 23.9, 19.9, # 2011/12, 13/13, 13/14, 14/15
              36, 22, 30, 
              61, 50,
              28.3,  22.3, 12.1, 13.7, 18.6, 10.8, #2012/13, 13/14, 14/15, 15/16, 16/17, 17/18
              22, 33, 21, 34, 30, 
              43, 22.4, 32.0, 29, 52.5, 65.8)),
    .Names = c("mean", "lower", "upper"), 
    row.names = c( -4L), 
    class = "data.frame")

tabletext <- cbind(
  c("Study", "Izurieta et al. (2020)","Balasubramani et al. (2020)",
    "", "", "", "Paudel et al. (2020)","","","","Young-Xu et al. (2020)","", "",
    "Doyle et al. (2020)", "", "Lu et al. (2019)",
    "","","","","","Young-Xu et al. (2019)", "", "", "", "",  "Young-Xu et al. (2018)",
    "Gravenstein et al. (2017)",  "Richardson et al. (2015)", "Izurieta et al. (2015)", 
    "DiazGranados et al. (2014)", "DiazGranados et al. (2013)"),
 
   c("Season", "2019/20", "2015/16", "2016/17", "2017/18", "2018/19", 
    "2011/12", "2012/13", "2013/14", "2014/15", "2012/13", "2013/14", "2014/15",
    "2015/16", "2016/17", "2012/13", "2013/14", "2014/15", "2015/16", 
    "2016/17", "2017/18", "2010/11", "2011/12", "2012/13", "2013/14", 
    "2014/15", "2015/16", "2012/13", "2010/11", "2012/13",
    "2011-13", "2009/10"),
  
  c("Outcome","H-I","MA","MA", "MA", "MA", "ILI", "ILI", "ILI", "ILI", "M-IP", 
    "M-IP", "M-IP", "H-I", "H-I", "H-IP", "H-I", "H-I", "H-I", "H-I", "H-I", "H-I", 
    "H-I", "H-I", "H-I", "H-I", "H-IP", "H-IP", "H-IP", 
    "ILI", "LAB",  "MM-I"),
 
   c("rVE(%) (95% CI)","6.8 (3.3, 10.1)","9.0 (-92.0, 57.0)",
    "8.0 (-46.0, 42.0)", "5.0 (-45.0, 38.0)", "-2.0 (-52.0, 32.0)", "21.8 (-5.9, 42.3)",
    "14.8 (9.3, 19.9)", "16.9 (9.2, 23.9)","17.2 (14.5, 19.9)","29.0 (21.0, 36.0)",
    "11.0 (-1.0, 22.0)","21.0 (11.0, 30.0)", "24.0 (-46.0, 61.0)","27.0 (-8.0, 50.0)",
    "23.2 (17.7, 28.3)","15.4 (7.8, 22.3)","9.0 (5.7, 12.1)", 
    "5.4 (-3.8, 13.7)","12.8 (6.5, 18.6)","4.9 (-1.4, 10.8)","11.0 (-2.0, 22.0)", 
    "16.0 (-5.0, 33.0)", "10.0 (-3.0, 21.0)", "14.0 (-13.0, 34.0)","18.0 (4.0, 30.0)",
    "25.0 (2.0, 43.0)", "12.7 (1.8, 22.4)",
    "2.0 (-40.0, 32.0)", "22.0 (15.0, 29.0)", "35.4 (12.5, 52.5)", "12.6 (-140.5, 65.8)"))


forest_dose <- 
  grid.grabExpr(
    print(
      forestplot(tabletext, 
                 cochrane_from_rmeta,new_page = TRUE,
                 is.summary = c(TRUE,rep(FALSE,36)),
                 boxsize = .07, 
                 zero = 0,
                 title = "High dose vs. standard dose",
                 hrzl_lines=list("2" = gpar(lty = 1)),
                 xticks = c(-100, -80, -60, -40, -20, 0, 20, 40, 60, 80, 100),
                 clip = c(-100, 100),
                 graphwidth = "auto",
                 graph.pos=3,
                 align = c("l", "l", "l", "l"),
                 txt_gp = fpTxtGp(ticks=gpar(cex=0.75), 
                                  xlab=gpar(cex=1), 
                                  legend=gpar(cex=1), 
                                  label=gpar(cex=0.8)
                                  ),
                 xlab = "Relative vaccine effectiveness (%)",
                 col = fpColors(box = "black",
                                line = "black",
                                summary = "black")
                 )
      )
    )



## B: Component comparisons - Adjuvanted vs non-adjuvanted


cochrane_from_rmeta_adj <- 
  structure(list(
    mean = c(NA, 2.0,  8.2, 25.0, 3.0, -13.0, -12.0, 39.0, 3.6, -0.7, 29.4,  76.8, 12.1, 25.0, 75.0 ),
    lower = c(NA, -3.7, 4.2, 17.0, 0.0, -32.0, -21.0, 4.0, 0.7, -19.8, 7.6, 18.5, -3.4, 2, 55.0 ),
    upper = c(NA, 7.3, 12.0, 32.2, 6.1, 6.4, -2.1, 61.0, 6.4, 15.4, 46.0, 93.4, 25.3, 43, 87.0 )),
    .Names = c("mean", "lower", "upper"), 
    row.names = c( -4L), 
    class = "data.frame")

tabletext_adj <- cbind(
  c("Study", "Pelton et al. (2021)", "Izurieta et al. (2020)", "Pelton et al. (2020)", "Izurieta et al. (2020)", 
    "van Aalst et al. (2020)","", "Lapi et al. (2019)", "Izurieta et al. (2019)", 
    "Vesikari et al. (2018)", "van Essen et al. (2014)", "Nolan et al. (2014)", 
    "McElhaney et al. (2013)", "Mannino et al. (2012)", "Vesikari et al. (2011)"),
  
  c("Season", "2018/19", "2019/20", "2017/18", "2018/19", "2016/17", "2017/18", "2001-17", 
    "2017/18", "2013-15", "2008/09", "2010/11", "2008-10","2006-09", "2007-09"),
  
  
  c("Outcome", "H-I", "H-I", "H-IP/MA", "H-I", "H-IP", "H-IP", "H-IC", "H-I", "LAB", "LAB", 
    "LAB", "LAB", "H-IP", "LAB"),
  
  c("rVE(%) (95% CI)", "2.0 (-3.7, 7.3)", "8.2 (4.2, 12.0)", "25.0 (17.0, 32.2)", "3.0 (0.0, 6.1)", 
    "-13.0 (-32.0, 6.4)", "-12.0 (-21.0, -2.1)", "39.0 (4.0, 61.0)", "3.6 (0.7, 6.4)", 
    "-0.7 (-19.8, 15.4)", "29.4 (7.6, 46.0)","76.8 (18.5, 93.4)", "12.1 (-3.4, 25.3)", 
    "25.0 (2.0, 43.0)", "75.0 (55.0, 87.0)"))

forest_adjuv <- 
  grid.grabExpr(
    print(
      forestplot(tabletext_adj, 
                 cochrane_from_rmeta_adj,new_page = TRUE,
                 is.summary = c(TRUE, rep(FALSE,14)),
                 boxsize = .07, 
                 zero = 0,
                 title = "Adjuvanted vs. non-adjuvanted",
                 xticks = c(-100, -80, -60, -40, -20, 0, 20, 40, 60, 80, 100),
                 clip = c(-80, 100),
                 graphwidth = "auto",
                 graph.pos = 3,
                 align = c("l", "l", "l", "l"),
                 hrzl_lines = list("2" = gpar(lty = 1)), 
                 txt_gp = fpTxtGp(
                   ticks = gpar(cex=0.75), 
                   xlab=gpar(cex=1), 
                   legend=gpar(cex=1), 
                   label=gpar(cex=0.8)
                   ),
           xlab = "Relative vaccine effectiveness (%)",
           col = fpColors(box = "black",
                          line = "black",
                          summary = "black")
           )
      )
    )




## C: cell-based vs egg-based 


cochrane_from_rmeta_cell <- 
  structure(list(
    mean = c(NA, 13.4, 7.6, 14.4, 36.2, 8.0, 0.8, 12.0, 0, 10, 30 ),
    lower = c(NA, 11.4, 6.5, 8.8, 26.1, -10.0, -4.6, -40, -30, 7, 10 ),
    upper = c(NA, 15.4, 8.6,  19.6, 44.9, 23.0, 5.9, 45, 20, 13, 47)),
    .Names = c("mean", "lower", "upper"), 
    row.names = c( -4L), 
    class = "data.frame")

tabletext_cell <- cbind(
  c("Study","Boikos et al. (2021)", "Boikos et al. (2021)", "Divino et al. (2020)", "Boikos et al. (2020)", 
    "Klein et al. (2020)", "Izurieta et al. (2020)", "Bruxvoort et al. (2019)", 
    "DeMarcus et al. (2019)", "Izurieta et al. (2019)", "Dunkle et al. (2017)"),
  
  c("Season", "2018/19", "2018/19", "2017/18", "2017/18", "2017/18", "2018/19", "2017/18", 
    "2017/18", "2017/18", "2014/15"),
  
  c("Outcome", "MA-HR", "MA", "H-IP", "ILI", "LAB", "H-IP", "H-IP", "LAB", "H-I", "LAB"), 
  
  c("rVE(%) (95% CI)", "13.4 (11.4, 15.4)", "7.6 (6.5, 8.6)", "14.4 (8.8, 19.6)","36.2 (26.1, 44.9)", 
    "8.0 (-10.0, 23.0)", "0.8 (-4.6, 5.9)", "15.0 (-26.0, 43.0)", "0.0 (-30.0, 20.0)", 
    "10.0 (7.0, 13.0)", "30.0 (10.0, 47.0)"))

forest_cell <- 
  grid.grabExpr(
    print(
      forestplot(
        tabletext_cell, 
        cochrane_from_rmeta_cell,new_page = TRUE,
        is.summary = c(TRUE, rep(FALSE,10)),
        boxsize = .07, 
        zero = 0,
        title = "cell-based vs. egg-based",
        xticks = c(-100, -80, -60, -40, -20, 0, 20, 40, 60, 80, 100),
        clip = c(-100, 100),
        graphwidth = "auto",
        graph.pos = 3,
        align = c("l", "l", "l", "l"),
        hrzl_lines = list("2" = gpar(lty = 1)), 
        txt_gp = 
          fpTxtGp(
            ticks = gpar(cex = 0.75), 
            xlab = gpar(cex = 1), 
            legend = gpar(cex = 1), 
            label = gpar(cex = 0.8)
            ),
           xlab = "Relative vaccine effectiveness (%)",
           col = fpColors(
             box = "black",
             line = "black",
             summary = "black")
        )
      )
    )


FigS3 <- grid.arrange(
  forest_dose, forest_adjuv, forest_cell, heights=c(2,1,0.8)
  )


