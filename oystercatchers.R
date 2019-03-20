# Badly formatted script for oystercatcher analysis
# Try the following:
# 1. Add additional comments to make the code clearer, then commit and
#    push to github
# 2. Move the multiplot function into a separate R file and source(<file>)
#    near the start of the script. Commit and push to gitlab
# 3. Neaten up the indentation of p1, p2 and p2. Commit and push to gitlab.


library(readr)
OC <- read_tsv("data/OystercatcherData.txt")
summary(OC)

# Set the Month, FeedingType and FeedingPlot as factors ####
OC$Month <- as.factor(OC$Month)
OC$FeedingType <- as.factor(OC$FeedingType)
OC$FeedingPlot <- as.factor(OC$FeedingPlot)
summary(OC)

library(ggplot2)
p1 <- ggplot() +
  geom_boxplot(data=OC, aes(x = FeedingType, y = ShellLength)) +
        xlab("Feeding type") +
ylab("Shell length") +
                     theme_classic()
p1
p2 <- ggplot() +
  geom_boxplot(data=OC, aes(x = Month, y = ShellLength)) +
  xlab("Month") +
              ylab("Shell length") +
theme_classic()
p3 <- ggplot() +
  geom_boxplot(data=OC, aes(x = FeedingPlot, y = ShellLength)) +
                                             xlab("Feeding plot") +
  ylab("Shell length") +
  theme_classic()

# Multiple plot function ####
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(p1, p2, p3, cols=2)

table(OC$Month)
table(OC$FeedingPlot)
table(OC$FeedingType)

M1 <- lm(ShellLength ~ FeedingType * FeedingPlot * Month,
         data = OC)
print(summary(M1), digits = 2)
drop1(M1, test = "F")


E1 <- rstandard(M1) # Extract standardised residuals ####
p1r <- ggplot(data=OC, aes(x=FeedingType, y=E1)) +
  geom_boxplot() +
  geom_hline(aes(yintercept=0), linetype="dashed") +
  theme_classic()
p1r


MyData <- expand.grid(
  FeedingType = levels(OC$FeedingType),
  FeedingPlot = levels(OC$FeedingPlot),
  Month       = levels(OC$Month))
MyData


#Do the actual prediction ####
P1 <- predict(M1, newdata = MyData, se = TRUE)                      

#Add the predicted values
MyData$Fit    <- P1$fit
MyData$SE     <- P1$se.fit
MyData$se.low <- P1$fit - 1.96 * P1$se.fit
MyData$se.up  <- P1$fit + 1.96 * P1$se.fit
print(MyData, digits = 3)

library(magrittr) # pipe operator %>%
library(dplyr)    # mutate function
MyData %>%
  mutate(treatment = paste(FeedingType, FeedingPlot, Month)) %>% 
  ggplot(aes(x = treatment, y=Fit)) +
  geom_pointrange(aes(ymin=se.low, ymax=se.up)) +
  labs(x="", y="Fitted Values") +
  coord_flip() +
  theme_classic()


# Set out basic structure of plot ####
p <- ggplot()
p <- p + xlab("Feeding type") + ylab("Shell length")
p <- p + theme(text = element_text(size=15)) + theme_bw()

# Add points for the fitted values
p <- p + geom_point(data = MyData, 
                    aes(x = FeedingType, 
                        y = MyData$Fit, 
                        size = 6),    
                    col = ("black"))

# Add error bars
p <- p + geom_errorbar(data = MyData,
                       aes(x = FeedingType, 
                           ymax = se.up, 
                           ymin = se.low), 
                       width=0.2)

# Add observations, with random jitter. See what happens when you alter jitter width
p <- p + geom_point(data = OC, 
                    aes(x = FeedingType, y = ShellLength),
                    position = position_jitter(width = .02), #
                    color = grey(0.3),
                    size = 2)

# Change to a grid layout for each combination of factor levels
p <- p + facet_grid(Month ~ FeedingPlot,  # defines vertical ~ horizontal
                    scales = "fixed")     # same scaling in every plot

# Get rid of the legend which isn't doing anything
p <- p + theme(legend.position="none") 

p

# Three-way plot
table(OC$Month, OC$FeedingPlot, OC$FeedingType)
