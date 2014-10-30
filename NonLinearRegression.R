##https://seriousstats.wordpress.com/tag/splines/
library(ggplot2)
library(GGally)

data(state)
View(state.x77)
st = as.data.frame(state.x77)
#lets rename the column without space
colnames(st)[4] = "Life.Exp"         # no spaces in variable names, please
colnames(st)[6] = "HS.Grad"

#already derive why to avoid other param or to find non performer in the model
model = lm(Life.Exp ~ Murder + HS.Grad + Frost , data=st)




qplot(Life.Exp, Murder +
        HS.Grad + Frost, data = model, geom = c("point", "smooth"),
      method = "lm")

#Even lm plotted a straight line above but when we want more data drive plot the very good
#choice will be local regression approach such as loess. It actually fit the data using a complex
#curve so naturally we can expect more better prediction
qplot(Life.Exp, Murder +
        HS.Grad + Frost, data = model, geom = c("point", "smooth"),
      method = "loess")

#Spline is an alternative of loess that fits sections of simpler curves together
#but increasing the degree in the formula can lead to OVERFITTING error
qplot(Life.Exp, Murder +
        HS.Grad + Frost, data = model, geom = c("point", "smooth"),
      method = "lm",formula = y ~ ns(x, 3))

#OVERFITTING EXAMPLE
qplot(Life.Exp, Murder +
        HS.Grad + Frost, data = model, geom = c("point", "smooth"),
      method = "lm",formula = y ~ ns(x, 15))


qplot(Life.Exp, Murder +
        HS.Grad + Frost, data = model, geom = c("point", "smooth"),
      method = "rlm")