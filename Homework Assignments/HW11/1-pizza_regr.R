# Prepare pizza dataset
students = c(2, 6, 8, 8, 12, 16, 20, 20, 22, 26)
sales = c(58, 105, 88, 118, 117, 137, 157, 169, 149, 202)
pizza = data.frame(students, sales)
write.csv(pizza, "pizza.csv", row.names = FALSE, quote = FALSE)

# Read pizza data
pizza = read.csv("pizza.csv", header = TRUE)

# Variances
pizza
round( var(pizza), 2)
round( cor(pizza), 2)

# Regression coefficient estimates
summary(lm(sales ~ students, data=pizza))

# Standardized Variances
pizza_std <- data.frame(scale(pizza))
round( var(pizza_std), 2)
round( cor(pizza_std), 2)

# Standardized coefficient estimates
summary(lm(sales ~ students, data=pizza_std))

# Regression Model
pizza_regr <- lm(sales ~ students, data=pizza)
pizza_regr
summary(pizza_regr)

b1 <- cov(pizza$students, pizza$sales) / var(pizza$students); b1
b0 <- mean(pizza$sales) - b1*mean(pizza$students); b0


# Visualization
plot(pizza, 
     xlim=c(0, max(pizza$students)), ylim=c(0, max(pizza$sales)),
     pch=19, cex=1.5, col="gray")
abline(h=mean(pizza$sales), v=mean(pizza$students), col="lightgray")
abline(pizza_regr, lwd=2, col="cornflowerblue")

# Standardized Model and Visualization
pizza_regr_std <- lm(sales ~ students, data=pizza_std)
summary(pizza_regr_std)
plot(pizza_std, 
     pch=19, cex=1.5, col="gray")
abline(h=0, v=0, col="lightgray")
abline(pizza_regr_std, lwd=2, col="cornflowerblue")
