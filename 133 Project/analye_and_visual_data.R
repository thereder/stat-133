#input data source
infile=read.csv("C:/users/Chi/gas_price_data.csv")

#reassign data to new variables in order to use easier
df=data.frame(infile)
print(df)
cities=df[['cities']]
income=df[['income']]
population_density=df[['population.density']]
automobile_density=df[['automobile.density']]
cost_of_living_index=df[['Cost.of.Living.index']]
gas_price=df[['gas.price']]

#plot scater relationship between gas price and each other facors.
plot(cities,gas_price,'Red',col=1)
plot(income,gas_price,col=2)
plot(population_density,gas_price, col=3)
plot(automobile_density,gas_price, col=4)
plot(cost_of_living_index,gas_price, col=5)

#compute linear coefients with formula gas_price=??+??1(income+??2(population_density)
#+??3(automobile_density)+??4(cost_of_living_index)
coef = lm(gas_price ~ income+population_density+automobile_density+cost_of_living_index)
print(coef)
stats=summary(coef)
print(stats)
gas_stand=((gas_price-mean(gas_price))/sd(gas_price))
cost_stand=(cost_of_living_index-mean(cost_of_living_index))/sd(cost_of_living_index)
plot(cost_stand,gas_stand)
abline(cost_stand,gas_stand)
income_stand=((income-mean(income))/sd(income))
plot(income_stand,gas_stand)
abline(income_stand,gas_stand)
pop_stand=((population_density-mean(population_density))/sd(population_density))
plot(pop_stand,gas_stand)
abline(pop_stand,gas_stand)
auto_stand=((automobile_density-mean(automobile_density))/sd(automobile_density))
plot(auto_stand,gas_stand)
abline(auto_stand,gas_stand)

boxplot(gas_price)
boxplot(income)
boxplot(population_density)
boxplot(automobile_density)
boxplot(cost_of_living_index)

model = lm(gas_price ~ income+population_density)
plot(income, gas_price, col='RED')
points(income, fitted(model), col='Orange')
segments(income, fitted(model), income, gas_pr ice)

df.hist(bin=20)