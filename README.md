# ShinyWebApp
Cryptocurrencies vs. Stocks - An analysis in R

Comparing cryptocurrency trends against US Stocks using R and the Shiny framework

# Running the Application
If you use RStudio as your IDE, you can simply open the `492Project.Rproj` and run the ShinyWebApp from within the IDE. Otherwise, if you run your R programs through another process you will need to navigate to the end of the `app.R` file and uncomment the last three lines. The last line starts the actual application, the line before that needs to be updated with your host machine IP, and the line before that just represents the port it will run on. 

**Note you only need to have the `runApp(app)` method uncommented to run it on your computer, but leaving the other two lines uncommented and updated with your IP will allow other individuals to connect to your application.**

# GitHub
You can find all of the source code and updates on [GitHub](https://github.com/RobertMcCoy/ShinyWebApp).

# Kaggle Data Sources
* [Price Volume Data for All US Stocks & ETFs by Boris Marjanovic](https://www.kaggle.com/borismarjanovic/price-volume-data-for-all-us-stocks-etfs)
* [All Crypto Currencies by Jesse Vent](https://www.kaggle.com/jessevent/all-crypto-currencies)