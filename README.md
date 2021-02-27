# sea_cliff_erosion_machine_learning_analysis
Machine learning analysis performed on GlobR2C2 sea cliff erosion database [https://esurf.copernicus.org/articles/6/651/2018/esurf-6-651-2018.html]
Those codes where created during my PhD in Géoscience Environnement Toulouse lab. Analysis were performed on R.

The goal was to understand why cliff erodes at global scale.
The first step was to create a database with data comming from litterature and public database. It describe cliff erosion rate [m/yr] and several parameters supposed to influence erosion (wave mean height, storm occurence, type of rock, meanannual rainfalls ...)

Then came the data analysis, oh yeah!

First I conduct an exploratory analysis, you can find the main results on my article.

Then, I applied a random forest analysis. It wasn't to predict an erosion rate but because random forest allows to see variable importance thanks to gini index. 
Here is the plot of decreasing Gini index (sorry it's in French!). My trees are trained to predict if the erosion rate will be less 10 cm/yr or speeder than this threshold.

![Var_imp_rftune](https://user-images.githubusercontent.com/33718108/109390371-8a804880-7911-11eb-8afa-ac2ee9eaba08.png)



