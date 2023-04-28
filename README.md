# Spotify Song Recommendation System
By Aashish Anand, Phi Nguyen, Sebastian Santos-Mendoza, Cian Murray\
<br />
This is a Shiny dashboard that not only performs k-means clustering for song recommendation, but also exploratory data analysis for each audio feature. There are two tabs in the dashboard: Recommend and Explore.

In the "Recommend" tab, the user inputs a Spotify playlist URI and then clicks the "Get Recommendation" button to start the k-means song recommendation. Once complete, a dataset is displayed on the dashboard where 5 songs within a certain distance from the same cluster are shown.

In the "Explore" tab, the user gets the opportunity to explore the dataset consisting of 10,000+ Spotify songs. Those songs are all plotted on a scatterplot where the axes are autoscaled. There are many filters that the user can interact with such as the text inputs for the artist and song names, and sliders for adjusting the ranges of acousticness, danceability, energy, liveness, speechiness, tempo, and valence. There are even dropdown menus to change the x and y variables for the graph. All of these widgets update the graph such that the points meet the criteria from the filters.
