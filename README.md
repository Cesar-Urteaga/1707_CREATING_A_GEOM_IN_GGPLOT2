##  Creating a new ggplot2 geom

This project illustrates **how to create a new ggplot2 geom in R** that allows to visualize hurricane wind fields using the data from [The Tropical Cyclone Extended Best Track Dataset](http://rammb.cira.colostate.edu/research/tropical_cyclones/tc_extended_best_track_dataset/).  The motivation of this undertaking was to solve the peer-graded assignment of the [Building Data Visualization Tools](https://www.coursera.org/learn/r-data-visualization) course.

##  Organization of the Files

The project has the following structure:
  * **_CODES**: Has two files: [ggplot2.R](./_CODES/ggplot2.R) and [submitted\_geom\_hurricane.R](./_CODES/submitted_geom_hurricane.R).  The first one creates two visualizations of the Hurricane Ike and the latter was the submitted code for the peer-graded assignment of the Coursera's course; moreover, this contains roxygen2-style documentation to exemplify how to integrate the new geom into a new R package.
  * **_GRAPHS**: Contains the plots created by [ggplot2.R](./_CODES/ggplot2.R) code in order to demonstrate how the new geom works: [Ike\_path.png](./_GRAPHS/Ike_path.png) and [submitted\_image.png](./_GRAPHS/submitted_image.png).
  * **_RESOURCES**: Includes all the resources that I used during the creation process of the new geom.
  * **_TEXTS**: Consists of all my personal notes that I considered relevant for this task.

---

##  Sample images using the new geom

As stated above, the two images created by the geom are the following:

### Whole path of the Hurricane Ike at 18:00 UTC during each observed day.

![Path of the Hurrican Ike](./_GRAPHS/Ike_path.png?raw=true)

### Hurricane Ike at latitude 25.8

![Path of the Hurrican Ike](./_GRAPHS/submitted_image.png?raw=true)
