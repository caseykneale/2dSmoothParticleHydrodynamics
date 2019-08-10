![alt text](https://github.com/EntropyAndAmbiguity/2DSPH-UI/blob/master/Screenshot%20from%202018-05-14%2010-05-39.png)

# 2DSPH-UI
This is a 2D smoothed particle hydrodynamics simulator. The core of the simulation is based on "Particle-based Viscoelastic Fluid Simulation" by S. Clavet, P. Beaudoin, and P. Poulin. This is written in scala by a hobbyist (me) in their free-time, so it is not dumby proof. IE: You can enter strings into text boxes which pertain to numerical values and get errors. If this perturbs you, you can fix it! Commenting is pretty sparse, and over-all this is pretty hacky. I'm only sharing the code because the application is pretty cool; the code itself really isn't something that should be learned from. The code is not clean - at all.

This was my first ever scala project. I learned a lot by doing this, but if time and external interest permits I can and will clean this up. I didn't plan almost any of this out prior to writing it, and I am sure the code-base shows this. Ie: I shouldnt have mapped the text boxes to label text in the UI for updating the simulation parameters (yuck), but I wanted to play more with the scala data structures v.s. my admittedly weak JAVA background. A lot of this is tinkering and solving pretty generic programming problems.

Features:
1. Allows a wide range of 'physical' and simulation parameters to be tinkered with for viewing the simulation.
2. Settings/parameters can be saved and loaded.
3. Simulations can be logged in a few ways. The particles can be randomly sampled (useful for training a neural network because this will reduce filesize/training time), nearest neighbor locations can be stored, or the whole simulation can be saved. When the simulation is saved the current position at each frame is stored (with or without nearest neighbor positions) in the _X.csv file, while the subsequent position after the simulation has done it's calculations is stored in the _Y.csv file. This should be pretty close to a ready made implementation for machine learning modelling.
4. Stored simulations can be viewed in 'real-time'. Load the settings file for the simulation and the simulation _Y.csv file then press start in the viewer frame. Pretty cool.
5. The simulations aren't real-time but can be interacted with by a horizontal piston force. This can be triggered by left clicking on the simulation frame (anywhere). To edit the piston force settings you can go to Simulation -> External Force via the main menu.

Known Breaks:
1. Altering the volume parameter or number of particles does not allow for true to form spacing in fluid initialization. Meaning, it can over-flow particles if the settings are not with-in an appropriate bound. I could fix this, but, I don't have a need to do that.

2. Again, changing the volume is not guaranteed for playback. This could also be fixed, but I don't have a need for it.

3. If you see in the console a printout about how a particle was lost, this is problematic. It means a particle escaped the quadtree type partitioning (oops). Try changing the settings to something more balanced, whatever that means, SPH simulations are somewhat notorious for being unstable.

Note: I know 'volume' is not 2-D, it's just vernacular. You'll also notice in the code that my Point2D class has vector operations *gasp*.


The main class is "UIWorkbench".
