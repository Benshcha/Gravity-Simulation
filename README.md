# Gravity-Simulation
To run the simulation, place the entire project folder inside TASM\BIN and execute GRAVIT~1\startup.bat

# Changing Planet Formation
inorder to change the planets formation you should head to the start location and change the arrange these code lines:

```
push xPosition <br>
push yPosition  <br>
push xVelocity  <br>
push yVelocity  <br>
push mass <br>
call addPlanet 
```

**There should be up to _5_ segments of code like the above! If you wish to change existing problems you should change the xPosition, yPosition... etc of the planet which you wish to change.**



Gal Ben-Shach 10.6.2020
