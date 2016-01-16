# VolcanoSRI
Here are few sample codes that was developed for VolcanoSRI Project. You can find codes mainly written in C/C++, Matlab, R and Python.

C/C++ Code - This part contains the code that run on the real system such as Beaglebone/Raspberry Pi.In our system design BeagleBone is connected to a seismic sensor (Geophone) which continously samples seismic vibration. There is a C code which analyses the signal and detects an event such as earthquake. A MLE based algorithms is used to detect the anomaly.

Using this data we further process it to obtain seismic image. This has to run on beaglebone in a decentralized fashion. An asynchronous decentralized gradient descent method has been implemented which talks to different beaglebone in order to solve the least squares problem. Refer to my publication for better understanding.

R Code: Initially when we started our project we choose R to prototype the code. R was choosen as seismic community had several package to analyse the signals. This particularly has codes to generate image from real data set of Mt St Helens. TomoRunReal.R is the driver program and if this is executed you will obtain the final image.

Matlab Code:
