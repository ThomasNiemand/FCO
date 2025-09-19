## R CMD check results

There were no ERRORs or WARNINGs. 

There are two NOTEs:

Found the following (possibly) invalid URLs:
   URL: https://github.com/ThomasNiemand/FCO.git (moved to https://github.com/ThomasNiemand/FCO)
     From: DESCRIPTION
     Status: 301
     Message: Moved Permanently
 For content that is 'Moved Permanently', please change http to https,
 add trailing slashes, or replace the old by the new URL.
 
Response:  We did so. 
  
Check: examples, Result: NOTE
 Examples with CPU (user + system) or elapsed time > 10s
          user system elapsed
 gen_fit2 8.93   0.15   17.73

Flavor: r-devel-linux-x86_64-debian-gcc
Check: examples, Result: NOTE
 Examples with CPU (user + system) or elapsed time > 5s
           user system elapsed
 gen_fit2 6.167  0.237  12.753
 flex_co2 2.812  0.309   6.324
 
Response: The function need a while due to an external function from another package PoisBinOrdNor::intermat
that simply takes a while. We could use donttest if you suggest so. 

Maintainer: ‘Thomas Niemand <thomas.niemand@gmail.com>’

Resubmission

New version integrated a lot of new features. 
  
Response: Please consider that we skip 1.x for the reason that the new version number fits with the intend of the new functions (e.g., gen_fit2, flex_co2, plot_fit2) who incorporate correct and misspecified models (2) instead of correct models (1) only.

* This is a package update from FCO 0.8.1 to 2.0.0
