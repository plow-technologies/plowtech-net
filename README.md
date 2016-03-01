plowtech-net
============
Company website for Plow Technologies. 


# Document Size and color specifications

## Images
* All product page images should be sized at **1024 x 683**

# Deployment

## To Mockup
Pushing to master will automatically update mockup.plowtech.net

## To Production
Pushing to production will automatically push to production
However, you need to add any new products into the [production-ready-products.txt](/production-ready-products.txt) file. 
for them to make the leap to production

# Development

If you are working on the templates and want to view the site locally, things are set up to allow this without over-writing the copy deployed to master.
This is true even when you are on the master branch.  However, pushing up from local **will over write master** .

To view locally run ```runhaskell Build.hs watch```.

This will build and launch the static site on port 8000.

To view go to [localhost:8000](localhost:8000).
