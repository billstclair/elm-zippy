In 1985 or thereabouts, I wrote a little program for the Symbolics Lisp Machine that sent Zippy the Pinheads careening across the screen, reflecting off of top and sides, and always facing the left/right direction they were traveling.

This project reproduces that, in a browser, with Elm, with check boxes for the images you want to use, and a fill-in for how many are active at once.

The project is live at [gibgoygames.com/zippy](https://gibgoygames.com/zippy/).

For development:

    cd .../elm-zippy
    elm reactor
    
Then aim your web browser at http://localhost:8000/site/index.html

To convert the elm code to `site/elm.js`:

    cd .../elm-zippy
    bin/build
    
Then refresh the browser pointing at Elm reactor (command-shift-R on my Mac).

To update the live web site (assuming `site/.sshdir` points to it and you have my `rsyncit` script [installed](https://github.com/billstclair/wws-scripts)):

    cd .../elm-zippy
    bin/update-site
    
