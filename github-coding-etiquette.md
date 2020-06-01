# Coding Etiquette
## Object names should be concise and meaningful.

Calling your data data might cause problems if you are doing multiple analyses at once / don’t clean your environment, and you keep using the same object name. But if you need an overwrittable universal object and you don’t need to keep lots of objects from each step of your analysis, sticking with the same object name might be useful.
Long object names are annoying to type - more letters, higher chance you’ll make a typo.
Variable and function names should be lowercase.
Variable names should be nouns and function names should be verbs.
Use an underscore to separate words within a file.
Use a dot to separate words within objects and functions.
This way it’s clear what’s an object and what’s an external file.

The preferred form for variable names is all lower case letters and words separated with dots (variable.name). Function names have lowercase letters and words are separated with dots (function.name).

Spacing around infix operators (=, +, -, <-) - should there be spaces?

Line length - how long should a line of code be?

The official convention is to limit your code to 80 characters per line. Having to continuously scroll left and write can be annoying and confusing. Also, when you publish your code on Github, the scrolly bar is all the way down, so to scroll right, you first need to scroll all the way down, scroll right, then scroll all the way up to wherever you want to be - unnecessary.

How do you know what’s 80 characters though? RStudio can place a handy line in your editor as a reminder! Go to Tools/Global Options/Code/Display/Show Margin/80 characters. Sometimes it might make more sense for your code to be a bit longer than 80 characters, but in general code is easier to read if there is no need for continuous scrolling left and right.

When using pipes, keep the piping operator %>% at the end of the line and continue your pipe on a new line.

When using ggplot2, keep the + at the end of the line and continue adding on layers on a new line.

Here is a before and after of a ggplot2 figure code:

Also, remember that putting your entire ggplot code in brackets () creates the graph and then shows it in the plot viewer. If you don’t have the brackets, you’ve only created the object, but haven’t visualized it. You would then have to call the object such that it will be displayed by just typing plot after you’ve created the “plot” object.

## Code chunks
If your code is many, many lines (as it usually is!), it would be easier for you and everyone who might need to read and use it, to split it into different sections. To do that, add in four or more dashes after your comments - you will see a little arrow appear next to that line of code and you will be able to collapse and expand the code chunk based on your needs.

Organise your chunks in a logical order, so that your code flows nicely.

## Commenting guidelines
As an example of a lab’s coding etiquette, you can check out [link](https://github.com/ourcodingclub/TeamShrub-practice/blob/master/TeamShrub_Coding_Etiquette.Rmd).

For more details you can check out our Coding Etiquette tutorial.
